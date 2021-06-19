############################################################################
########################### MARKOV SAMPLER #################################
############################################################################

library(dplyr)
options(dplyr.summarise.inform = FALSE)
Rcpp::sourceCpp("sampler_cpp_3.cpp")

sampler <- function(data, atributos_posibles, n_samples=NULL, undersampling = FALSE, perc = 0.1, alpha=NA, alpha_in=1){
  
  #---------------------------------------------------------------------------------------------------
  #--------------CALCULO EL NUMERO DE ARTICULOS QUE NECESITO SAMPLEAR EN BASE AL TAMAÑO DE MI MUESTRA (DATA) -----------------
  colnames(data) <- c("ut", "country")
  data_raw <- data
  n_aut_per_art <- (data %>% group_by(ut) %>% summarise(n=length(ut)))[,"n"][[1]]
  
  n_tot_articles <- length(n_aut_per_art) 
  n_sam_articles <- length(unique(data$ut))
  
  data <- data[!(data$ut %in% unique(data$ut[is.na(data$country)])),]
  n_samples <- n_tot_articles - n_sam_articles
  
  output <- list()
  
  if (undersampling){  #undersampling , perc variables de la funcion
    
    n_sam_articles_2 <- round(perc * n_tot_articles)
    uts <- unique(data_raw$ut)[sample.int(n_sam_articles, n_sam_articles_2)]
    data <- data_raw[data_raw$ut %in% uts,]
    
    n_samples <- n_tot_articles - n_sam_articles_2
    
  }
  
  ############################################################################
  ########################### P PAIS NO VISTO ################################
  ############################################################################
  
  frecs <- table(n_aut_per_art) #para calcular las probabilidades de que el articulo tenga mas de una persona
  country_codes <- data.frame(country=atributos_posibles)
  p_t <- nrow(country_codes) #paises totales
  p_s <- length(unique(data$country)) #numero de paises que han salido
  n_nc <- p_t - p_s #numero de paises nuevos que deberían salir 
  if (n_nc!=0){
    p_r <- p_t - p_s #numero de paises restantes  #n
    
    h <- p_r - n_nc  #k
    p_prev <- -1
    p_act <- 0
    n_in <- n_nc  #p
    while (p_prev < p_act){
      p_prev <- p_act
      p_act <- multicool::Stirling2(n_in, p_r - h)/p_r^n_in
      n_in <- n_in + 1
    }
    n_p <- n_in - 2
    p_1 <- n_p/(n_samples*(1-frecs[1]/sum(frecs)))
  } else {
    p_1 <- 0
  }
  
  ############################################################################
  ########################### DISTRIBUCION INICIAL ###########################
  ############################################################################
  
  P_in.matrix <- function(data){
    y <- function(x) return(length(x)^-1)
    
    P_in <- data %>% group_by(ut) %>% summarise(len=y(country), country=country) %>% 
      group_by(country) %>% summarise(p=sum(len))
    P_in <- P_in %>% mutate(p=p/sum(p))
    P_in <- merge(country_codes, P_in, by="country", all.x=T)
    #P_in[is.na(P_in)] <- 0
    P_in[is.na(P_in)] <- p_1/sum(is.na(P_in))/1.5
    P_in <- P_in %>% mutate(p=p/sum(p))
    return(P_in)
  }
  P_in <- P_in.matrix(data)
  
  ############################################################################
  ######################### MATRIZ DE TRANSICIONES ###########################
  ############################################################################
  
  country_codes$country <- sort(country_codes$country)
  
  P.matrix <- function(data){
    y <- function(x, c){  #elimino el primer registro del pais c para cada artículo
      bool <- x==c
      cs <- x[bool]
      x <- c(cs[-1], unique(x[!bool]))
      return(x)
    } 
    
    P <- data.frame(matrix(0, nrow(country_codes), nrow(country_codes)+1))
    colnames(P) <- c("id", country_codes$country)
    P[, 1] <- country_codes$country
    
    for (i in 1:nrow(country_codes)){
      uts <- unique(data[data$country == country_codes[i, 1], "ut"])
      if (length(uts)==1 && is.na(uts)){
        next
      }
      data_c <- data[data$ut %in% uts,]
      data_c <- data_c %>% group_by(ut) %>% summarise(country=y(country, country_codes[i, 1])) %>% 
        group_by(country) %>% summarise(count=n()) %>% 
        mutate(p = count/sum(count))
      
      data_c <- merge(country_codes, data_c, by="country", all.x=T)
      
      P[, i+1] <- data_c[, "p"]
    }
    P[is.na(P)] <- 0
    
    for (i in 1:nrow(P)){   #para que aquellos paises para los cuales no se tiene informacion tengan una distribución uniforme sobre el resto
      if (sum(P[,i+1])==0){
        #  P[,i+1] <- 1/nrow(P)
        P[,i+1] <- P_in$p
        P[i,i+1] <- max(P_in$p)  #le pongo mas probabilidad de publicar con un autor del mismo pais
        P[,i+1] <- P[,i+1]/sum(P[,i+1])
      }
    }
    return(P)
  }
  P <- P.matrix(data)
  
  ############################################################################
  ############################ CAMINO MAS CORTO F ############################
  ############################################################################
  
  SP <- function(P){
    P.dist <- P[,-1]
    P.dist <- P.dist>0
    P.dist[!P.dist] <- 10000
    s.path<-Rfast::floyd(P.dist)
    s.path<-pmin(s.path, t(s.path))
    return(s.path)
  }
  s.path <- SP(P)
  
  
  ############################################################################
  ############################## COMPUTE ALPHA  ##############################
  ############################################################################
  
  if (is.na(alpha)){
    datos <- data
    n <- length(unique(datos$ut))
    datos$ut <- as.integer(factor(datos$ut))
    p <- seq(0.9, 0.95, by=0.01)
    tams <- c()
    
    for (perc2 in seq(0.9, 0.95, by=0.01)){
      t=0
      for (j in 1:6){
        
        n_sam <- round(perc2 * n)
        uts <- unique(datos$ut)[sample.int(n, n_sam)]
        datos_perc<- datos[datos$ut %in% uts,]
        datos_resto <- datos[!(datos$ut %in% uts),]
        
        
        P.alpha <- P.matrix(datos_perc)
        s.path.alpha <- SP(P.alpha)
        P_in.alpha <- P_in.matrix(datos_perc)
        
        uts_draw <- unique(datos_resto$ut)
        p.counter <- rep(0, length(uts_draw))
        
        for (i in 1:length(uts_draw)){
          countries <- unique(datos_resto$country[datos_resto$ut==uts_draw[i]])
          ids <- which(P.alpha$id %in% countries)
          if (max(s.path.alpha[ids,ids])>1){
            p.counter[i] <- 1
          }
        }
        t <- t+sum(p.counter)/length(p.counter)
      }
      tams <- c(tams, t/10)
    }
    
    vals <- predict(lm(tams~p, data=data.frame(p=p,tams=tams)),data.frame(p=1),interval = "confidence")
    #print(vals)
    lim_sup <- vals[3]
    lim_inf <- vals[2]
    
    n_error <- 10*(n-n_sam)
    n_autores <- sample(n_aut_per_art, size = n_error, replace = T) #numero de autores de la muestra
    keys <- rep(1:n_error, n_autores)
    
    step=1
    flag=T
    thres=4
    counter=0
    alpha=alpha_in
    
    while(flag & counter<thres){
      counter <- counter+1
      
      muestra= sampler_cpp_3(P, n_autores, keys, s.path, P_in$p, alpha=alpha)
      uts_draw <- unique(muestra$ut)
      p.counter <- rep(0, length(uts_draw))
      
      for (i in 1:length(uts_draw)){                                                        # pasarlo a rcpp o hacerl mejor
        countries <- unique(muestra$country[muestra$ut==uts_draw[i]])
        ids <- which(P$id %in% countries)
        if (max(s.path[ids,ids])>1){
          #p.counter[i] <- sum(s.path[ids,ids][lower.tri(s.path[ids,ids])]-1)
          p.counter[i] <- 1
        }
      }
      p <- sum(p.counter)/length(p.counter)
      #print(p)
      #print(alpha)
      if (p>lim_sup) {
        alpha=alpha+step
      }else if (p<lim_inf){
        alpha=alpha-step
      }else {
        flag=F
      }
      step <- step/2
    }
  }
  
  
  ############################################################################
  ################################# SAMPLER  #################################
  ############################################################################
  
  n_autores <- sample(n_aut_per_art, size = n_samples, replace = T) #numero de autores de la muestra
  keys <- rep(1:n_samples, n_autores)
  
  data_total=sampler_cpp_3(P, n_autores, keys, s.path, P_in$p, alpha=alpha)
  
  return(rbind(data, data_total))
}
