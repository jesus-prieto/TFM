# Probabilistic Modelling of MissingAttributes in Bipartite Networks

## Scripts
* **sampler_gen.R:** En el siguiente script se encuentra escrita la función necesaria para samplear atributos categoricos de una red bipartita. Como entradas, toma:

    * **data**: Los datos del nodo de interés y su consiguiente atributo (e.g articulos y paises)
    * **atributos_posibles**: El numero de posibles valores que puede tomar el atributo en cuestión
    * **n_samples**: El numero de muestras que se desea extraer, si no hay nas en data. Por defecto NULL
    * **undersampling**: Si se desea considerar una submuestra de los datos como el total de los datos observados. Por defecto FALSE
    * **perc**: El tamaño de la submuestra. Por defecto 0.1
    * **alpha**: El valor de alpha que se desea utilizar para samplear. Por defecto NA, se calcula internamente con el algoritmo propuesto en el trabajo.
    * **alpha_in**: El valor que empieza tomando alpha al ejecutarse el algoritmo. Por defecto 1.
    
* **sampler_cpp_3.R:** Forma parte de sampler_gen.R. Corresponde con la aplicación del algoritmo de metropolis. Como entradas toma:
    * **P**: La matriz P, de la cadena de Markov
    * **n_autores**: Vector con el numero de elementos de la observacion a samplear
    * **keys**: Valor unico asociado a cada observacion
    * **s_path**: Matriz de caminos más cortos para los atributos
    * **P_in**: Distribución del primer elemento de cada observacion
    * **alpha**: Valor de alpha, por defecto 1.5
    * **progress**: Boleano que indica si se debe mostrar una barra de progreso. Por defecto true

* **from_data_to_graph.R:** Función empleada para transformar los datos devueltos por **sampler_gen.R:** en el grafo, fruto de projectar el grafo bipartito sobre los atributos, en un tiempo razonable. Como entradas toma:
    * **data**: Datos devueltos por **sampler_gen.R:**
    * **keyword**: Expresión lógica necesaria para identificar los nodos de los atributos en data. Por defecto "[1-9]|WOS"
    
** Librerias necesarias
* **dplyr** >= ‘1.0.6’
* **multicool** >= ‘0.1.11’
* **Rfast** >= ‘2.0.1’
* **Rcpp** >= ‘1.0.2’
* **igraph** >= ‘1.2.4.2’
* **stringr** >= ‘1.4.0’
* **RcppProgress** >= ‘0.4.2’

    