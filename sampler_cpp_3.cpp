#include <iostream>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>

// [[Rcpp::export]]
DataFrame sampler_cpp_3(
    DataFrame P, 
    IntegerVector n_autores, 
    IntegerVector keys,
    DataFrame s_path,
    NumericVector P_in,
    double alpha = 1.5,
    bool progress=true)
{
  CharacterVector country_out(keys.size());
  IntegerVector inds_country(keys.size());
  CharacterVector country_in = P[0];
  
  int s=0;
  int ind=0;
  IntegerVector inds=seq_along(country_in); //possible indexes for country
  Progress pb(n_autores.size(), progress);
  bool flag;
  int ind_prev;
  NumericVector v(P_in.size());
  double shortest_path;
  int thres;
  double p_actual=1; // probabilidad propuesta
  double p_anterior=1e-100;
  
  for (int i = 0; i < n_autores.size(); i++) {
    flag=true;
    thres=0;
    //std::cout <<i<< "\n";
    
    
    shortest_path=0;
    thres++;
    ind=sample(inds, 1, false, P_in)[0];
    inds_country[s]=ind-1;
    country_out[s++]=country_in[ind-1];
    
    //std::cout <<country_in[ind-1]<< " country_in \n"; //quitar
    //std::cout <<i<< " i \n"; //quitar
    
    
    if (n_autores[i]>1){
      for (int j=1; j<n_autores[i]; j++){
        ind=sample(inds, 1, false, P[ind])[0];
        inds_country[s]=ind-1;
        country_out[s++]=country_in[ind-1];
      }
      
      IntegerVector indices=seq(s-n_autores[i], s-1);
      IntegerVector prev_inds=inds_country[indices];
      
      ind_prev=prev_inds[0];
      p_actual=P_in[ind_prev];
      
      // for (int j=1; j<prev_inds.size(); j++){  //calcular probabilidad de la propuesta
      //   v=P[ind_prev+1];
      //   //std::cout <<p_actual<< " v \n"; //quitar
      //   ind_prev=prev_inds[j];
      //   p_actual=p_actual*v[ind_prev];
      // }
      
      prev_inds=unique(prev_inds);
      //std::cout <<prev_inds<< " prev_inds \n"; //quitar
      
      for (int j=0; j<prev_inds.size(); j++){  //calcular media de distancias de la cofig propuesta
        ind_prev=prev_inds[j];
        v=s_path[ind_prev];
        
        NumericVector dists=v[prev_inds];
        //std::cout <<dists<< " dists \n"; //quitar
        if (max(dists)>shortest_path){
          shortest_path=max(dists);
        }
        //shortest_path=shortest_path+sum(dists);
      }
      //shortest_path=shortest_path/(pow(prev_inds.size(), 2)-prev_inds.size());
      //std::cout <<shortest_path<< " shortest_path \n"; //quitar
      
      //std::cout <<p_actual<< " p_actual 1 \n"; //quitar
      if (shortest_path==0){
        shortest_path=1;
        }
      p_actual=pow(shortest_path,-alpha);
      //flag = Rcpp::runif(1)[0] > 1/pow(shortest_path,alpha); //anterior flag
      flag=Rcpp::runif(1)[0] > p_actual/p_anterior;
      // std::cout <<p_anterior<< " p_anterior \n";
      // std::cout <<p_actual<< " p_actual \n";
      if (flag){
        //std::cout <<indices<< " indices \n";
        country_out[indices]=country_out[indices-n_autores[i]];
      } else{
        p_anterior=p_actual;
      }
    }
    
    
    pb.increment(); // update progress
  }
  
  return DataFrame::create(_["country"] = country_out, _["ut"] = keys);
}
