####################
####################
### Function: compute_interdependent_BC
### Inputs:   ipp,bp,bonus_add,bonus_mult
### Author:   Ian Durbach (indurbach@gmail.com)
### Update:   5/2/2016
###
### from bp and cp, compute benefits and costs of implementing interaction k 
### (i.e. ALL the projects in int k). Given by Bi and Ci respectively.
### NOTE: depends on earlier specification of multiplicative or additive
###       benefits. 
####################
####################

compute_interdependent_BC = function(ipp,bp,cp,alpha,gamma,beta,phi){
  
  n_int_proj = c()
  
  for(i in length(ipp):1)
  {
    #n_int_proj[i] = ncol(ipp[[i]])
    n_int_proj[i] = 1
  }
  
  # Benefits
  
  k = 1
  Bi = c()
  for(i in 1:length(n_int_proj))
  {
    #for(j in 1:n_int_proj[i])
    #{
      # multiplicative benefit multiplies by the sum of all projects project 
      # benefits
      #size_synergy =  sum(bp[ipp[[i]][,j]])
      size_synergy =  sum(bp[ipp[[i]]]) 
      # compute benefit
      #Bi[k] = alpha[k] + (gamma[k] / length(bp[ipp[[i]][,j]]) * size_synergy)
      Bi[i] = alpha[i] + (gamma[i] / length(bp[ipp[[i]]]) * size_synergy)
      k = k + 1
    #}
  }
  
  # Costs
  
  k = 1
  Ci = c()
  for(i in 1:length(n_int_proj))
  {
    # for(j in 1:n_int_proj[i])
    # {
      # multiplicative costs multiplies by the mean of the individual project 
      # costs
      #size_synergy = mean(cp[ipp[[i]][,j]]) 
      size_synergy = mean(cp[ipp[[i]]]) 
      # compute benefit
      Ci[i] = beta[i] + (phi[i] * size_synergy)
      k = k + 1
    # }
  }
  
  output = list(Bi = Bi, Ci = Ci)
  return(output)
  
}