####################
####################
### Function: evaluate_z
### Inputs:   z,ipp,bp,Bi,cp,Ci,budget
### Author:   Ian Durbach (indurbach@gmail.com)
### Update:   2/2/2016
###
### Evaluates benefits and costs associated with any vector of
### decision variables, and tells if feasible given budget constraint
####################
####################

evaluate_z = function(z,ipp,bp,Bi,cp,Ci,budget, decompose = F)
{
  n_int_proj = c()
  
  for(i in length(ipp):1)
  {
    n_int_proj[i] = ncol(ipp[[i]])
  }
  
  k = 1
  g = c()
  for(i in 1:length(n_int_proj))
  {
    for(j in 1:n_int_proj[i])
    {
      g[k] = prod(z[ipp[[i]][,j]])
      k = k + 1
    }
  }
  benefit = sum(c(z,g) * c(bp,Bi))
  cost = sum(c(z,g) * c(cp,Ci))
  feasible = ifelse(cost<=budget,1,0)
  benefit_bare = 0
  benefit_interactions = 0
  if(decompose){
    benefit_bare = sum(z * bp)
    benefit_interactions = sum(g * Bi)
  }
  return(list(benefit=benefit,cost=cost,feasible=feasible,g=g, benefit_bare = benefit_bare, benefit_interactions = benefit_interactions))
}
