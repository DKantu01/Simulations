####################
####################
### Function: construct_random_portfolios
### Inputs:   nRP,nCV,ipp,bp,Bi,cp,Ci,budget
### Author:   Ian Durbach (indurbach@gmail.com)
### Update:   5/2/2016
###
### Generates a number of random feasible portfolios. Done by randomly adding
### alternatives until nCV budget constraint violations are recorded, or all
### alternatives have been tried (see paper for details).
####################
####################

construct_random_portfolios = function(nRP,nCV,ipp,bp,Bi,cp,Ci,budget){
  
  n = length(bp)
  K = length(Bi)
  
  final_z = matrix(0,nrow=nRP,ncol=n)
  benefit = c() 
  cost = c() 
  feasible = c()
  g = matrix(0,nrow=nRP,ncol=K)
  
  for(k in 1:nRP){
    
    # generate ordered list of alternatives for consideration in portfolio
    alts = sample(1:n,n)
    
    # generate random portfolio
    i = 1
    cv = 0 # index for consecutive constraint violations (terminate when = 3)
    
    # start with empty portfolio
    my_z = rep(0,n)
    
    while((cv < nCV)&(i < n)){
      
      # proposed addition to portfolio
      new_alt = alts[i]
      
      # proposed portfolio
      my_z[new_alt] = 1
      
      # evaluate proposed portfolio
      my_propsol = evaluate_z(z = my_z, ipp = ipp, bp  = bp, Bi = Bi, 
                              cp = cp, Ci = Ci, budget = budget)  
      
      # if feasible, accept; if not feasible, reject
      if(my_propsol$feasible == 1){
        my_z[new_alt] = 1 # accept proposed alternative in portfolio
        cv = 0   # reset constraint violation counter to 0
      }else{
        my_z[new_alt] = 0  # reject proposed alternative in portfolio
        cv = cv + 1    # increment constrain violation counter
      }
      
      # move on to next alternative
      i = i + 1
      
    }
    
    final_z[k,] = my_z
    # evaluate last portfolio
    my_propsol = evaluate_z(z = my_z, ipp = ipp, bp  = bp, Bi = Bi, 
                            cp = cp, Ci = Ci, budget = budget, decompose = T)  
    benefit[k] = my_propsol$benefit 
    cost[k] = my_propsol$cost 
    feasible[k] = my_propsol$feasible
    g[k,] = my_propsol$g 
    
  }
  
  return(list(final_z=final_z,benefit=benefit,cost=cost,feasible=feasible,g=g, benefit_bare = my_propsol$benefit_bare))
  
}
  
  