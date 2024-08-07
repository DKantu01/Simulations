########################
# first cue is whether portfolio is in a positive interaction
# second cue is cost-value ratio
########################
construct_lex_portfolio <- function(nCV, ipp, bp, Bi, cp, Ci, budget){
  
  n = length(bp)
  
  # generate LEX portfolio
  i = 1
  add_order = 1
  cv = 0 # index for consecutive constraint violations (terminate when = 3)
  
  # start with empty portfolio
  my_z = rep(0,n)
  order_my_z = rep(0,n)
  proj <- as.data.frame(cbind(idx = seq(1,n),bp,-cp))
  
  posinteractions = ifelse(positiveInteractions(proj, ipp, Bi) > 0, -1, 0)
  # order alternatives by cost/benefit ratio and whether there are positive interactions
  cb_ratio = cp/bp 

  random_order = sample(1:length(bp))
  #alts = sort.int(-cb_ratio, random_order,index.return = T)$ix
  alts = order(posinteractions, cb_ratio, random_order)
  
  while((cv < nCV)&(i < n)){
    
    # proposed addition to portfolio
    new_alt = alts[i]
    
    # proposed portfolio
    my_z[new_alt] = 1
    
    # evaluate proposed portfolio
    my_propsol = evaluate_z1(z = my_z, ipp = ipp, bp  = bp, Bi = Bi, 
                            cp = cp, Ci = Ci, budget = budget)  
    
    # if feasible, accept; if not feasible, reject
    if(my_propsol$feasible == 1){
      my_z[new_alt] = 1 # accept proposed alternative in portfolio
      order_my_z[new_alt] = add_order
      add_order = add_order + 1
      cv = 0   # reset constraint violation counter to 0
    }else{
      my_z[new_alt] = 0  # reject proposed alternative in portfolio
      cv = cv + 1    # increment constrain violation counter
    }
    
    # move on to next alternative
    i = i + 1
  }
  
  final_z = my_z
  
  final_res = evaluate_z1(z = my_z, ipp = ipp, bp  = bp, Bi = Bi, 
                         cp = cp, Ci = Ci, budget = budget, decompose = T) 
  
  benefit = final_res$benefit 
  cost = final_res$cost 
  feasible = final_res$feasible
  g = final_res$g 

  return(list(final_z=final_z,benefit=benefit,cost=cost,feasible=feasible,g=g, benefit_bare = final_res$benefit_bare, order_z = order_my_z))
}

########################
# first cue is whether portfolio is in a positive interaction
# second cue is whether portfolio is in a positive interaction with a project already in the portfolio
# third cue is benefit cost ratio
########################
construct_lex_portfolio_3cues <- function(nCV, ipp, nor_bp, bp, Bi, nor_cp,cp, Ci, budget, binary_num_interactions = T){
  
  n = length(bp)
  
  # generate LEX portfolio
  i = 1
  cv = 0 # index for consecutive constraint violations (terminate when = 3)
  
  # start with empty portfolio
  my_z = rep(0,n)
  order_my_z = rep(0,n)
  proj <- as.data.frame(cbind(idx = seq(1,n),bp,-cp))
  
  posinteractions = ifelse(positiveInteractions(proj, ipp, Bi) > 0, -1, 0)
  posinteractions_with_portfolio = numInteractionsWithExistingPortfolio(ipp, my_z, binary = binary_num_interactions)
  # order alternatives
  cb_ratio = cp/bp 
  cb_ratio = round(cb_ratio ,-1)
  
  random_order = sample(1:length(bp))
  #alts = sort.int(-cb_ratio, random_order,index.return = T)$ix
  alts = order(posinteractions, posinteractions_with_portfolio, cb_ratio, random_order)
  already_considered = c()
  while((cv < nCV)&(i < n)){
    # proposed addition to portfolio
    new_alt = alts[i]
    if(my_z[new_alt] == 1 | new_alt %in% already_considered){#already in portfolio? move ahead without changing cv
      i = i + 1 #move on to next alternative
    }else{
      # proposed portfolio
      my_z[new_alt] = 1
      
      # evaluate proposed portfolio
      my_propsol = evaluate_z(z = my_z, ipp = ipp, bp  = bp, Bi = Bi, 
                              cp = cp, Ci = Ci, budget = budget)  
      
      # if feasible, accept; if not feasible, reject
      if(my_propsol$feasible == 1){
        my_z[new_alt] = 1 #accept proposed alternative in portfolio
        order_my_z[new_alt] = i
        cv = 0   #reset constraint violation counter to 0
        posinteractions_with_portfolio = numInteractionsWithExistingPortfolio(ipp, my_z, binary = binary_num_interactions)
        alts = order(posinteractions, posinteractions_with_portfolio, cb_ratio, random_order)
        i = 1 #start counter again since alts has changed
      }else{
        my_z[new_alt] = 0  #reject proposed alternative in portfolio
        already_considered = c(already_considered, new_alt)
        cv = cv + 1    #increment constrain violation counter
        i = i + 1 #move on to next alternative
      }
    }
  }
  
  final_z = my_z
  
  final_res = evaluate_z(z = my_z, ipp = ipp, bp  = bp, Bi = Bi, 
                         cp = cp, Ci = Ci, budget = budget, decompose = T) 
  
  benefit = final_res$benefit 
  cost = final_res$cost 
  feasible = final_res$feasible
  g = final_res$g 

  return(list(final_z=final_z,benefit=benefit,cost=cost,feasible=feasible,g=g, benefit_bare = final_res$benefit_bare, order_z = order_my_z))
}

rounding_cb_cp_effect = function(){
  notrounded = c()
  rounded = c()
  for(prefix in c("data/uniform_data_", "data/pos_skew_data_", "data/neg_skew_data_")){
    for(i in 1:100){
      filepath = paste0(prefix, i, ".csv")
      u1 = read.csv(filepath)
      notrounded = c(notrounded, length(table(u1$cost / u1$value)))
      rounded = c(rounded, length(table(round( u1$cost / u1$value, -1))))
    }
  }
  rounded
  notrounded
}