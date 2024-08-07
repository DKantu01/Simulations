createInteractions = function(nproj, my_nCV, my_budget, my_alpha, my_selprob = "equal", random_nested = 0, 
                              interaction_pool = 10, my_bp = NULL, my_cp = NULL, my_gamma, my_ipp = NULL, 
                              my_ipp_neg = NULL, my_BC = NULL, my_BC_neg = NULL, order_int_proj = c(5,4,3,2), 
                              n_int_proj = c(2,6,8,10), my_beta = c(0,0,0,0), my_phi = c(0,0,0,0)){

  my_nint = interaction_pool    # size of subset of related projects
  my_order_int_proj = order_int_proj #c(6,5,4,3,2) # order of interdependencies between projects
  my_n_int_proj = n_int_proj #c(1,3,5,6,10) # number of interdependencies of each order
  my_use_ipp_str = random_nested   # 1 if use structured interdependencies, 0 for random (see below)
  # benefit of implementing all projects in I_k = B_k
  # B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
  # NOTE: in paper, I've removed reference to multiplier -- additive effects only
  my_alpha = my_alpha # additive effect for benefits
  my_gamma = my_gamma # multiplicative effect for benefits
  my_beta = my_beta # additive effect for costs
  my_phi = my_phi # multiplicative effect for costs
  my_selprobs = my_selprob # one of "equal", "prop", "invprop"
  # for negatively related projects
  my_nint_neg = 8    # size of subset of related projects
  my_order_int_proj_neg = c(3,2) # order of interdependencies between projects
  my_n_int_proj_neg = c(4,2) # number of interdependencies of each order
  my_use_ipp_str_neg = 0   # 1 if use structured interdependencies, 0 for random (see below)
  # penalty of implementing all projects in I_k = B_k
  # B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
  my_alpha_neg = c(0,0) # additive effect for benefits
  my_gamma_neg = c(0,0) # multiplicative effect for benefits
  my_beta_neg = c(0,0) # additive effect for costs
  my_phi_neg = c(0,0) # multiplicative effect for costs
  my_selprobs_neg = "equal" # one of "equal", "prop", "invprop"
  
  n = length(my_bp) # number of projects
  
    # generate set of projects involved in positive interdependencies
    selprobs = compute_selection_probs(selprobs = my_selprob, bp=my_bp,cp=my_cp)
    my_starting_proj = sample(1:n,my_nint,prob=selprobs)
    
    my_ipp = create_interdependencies(starting_proj=my_starting_proj,
                                      n_int_proj=my_n_int_proj,
                                      order_int_proj=my_order_int_proj,
                                      use_ipp_str=my_use_ipp_str)
  
  #my_ipp
  # generate negative project interdependencies
    # generate set of projects involved in negative interdependencies
    selprobs_neg = compute_selection_probs(selprobs = my_selprobs_neg,bp=my_bp,cp=my_cp)
    my_starting_proj_neg = sample(1:n,my_nint_neg,prob=selprobs_neg)
    
    my_ipp_neg = create_interdependencies(starting_proj=my_starting_proj_neg,
                                          n_int_proj=my_n_int_proj_neg,
                                          order_int_proj=my_order_int_proj_neg,
                                          use_ipp_str=my_use_ipp_str_neg)
  
  #my_ipp_neg
  # compute benefits and costs of positive interdependencies
    my_BC = compute_interdependent_BC(ipp=my_ipp,
                                      bp=my_bp,
                                      cp=my_cp,
                                      alpha=my_alpha,
                                      gamma=my_gamma,
                                      beta=my_beta,
                                      phi=my_phi)
  
  #my_BC
  # compute benefits and costs of negative interdependencies
    my_BC_neg = compute_interdependent_BC(ipp=my_ipp_neg,
                                          bp=my_bp,
                                          cp=my_cp,
                                          alpha=my_alpha_neg,
                                          gamma=my_gamma_neg,
                                          beta=my_beta_neg,
                                          phi=my_phi_neg)
  
  return (list(my_ipp = my_ipp, my_ipp_neg = my_ipp_neg, my_BC = my_BC, my_BC_neg))
}

getContext = function(nproj, my_nCV, my_budget, my_alpha, my_selprob = "equal", random_nested = 0, 
                      interaction_pool = 10, my_bp = NULL, my_cp = NULL, my_gamma, my_ipp = NULL, 
                      my_ipp_neg = NULL, my_BC = NULL, my_BC_neg = NULL, order_int_proj = c(5,4,3,2), 
                      n_int_proj = c(2,6,8,10), my_beta = c(0,0,0,0), my_phi = c(0,0,0,0)){
  
  # for positively related projects
  my_nint = interaction_pool    # size of subset of related projects
  my_order_int_proj = order_int_proj #c(6,5,4,3,2) # order of interdependencies between projects
  my_n_int_proj = n_int_proj #c(1,3,5,6,10) # number of interdependencies of each order
  my_use_ipp_str = random_nested   # 1 if use structured interdependencies, 0 for random (see below)
  # benefit of implementing all projects in I_k = B_k
  # B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
  # NOTE: in paper, I've removed reference to multiplier -- additive effects only
  my_alpha = my_alpha # additive effect for benefits
  my_gamma = my_gamma # multiplicative effect for benefits
  my_beta = my_beta # additive effect for costs
  my_phi = my_phi # multiplicative effect for costs
  my_selprobs = my_selprob # one of "equal", "prop", "invprop"
  # for negatively related projects
  my_nint_neg = 8    # size of subset of related projects
  my_order_int_proj_neg = c(3,2) # order of interdependencies between projects
  my_n_int_proj_neg = c(4,2) # number of interdependencies of each order
  my_use_ipp_str_neg = 0   # 1 if use structured interdependencies, 0 for random (see below)
  # penalty of implementing all projects in I_k = B_k
  # B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
  my_alpha_neg = c(0,0) # additive effect for benefits
  my_gamma_neg = c(0,0) # multiplicative effect for benefits
  my_beta_neg = c(0,0) # additive effect for costs
  my_phi_neg = c(0,0) # multiplicative effect for costs
  my_selprobs_neg = "equal" # one of "equal", "prop", "invprop"
  
  ################################
  ######## end user data
  ################################
  
  #x = generate_project_data(nproj = nproj, minbc = 0.008, maxbc = 0.013, mincp = 52, maxcp = 550)
  
  #my_bp = x$bp
  #my_cp = x$cp
  if(is.null(my_bp) & is.null(my_cp)){
    x = read.csv("liesio_data.csv")
    my_bp = x[,2]
    my_cp = x$cost
  }
  
  
  n = length(my_bp) # number of projects
  
  # generate positive project interdependencies
  if(is.null(my_ipp)){
    # generate set of projects involved in positive interdependencies
    selprobs = compute_selection_probs(selprobs = my_selprobs,bp=my_bp,cp=my_cp)
    my_starting_proj = sample(1:n,my_nint,prob=selprobs)
    
    my_ipp = create_interdependencies(starting_proj=my_starting_proj,
                                      n_int_proj=my_n_int_proj,
                                      order_int_proj=my_order_int_proj,
                                      use_ipp_str=my_use_ipp_str)
  }
  #my_ipp
  # generate negative project interdependencies
  if(is.null(my_ipp_neg)){
    # generate set of projects involved in negative interdependencies
    selprobs_neg = compute_selection_probs(selprobs = my_selprobs_neg,bp=my_bp,cp=my_cp)
    my_starting_proj_neg = sample(1:n,my_nint_neg,prob=selprobs_neg)
    
    my_ipp_neg = create_interdependencies(starting_proj=my_starting_proj_neg,
                                          n_int_proj=my_n_int_proj_neg,
                                          order_int_proj=my_order_int_proj_neg,
                                          use_ipp_str=my_use_ipp_str_neg)
  }
  #my_ipp_neg
  # compute benefits and costs of positive interdependencies
  if(is.null(my_BC)){
    my_BC = compute_interdependent_BC(ipp=my_ipp,
                                      bp=my_bp,
                                      cp=my_cp,
                                      alpha=my_alpha,
                                      gamma=my_gamma,
                                      beta=my_beta,
                                      phi=my_phi)
  }
  #my_BC
  # compute benefits and costs of negative interdependencies
  if(is.null(my_BC_neg) & length(my_ipp_neg) > 0){
    my_BC_neg = compute_interdependent_BC(ipp=my_ipp_neg,
                                          bp=my_bp,
                                          cp=my_cp,
                                          alpha=my_alpha_neg,
                                          gamma=my_gamma_neg,
                                          beta=my_beta_neg,
                                          phi=my_phi_neg)
  }
  
  return (list(ipp=c(my_ipp,my_ipp_neg),
               order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
               bp  = my_bp,
               Bi = c(my_BC$Bi,my_BC_neg$Bi),
               cp = my_cp,
               Ci = c(my_BC$Ci,my_BC_neg$Ci),
               budget = my_budget))
}

#Possible strategies are:
# opt
# nadir
# random
# atb
# atb_full
# atb_mvp
# atb_lvp
# atb_rvp
# atv
# atc
# lex

getPortfolio = function(strategy, context, nCV = 3, calculateDomPrevalence = F){
  switch(strategy, 
         opt={
           sol = solve_portfolio(ipp=context$ipp,
                                 order_int_proj = context$order_int_proj,
                                 bp  = context$bp,
                                 Bi = context$Bi,
                                 cp = context$cp,
                                 Ci = context$Ci,
                                 budget = context$budget)
         },
         nad={
           sol = solve_portfolio(ipp=context$ipp,
                           order_int_proj = context$order_int_proj,
                           bp  = context$bp,
                           Bi = context$Bi,
                           cp = context$cp,
                           Ci = context$Ci,
                           budget = context$budget,
                           max = FALSE)   
         },
         random={
           sol = construct_random_portfolios(nRP = 1,
                                 nCV = nCV,           
                                 ipp=context$ipp,
                                 bp  = context$bp,
                                 Bi = context$Bi,
                                 cp = context$cp,
                                 Ci = context$Ci,
                                 budget = context$budget)   
         },
         atb={
           sol = take_the_best(nCV = nCV,           
                               ipp=context$ipp,
                               bp  = context$bp,
                               Bi = context$Bi,
                               cp = context$cp,
                               Ci = context$Ci,
                               budget = context$budget)  
         },
         atb_full={
           sol = greedy_netvalue(nCV = nCV,           
                               ipp=context$ipp,
                               bp  = context$bp,
                               Bi = context$Bi,
                               cp = context$cp,
                               Ci = context$Ci,
                               budget = context$budget)  
         },
         atb_mvp={
           sol = mvp_max(nCV = nCV,           
                                 ipp=context$ipp,
                                 bp  = context$bp,
                                 Bi = context$Bi,
                                 cp = context$cp,
                                 Ci = context$Ci,
                                 budget = context$budget)  
         },
         atb_lvp={
           sol = lvp_max(nCV = nCV,           
                         ipp=context$ipp,
                         bp  = context$bp,
                         Bi = context$Bi,
                         cp = context$cp,
                         Ci = context$Ci,
                         budget = context$budget)  
         },
         atb_rvp={
           sol = rvp_max(nCV = nCV,           
                         ipp=context$ipp,
                         bp  = context$bp,
                         Bi = context$Bi,
                         cp = context$cp,
                         Ci = context$Ci,
                         budget = context$budget)  
         },
         atv={
           sol = greedy_value(nCV = nCV,           
                         ipp=context$ipp,
                         bp  = context$bp,
                         Bi = context$Bi,
                         cp = context$cp,
                         Ci = context$Ci,
                         budget = context$budget)  
         },
         atc={
           sol = greedy_cost(nCV = nCV,           
                              ipp=context$ipp,
                              bp  = context$bp,
                              Bi = context$Bi,
                              cp = context$cp,
                              Ci = context$Ci,
                              budget = context$budget)  
         },
         lex={
           sol = construct_lex_portfolio(nCV = nCV,           
                                         ipp=context$ipp,
                                         bp  = context$bp,
                                         Bi = context$Bi,
                                         cp = context$cp,
                                         Ci = context$Ci,
                                         budget = context$budget)  
         },
         dom={
           #We normalize values, cost and budget.
           nor_bp = (context$bp - min(context$bp))/(max(context$bp)-min(context$bp))
           nor_cp = (context$cp - min(context$cp))/(max(context$cp)-min(context$cp))
           sol = construct_dombased_portfolios(
                             nRP = 1,
                             nCV = nCV,           
                             ipp=context$ipp,
                             bp  = context$bp,
                             Bi = context$Bi,
                             cp = context$cp,
                             Ci = context$Ci,
                             budget = context$budget,
                             nor_bp = nor_bp,
                             nor_cp = nor_cp,
                             calculateDomPrevalence = calculateDomPrevalence)  
          
         }
  )
         
  return(sol)
}





