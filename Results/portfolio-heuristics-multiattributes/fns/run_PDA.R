run_PDA = function(nproj, my_nCV = 3, my_budget_perc, my_alpha, my_selprob = "equal", random_nested = 0, 
                   interaction_pool = 10, my_gamma, my_ipp = NULL, 
                   my_ipp_neg = NULL, my_BC = NULL, my_BC_neg = NULL, my_BC_EQW = NULL, my_BC_neg_EQW = NULL, my_BC_LEX = NULL, my_BC_neg_LEX = NULL, my_BC_SAT = NULL, my_BC_neg_SAT = NULL, order_int_proj = c(5,4,3,2), 
                   n_int_proj = c(2,6,8,10), my_beta = c(0,0,0,0), my_phi = c(0,0,0,0),
                   neg_int_nint_multiplier = 1, neg_int_BC_multiplier = 0, bp_type = NULL, ...){ 
  
  ################################
  ######## enter user data
  ################################
  #browser()
  # for positively related projects
  my_nint = interaction_pool    # size of subset of related projects
  my_order_int_proj = order_int_proj #c(6,5,4,3,2) # order of interdependencies between projects
  my_n_int_proj = n_int_proj #c(1,3,5,6,10) # number of interdependencies of each order
  my_use_ipp_str = random_nested   # 1 if use structured interdependencies, 0 for random (see below)
  # benefit of implementing all projects in I_k = B_k
  # B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
  # NOTE: in paper, I've removed reference to multiplier -- additive effects only
  my_alpha = rep(my_alpha, length(n_int_proj)) # additive effect for benefits
  my_gamma = rep(my_gamma, length(n_int_proj)) # multiplicative effect for benefits
  my_beta = my_beta # additive effect for costs
  my_phi = my_phi # multiplicative effect for costs
  my_selprobs = my_selprob # one of "equal", "prop", "invprop"
  # for negatively related projects
  my_nint_neg = interaction_pool    # size of subset of related projects
  my_order_int_proj_neg = order_int_proj # order of interdependencies between projects
  my_n_int_proj_neg = pmax(1, round(neg_int_nint_multiplier * n_int_proj)) # number of interdependencies of each order
  my_use_ipp_str_neg = random_nested   # 1 if use structured interdependencies, 0 for random (see below)
  # penalty of implementing all projects in I_k = B_k
  # B_k = bonus_add + bonus_mult * mean benefit of projects in I_k
  my_alpha_neg = neg_int_BC_multiplier * my_alpha # additive effect for benefits
  my_gamma_neg = neg_int_BC_multiplier * my_gamma # multiplicative effect for benefits
  my_beta_neg = my_beta # additive effect for costs
  my_phi_neg = my_phi # multiplicative effect for costs
  my_selprobs_neg = my_selprob # one of "equal", "prop", "invprop"
  
  ################################
  ######## end user data
  ################################
  # nproj <- 10 #Temporary: only for testing
  # bp_type <- c("uni") #Temporary: only for testing
  #browser()
  na <- 6 # Number of attributes - Change number as required
  na1 <- na + 1
  #x<-array(0,dim = c(nproj,na))
  # z = array(0,dim=c(na))
  # z = as.data.frame(z)
  # z$rank = sample(1:na,na,replace=F)
  # z$zip = (1/z$rank)^1.35
  # sum_zip = sum(z$zip)
  # z$w = z$zip/sum_zip
  # w = z$w
  w = runif(na,0,1)
  max.w.pos<-which(w %in% max(w))
  max.w.pos<-which(w %in% max(w))
  
  my_bp1<-c()
  my_bp<-c()
  my_bp_EQW<-c()
  my_bp_LEX<-c()
  my_bp_SAT<-c()
  #for (i in 1:na){
    if(bp_type == "uni"){
    x <- generateUniformData(nproj, 0, 20)
  } else if(bp_type == "pos"){
    x <- generateSkewedData(nproj, 5, 2, T)
  } else if(bp_type == "neg"){
    x <- generateSkewedData(nproj, 5, 2, F)
  } else { stop("my_bp1 must be uni,pos, or neg") }
  #my_bp1<-rbind(my_bp1,x$value)  
  #}
  #my_bp1<-x$value
  my_bp1<-t(x[,1:na])
  my_bp = apply(t(my_bp1*w),1,sum) #WADD: sum across columns
  my_bp_EQW  <- apply(my_bp1,2,mean) #EQW: average across rows
  my_bp_LEX = my_bp1[max.w.pos,]

  my_bp2<-my_bp1
  my_bp3<-my_bp1
  my_bp2[which(my_bp2<=10)]<-NA  #Replace values below 10 with NAs
  my_bp3[which(my_bp3<=12)]<-NA
  my_bp2[which(my_bp2>10)]<-1  #Replace values above 10 with 1s
  my_bp3[which(my_bp3>12)]<-1
  
  if(bp_type == "uni"){
    my_bp_SAT  <- apply(my_bp2,2,sum,na.rm=TRUE) #SAT: sum across rows
    my_bp_SAT <- ifelse(is.na(my_bp_SAT), 0, my_bp_SAT) #replace NA with zero
    
  } else if(bp_type == "pos"){
    
    my_bp_SAT  <- apply(my_bp3,2,sum,na.rm=TRUE) #SAT: sum across rows
    my_bp_SAT <- ifelse(is.na(my_bp_SAT), 0, my_bp_SAT) #replace NA with zero
    
  } else if(bp_type == "neg"){
    
    my_bp_SAT  <- apply(my_bp3,2,sum,na.rm=TRUE) #SAT: sum across rows
    my_bp_SAT <- ifelse(is.na(my_bp_SAT), 0, my_bp_SAT) #replace NA with zero
    
  } else { stop("my_bp must be uni,pos, or neg") }
  
  #my_cp <- x$cost
  my_cp <- x[,na1]
  
  #write.csv(my_bp1,file="C:/Users/DKantu01/Documents/my_bp1.csv")
  
  n <- length(my_bp) # number of projects
  
  my_budget <- sum(my_cp) * my_budget_perc
  
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

  # compute benefits and costs of positive interdependencies_WADD
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
  # compute benefits and costs of negative interdependencies_WADD
  if(is.null(my_BC_neg)){
    my_BC_neg = compute_interdependent_BC(ipp=my_ipp_neg,
                                          bp=my_bp,
                                          cp=my_cp,
                                          alpha=my_alpha_neg,
                                          gamma=my_gamma_neg,
                                          beta=my_beta_neg,
                                          phi=my_phi_neg)
  }
  # compute benefits and costs of positive interdependencies_EQW
  if(is.null(my_BC_EQW)){
    my_BC_EQW = compute_interdependent_BC(ipp=my_ipp,
                                      bp=my_bp_EQW,
                                      cp=my_cp,
                                      alpha=my_alpha,
                                      gamma=my_gamma,
                                      beta=my_beta,
                                      phi=my_phi)
  }
  #my_BC
  # compute benefits and costs of negative interdependencies_EQW
  if(is.null(my_BC_neg_EQW)){
    my_BC_neg_EQW = compute_interdependent_BC(ipp=my_ipp_neg,
                                          bp=my_bp_EQW,
                                          cp=my_cp,
                                          alpha=my_alpha_neg,
                                          gamma=my_gamma_neg,
                                          beta=my_beta_neg,
                                          phi=my_phi_neg)
  }
  # compute benefits and costs of positive interdependencies_LEX
  if(is.null(my_BC_LEX)){
    my_BC_LEX = compute_interdependent_BC(ipp=my_ipp,
                                      bp=my_bp_LEX,
                                      cp=my_cp,
                                      alpha=my_alpha,
                                      gamma=my_gamma,
                                      beta=my_beta,
                                      phi=my_phi)
  }
  #my_BC
  # compute benefits and costs of negative interdependencies_LEX
  if(is.null(my_BC_neg_LEX)){
    my_BC_neg_LEX = compute_interdependent_BC(ipp=my_ipp_neg,
                                          bp=my_bp_LEX,
                                          cp=my_cp,
                                          alpha=my_alpha_neg,
                                          gamma=my_gamma_neg,
                                          beta=my_beta_neg,
                                          phi=my_phi_neg)
  }
  # compute benefits and costs of positive interdependencies_SAT
  if(is.null(my_BC_SAT)){
    my_BC_SAT = compute_interdependent_BC(ipp=my_ipp,
                                      bp=my_bp_SAT,
                                      cp=my_cp,
                                      alpha=my_alpha,
                                      gamma=my_gamma,
                                      beta=my_beta,
                                      phi=my_phi)
  }
  #my_BC
  # compute benefits and costs of negative interdependencies_SAT
  if(is.null(my_BC_neg_SAT)){
    my_BC_neg_SAT = compute_interdependent_BC(ipp=my_ipp_neg,
                                          bp=my_bp_SAT,
                                          cp=my_cp,
                                          alpha=my_alpha_neg,
                                          gamma=my_gamma_neg,
                                          beta=my_beta_neg,
                                          phi=my_phi_neg)
  }
  #my_BC_neg
  ################################
  ######## models
  ################################
  # solve a MILP to get optimal portfolio
  #print("optsol")
  #for WADD
  #browser()
  my_optsol = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                              order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                              bp  = my_bp,
                              Bi = c(my_BC$Bi,my_BC_neg$Bi),
                              cp = my_cp,
                              Ci = c(my_BC$Ci,my_BC_neg$Ci),
                              budget = my_budget)
  my_optsol = evaluate_z(my_optsol$solution[1:n], ipp=c(my_ipp,my_ipp_neg), bp = my_bp,  cp = my_cp, Bi = c(my_BC$Bi,my_BC_neg$Bi), Ci = c(my_BC$Ci,my_BC_neg$Ci), budget = my_budget, decompose = T)
  #eval = evaluate_z(my_optsol$solution[1:n], ipp=c(my_ipp,my_ipp_neg), bp = my_bp,  cp = my_cp, Bi = c(my_BC$Bi,my_BC_neg$Bi), Ci = c(my_BC$Ci,my_BC_neg$Ci), budget = my_budget)
  #print(eval)
  #print(my_budget)
  #my_optsol
  # solve a MILP to get nadir portfolio
  #print("nadsol")
  # #For EQW heuristic
  my_optsol_EQW = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                              order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                              bp  = my_bp_EQW,
                              Bi = c(my_BC_EQW$Bi,my_BC_neg_EQW$Bi),
                              cp = my_cp,
                              Ci = c(my_BC_EQW$Ci,my_BC_neg_EQW$Ci),
                              budget = my_budget)
  my_optsol_EQW = evaluate_z(my_optsol_EQW$solution[1:n], ipp=c(my_ipp,my_ipp_neg), bp = my_bp,  cp = my_cp, Bi = c(my_BC$Bi,my_BC_neg$Bi), Ci = c(my_BC$Ci,my_BC_neg$Ci), budget = my_budget, decompose = T)

  #for LEX
  my_optsol_LEX = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                              order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                              bp  = my_bp_LEX,
                              Bi = c(my_BC_LEX$Bi,my_BC_neg_LEX$Bi),
                              cp = my_cp,
                              Ci = c(my_BC_LEX$Ci,my_BC_neg_LEX$Ci),
                              budget = my_budget)
  my_optsol_LEX = evaluate_z(my_optsol_LEX$solution[1:n], ipp=c(my_ipp,my_ipp_neg), bp = my_bp,  cp = my_cp, Bi = c(my_BC$Bi,my_BC_neg$Bi), Ci = c(my_BC$Ci,my_BC_neg$Ci), budget = my_budget, decompose = T)
  
  #for SAT
  my_optsol_SAT = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                              order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                              bp  = my_bp_SAT,
                              Bi = c(my_BC_SAT$Bi,my_BC_neg_SAT$Bi),
                              cp = my_cp,
                              Ci = c(my_BC_SAT$Ci,my_BC_neg_SAT$Ci),
                              budget = my_budget)
  my_optsol_SAT = evaluate_z(my_optsol_SAT$solution[1:n], ipp=c(my_ipp,my_ipp_neg), bp = my_bp,  cp = my_cp, Bi = c(my_BC$Bi,my_BC_neg$Bi), Ci = c(my_BC$Ci,my_BC_neg$Ci), budget = my_budget, decompose = T)
  
  my_nadsol = solve_portfolio(ipp=c(my_ipp,my_ipp_neg),
                              order_int_proj = c(my_order_int_proj,my_order_int_proj_neg),
                              bp  = my_bp,
                              Bi = c(my_BC$Bi,my_BC_neg$Bi),
                              cp = my_cp,
                              Ci = c(my_BC$Ci,my_BC_neg$Ci),
                              budget = my_budget,
                              max = FALSE)
  
  my_nadsol = evaluate_z(my_nadsol$solution[1:n], ipp=c(my_ipp,my_ipp_neg), bp = my_bp,  cp = my_cp, Bi = c(my_BC$Bi,my_BC_neg$Bi), Ci = c(my_BC$Ci,my_BC_neg$Ci), budget = my_budget, decompose = T)
  
  #my_nadsol
  # compute mean performance of random feasible portfolio
  my_randsols = construct_random_portfolios(nRP = 1,
                                            nCV = my_nCV,
                                            ipp=c(my_ipp,my_ipp_neg),
                                            bp  = my_bp,
                                            Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                            cp = my_cp,
                                            Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                            budget = my_budget)
  #my_randsols$benefit
  #hist(my_randsols$benefit)
  #mean(my_randsols$benefit)
  # generate greedy portfolio
  my_ttb = take_the_best(nCV = my_nCV,
                         ipp=c(my_ipp,my_ipp_neg),
                         bp  = my_bp,
                         Bi = c(my_BC$Bi,my_BC_neg$Bi),
                         cp = my_cp,
                         Ci = c(my_BC$Ci,my_BC_neg$Ci),
                         budget = my_budget)
  
  greedy_netvalue = greedy_netvalue(nCV = my_nCV,
                                    ipp=c(my_ipp,my_ipp_neg),
                                    bp  = my_bp,
                                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                    cp = my_cp,
                                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                    budget = my_budget)
  
  #if(greedy_netvalue$benefit > my_optsol$benefit){browser()}
  
  
  
  mvp_max = mvp_max(nCV = my_nCV,
                    ipp=c(my_ipp,my_ipp_neg),
                    bp  = my_bp,
                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                    cp = my_cp,
                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                    budget = my_budget)
  
  lvp_max = lvp_max(nCV = my_nCV,
                    ipp=c(my_ipp,my_ipp_neg),
                    bp  = my_bp,
                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                    cp = my_cp,
                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                    budget = my_budget)
  
  rvp_max = rvp_max(nCV = my_nCV,
                    ipp=c(my_ipp,my_ipp_neg),
                    bp  = my_bp,
                    Bi = c(my_BC$Bi,my_BC_neg$Bi),
                    cp = my_cp,
                    Ci = c(my_BC$Ci,my_BC_neg$Ci),
                    budget = my_budget)
  
  greedy_value = greedy_value(nCV = my_nCV,
                              ipp=c(my_ipp,my_ipp_neg),
                              bp  = my_bp,
                              Bi = c(my_BC$Bi,my_BC_neg$Bi),
                              cp = my_cp,
                              Ci = c(my_BC$Ci,my_BC_neg$Ci),
                              budget = my_budget)
  
  greedy_cost = greedy_cost(nCV = my_nCV,
                            ipp=c(my_ipp,my_ipp_neg),
                            bp  = my_bp,
                            Bi = c(my_BC$Bi,my_BC_neg$Bi),
                            cp = my_cp,
                            Ci = c(my_BC$Ci,my_BC_neg$Ci),
                            budget = my_budget)
  #my_ttb$benefit
  #We normalize values, cost and budget.
  nor_bp = (my_bp - min(my_bp))/(max(my_bp)-min(my_bp))
  nor_cp = (my_cp - min(my_cp))/(max(my_cp)-min(my_cp))
  nor_budget = (my_budget - min(my_cp))/(max(my_cp)-min(my_cp))
  
  my_dom = construct_dombased_portfolios(nRP = 1,
                                         nCV = my_nCV,
                                         ipp=c(my_ipp,my_ipp_neg),
                                         nor_bp  = nor_bp,
                                         bp = my_bp,
                                         Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                         nor_cp = nor_cp,
                                         cp = my_cp,
                                         Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                         budget = my_budget)
  
  
  
  my_dom_eval <- apply(my_dom$final_z, FUN = evaluate_z, MARGIN = 1, ipp = c(my_ipp,my_ipp_neg), bp  = my_bp, Bi = c(my_BC$Bi,my_BC_neg$Bi), 
                       cp = my_cp, Ci = c(my_BC$Ci,my_BC_neg$Ci), budget = my_budget) 
  
  lex = construct_lex_portfolio(nCV = my_nCV,
                                ipp=c(my_ipp,my_ipp_neg),
                                bp  = my_bp,
                                Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                cp = my_cp,
                                Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                budget = my_budget)
  
  lex_3cuesbinary = construct_lex_portfolio_3cues(nCV = my_nCV,
                                                  ipp=c(my_ipp,my_ipp_neg),
                                                  bp  = my_bp,
                                                  Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                                  cp = my_cp,
                                                  Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                                  budget = my_budget,
                                                  binary_num_interactions = T)
  
  lex_3cues = construct_lex_portfolio_3cues(nCV = my_nCV,
                                            ipp=c(my_ipp,my_ipp_neg),
                                            bp  = my_bp,
                                            Bi = c(my_BC$Bi,my_BC_neg$Bi),
                                            cp = my_cp,
                                            Ci = c(my_BC$Ci,my_BC_neg$Ci),
                                            budget = my_budget, binary_num_interactions = F)
  
  #my_dom_benefit = unlist(lapply(my_dom_eval, FUN= benefit))
  #order(cueValidity(cp, bp, budget))
  
  # my_cumdom_o1 = construct_cumdombased_portfolios(nRP = 50,
  #                                                 nCV = my_nCV,
  #                                                 ipp=c(my_ipp,my_ipp_neg),
  #                                                 nor_bp  = nor_bp,
  #                                                 bp = my_bp,
  #                                                 Bi = c(my_BC$Bi,my_BC_neg$Bi),
  #                                                 nor_cp = nor_cp,
  #                                                 cp = my_cp,
  #                                                 Ci = c(my_BC$Ci,my_BC_neg$Ci),
  #                                                 budget = my_budget,
  #                                                 cueOrder = c(1,2))
  
  my_cumdom_o1 <- list(final_z=0,benefit=0,cost=0,feasible=0,g=0)
  
  # my_cumdom_o2 = construct_cumdombased_portfolios(nRP = 50,
  #                                                nCV = my_nCV,
  #                                                ipp=c(my_ipp,my_ipp_neg),
  #                                                nor_bp  = nor_bp,
  #                                                bp = my_bp,
  #                                                Bi = c(my_BC$Bi,my_BC_neg$Bi),
  #                                                nor_cp = nor_cp,
  #                                                cp = my_cp,
  #                                                Ci = c(my_BC$Ci,my_BC_neg$Ci),
  #                                                budget = my_budget,
  #                                                cueOrder = c(2,1))
  
  my_cumdom_o2 <- list(final_z=0,benefit=0,cost=0,feasible=0,g=0)
  
  #my_optsol_EQW = my_optsol
  # output results
  v_zopt = my_optsol$benefit  # value of optimal portfolio
  v_zopt_EQW = my_optsol_EQW$benefit
  #v_zopt_EQW = my_optsol$benefit
  v_zopt_LEX = my_optsol_LEX$benefit
  v_zopt_SAT = my_optsol_SAT$benefit
  v_znad = my_nadsol$benefit  # value of nadir portfolio
  v_zrand = mean(my_randsols$benefit)   # mean value of random portfolio
  v_zttb = my_ttb$benefit   # value of take-the-best portfolio
  v_greedy_netvalue = greedy_netvalue$benefit# value of greedy net value portfolio
  v_dom = mean(my_dom$benefit)
  v_greedyvalue = mean(greedy_value$benefit)
  v_greedycost = mean(greedy_cost$benefit)
  v_mvpmax = mean(mvp_max$benefit)
  v_lvpmax = mean(lvp_max$benefit)
  v_rvpmax = mean(rvp_max$benefit)
  v_lex = lex$benefit
  v_lex3cb = lex_3cuesbinary$benefit
  v_lex3c = lex_3cues$benefit
  
  v_zopt_bare = (my_optsol$benefit_bare-v_znad) / (v_zopt - v_znad) # bare value of optimal portfolio
  v_zopt_EQW_bare = (my_optsol_EQW$benefit_bare-v_znad) / (v_zopt - v_znad) # bare value of optimal portfolio
  v_zopt_LEX_bare = (my_optsol_LEX$benefit_bare-v_znad) / (v_zopt - v_znad) # bare value of optimal portfolio
  v_zopt_SAT_bare = (my_optsol_SAT$benefit_bare-v_znad) / (v_zopt - v_znad) # bare value of optimal portfolio
  v_znad_bare = (my_nadsol$benefit_bare-v_znad) / (v_zopt - v_znad)  # bare value of nadir portfolio
  v_zrand_bare = (mean(my_randsols$benefit_bare)-v_znad) / (v_zopt - v_znad)   # mean bare value of random portfolio
  v_zttb_bare = (my_ttb$benefit_bare -v_znad) / (v_zopt - v_znad)  # bare value of take-the-best portfolio
  v_greedy_netvalue_bare = (greedy_netvalue$benefit_bare -v_znad) / (v_zopt - v_znad)# bare value of greedy net value portfolio
  v_dom_bare = (mean(my_dom$benefit_bare) -v_znad) / (v_zopt - v_znad)
  v_greedyvalue_bare = (mean(greedy_value$benefit_bare) -v_znad) / (v_zopt - v_znad)
  v_greedycost_bare = (mean(greedy_cost$benefit_bare) -v_znad) / (v_zopt - v_znad)
  v_mvpmax_bare = (mean(mvp_max$benefit_bare) -v_znad) / (v_zopt - v_znad)
  v_lvpmax_bare = (mean(lvp_max$benefit_bare) -v_znad) / (v_zopt - v_znad)
  v_rvpmax_bare = (mean(rvp_max$benefit_bare) -v_znad) / (v_zopt - v_znad)
  v_lex_bare = (lex$benefit_bare -v_znad) / (v_zopt - v_znad)
  v_lex3c_bare = (lex_3cues$benefit_bare -v_znad) / (v_zopt - v_znad)
  v_lex3cb_bare = (lex_3cuesbinary$benefit_bare -v_znad) / (v_zopt - v_znad)
  
  v_zopt
  v_zopt_EQW
  v_zopt_LEX
  v_zopt_SAT
  v_znad
  v_zrand
  v_zttb
  
  # standardised outputs
  PL_zopt = 1
  PL_znad = 0
  PL_zopt_EQW = (v_zopt_EQW - v_znad) / (v_zopt-v_znad)
  PL_zopt_LEX = (v_zopt_LEX - v_znad) / (v_zopt-v_znad)
  PL_zopt_SAT =  (v_zopt_SAT - v_znad) / (v_zopt-v_znad)
  PL_zrand = (v_zrand-v_znad) / (v_zopt - v_znad)
  PL_zttb = (v_zttb-v_znad) / (v_zopt - v_znad)
  PL_greedy_netvalue = (v_greedy_netvalue-v_znad) / (v_zopt - v_znad)
  PL_zdom = (v_dom-v_znad) / (v_zopt - v_znad)
  PL_greedyvalue = (v_greedyvalue-v_znad) / (v_zopt - v_znad)
  PL_greedycost = (v_greedycost-v_znad) / (v_zopt - v_znad)
  PL_mvpmax = (v_mvpmax-v_znad) / (v_zopt - v_znad)
  PL_lvpmax = (v_lvpmax-v_znad) / (v_zopt - v_znad)
  PL_rvpmax = (v_rvpmax-v_znad) / (v_zopt - v_znad)
  PL_lex = (v_lex - v_znad) / (v_zopt - v_znad)
  PL_lex3c = (v_lex3c - v_znad) / (v_zopt - v_znad)
  PL_lex3cb = (v_lex3cb - v_znad) / (v_zopt - v_znad)
  
  # collect inputs and outputs
  # inputs = c(n = n, ncv = my_nCV, budget = my_budget, alpha = my_alpha[1], gamma = my_gamma[1], selprob = my_selprob, random_nested, interaction_pool)
  # outputs = c(v_zopt, v_znad, v_zrand, v_zttb, v_dom, v_greedy_netvalue, v_greedyvalue, v_greedycost, v_mvpmax, v_lvpmax, v_rvpmax, v_lex, v_lex3cb, v_lex3c,
  #             PL_zopt, PL_znad, PL_zrand, PL_zttb,PL_zdom, PL_greedy_netvalue, PL_greedyvalue, PL_greedycost, PL_mvpmax, PL_lvpmax, PL_rvpmax, PL_lex, PL_lex3cb, PL_lex3c,
  #             v_zopt_bare, v_znad_bare, v_zrand_bare, v_zttb_bare, v_dom_bare, v_greedy_netvalue_bare, v_greedyvalue_bare, v_greedycost_bare, v_mvpmax_bare, v_lvpmax_bare, v_rvpmax_bare, v_lex_bare, v_lex3cb_bare, v_lex3c_bare)
  # ret = c(inputs, outputs)
  # names(ret) = c("nproj","nCV","budget","my_alpha","my_gamma","my_selprob","random_nested","interaction_pool",
  #                "opt","min","rand","ttb","dom","greedynet","greedyvalue","greedycost", "mvpmax","lvpmax","rvpmax", "lex", "lex3cb", "lex3c",
  #                "opt_nor","min_nor","rand_nor","ttb_nor","dom_nor","greedynet_nor","greedyvalue_nor", "greedycost_nor", "mvpmax_nor", "lvpmax_nor", "rvpmax_nor", "PL_lex","PL_lex3cb", "PL_lex3c",
  #                "opt_bare","min_bare","rand_bare","ttb_bare","dom_bare","greedynet_bare","greedyvalue_bare", "greedycost_bare", "mvpmax_bare", "lvpmax_bare", "rvpmax_bare", "v_lex_bare", "v_lex3cb_bare", "v_lex3c_bare")
  
  inputs_df = data.frame(n = n, ncv = my_nCV, budget = my_budget_perc, alpha = my_alpha[1], gamma = my_gamma[1], 
                         selprob = my_selprob, random_nested, interaction_pool, 
                         neg_int_nint_multiplier = neg_int_nint_multiplier, neg_int_BC_multiplier = neg_int_BC_multiplier, bp_type = bp_type,
                         stringsAsFactors = FALSE)
  outputs_df = data.frame(v_zopt, v_zopt_EQW, v_zopt_LEX, v_zopt_SAT, v_znad, v_zrand, v_zttb, v_dom, v_greedy_netvalue, v_greedyvalue, v_greedycost, v_mvpmax, v_lvpmax, v_rvpmax, v_lex, v_lex3cb, v_lex3c,
                          PL_zopt, PL_zopt_EQW,PL_zopt_LEX,PL_zopt_SAT,PL_znad, PL_zrand, PL_zttb,PL_zdom, PL_greedy_netvalue, PL_greedyvalue, PL_greedycost, PL_mvpmax, PL_lvpmax, PL_rvpmax, PL_lex, PL_lex3cb, PL_lex3c,
                          v_zopt_bare, v_zopt_EQW_bare, v_zopt_LEX_bare, v_zopt_SAT_bare, v_znad_bare, v_zrand_bare, v_zttb_bare, v_dom_bare, v_greedy_netvalue_bare, v_greedyvalue_bare, v_greedycost_bare, v_mvpmax_bare, v_lvpmax_bare, v_rvpmax_bare, v_lex_bare, v_lex3cb_bare, v_lex3c_bare,
                          stringsAsFactors = FALSE)
  names(outputs_df) = c("opt","opt_EQW","opt_LEX","opt_SAT","min","rand","ttb","dom","greedynet","greedyvalue","greedycost", "mvpmax","lvpmax","rvpmax", "lex", "lex3cb", "lex3c",
                        "opt_nor","opt_nor_EQW","opt_nor_LEX","opt_nor_SAT","min_nor","rand_nor","ttb_nor","dom_nor","greedynet_nor","greedyvalue_nor", "greedycost_nor", "mvpmax_nor", "lvpmax_nor", "rvpmax_nor", "PL_lex","PL_lex3cb", "PL_lex3c",
                        "opt_bare","opt_bare_EQW","opt_bare_LEX","opt_bare_SAT","min_bare","rand_bare","ttb_bare","dom_bare","greedynet_bare","greedyvalue_bare", "greedycost_bare", "mvpmax_bare", "lvpmax_bare", "rvpmax_bare", "v_lex_bare", "v_lex3cb_bare", "v_lex3c_bare")
  ret <- cbind.data.frame(inputs_df, outputs_df)
  
  return(ret)
}

benefit <- function(x){
  x$benefit
}