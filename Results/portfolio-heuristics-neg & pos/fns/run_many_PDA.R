# function running run_PDA many times

run_many_PDA <- function(nproj, my_budget_perc, my_alpha, my_selprob, random_nested, my_gamma, 
                         my_ipp = NULL, my_ipp_neg = NULL, my_BC = NULL, my_BC_neg = NULL, 
                         order_int_proj = c(5,4,3,2), n_int_proj = c(2,6,8,10), my_beta = c(0,0,0,0), my_phi = c(0,0,0,0),
                         neg_int_nint_multiplier, neg_int_BC_multiplier, bp_type, ...){
  # run N times 
  # res <- map_dfr(.x = 1:50, .f = run_PDA, nproj = nproj, my_budget_perc = my_budget_perc, my_alpha = my_alpha, 
  #            my_selprob = my_selprob, random_nested = random_nested, my_gamma = my_gamma, 
  #            my_ipp = NULL, my_ipp_neg = NULL, my_BC = NULL, my_BC_neg = NULL, 
  #            order_int_proj = order_int_proj, n_int_proj = n_int_proj, my_beta = my_beta, my_phi = my_phi,
  #            neg_int_nint_multiplier = neg_int_nint_multiplier, neg_int_BC_multiplier = neg_int_BC_multiplier, bp_type = bp_type)
  
  res_t <- list()
  for(i in 1:50){
    print(i)
    start_time <- Sys.time()
    print(start_time)
    res_t[[i]] = run_PDA(nproj = nproj, my_budget_perc = my_budget_perc, my_alpha = my_alpha, 
                         my_selprob = my_selprob, random_nested = random_nested, my_gamma = my_gamma,
                         my_ipp = NULL, my_ipp_neg = NULL, my_BC = NULL, my_BC_neg = NULL,
                         order_int_proj = order_int_proj, n_int_proj = n_int_proj, my_beta = my_beta, my_phi = my_phi,
                         neg_int_nint_multiplier = neg_int_nint_multiplier, neg_int_BC_multiplier = neg_int_BC_multiplier, bp_type = bp_type)
    
    end_time <- Sys.time()
    print(end_time - start_time)
  }
  print("done combo")
  res <- res_t %>% bind_rows()
  
  return(res)
}
