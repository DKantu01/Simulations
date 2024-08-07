### Main file for running the PDA simulation.
### Mostly this just involves running the scripts sourced below, which contain the 
### nuts-and-bolts of the simulation. The simulation consists of 2 main parts: 1)
### making the interactions between projects, 2) making portfolios using various
### heuristics. For more information on exactly how things are done, see the .R 
### files below, or see the paper.

### This code implements negative interactions between projects, and cleans up the 
### way in which the overall simulation runs (calls to map, rather than loops) after
### the sim study on positive interactions.

### Ian Durbach, 4/4/2019

rm(list = ls())
library(Rglpk)
library(purrr)
library(dplyr)

# function generating project values and costs
source("fns/generateSkewedData_fn.R")

# functions for generating interactions between projects 
# see individual .R files for details of what each does
source("fns/compute_selection_probs.R")
source("fns/create_interdependencies.R")
source("fns/compute_interdependent_BC.R")

# functions for creating various kinds of portfolios
source("fns/evaluate_z.R") # evaluates the value of any portfolio
source("fns/solve_portfolio.R") # computes the optimal portfolio
source("fns/construct_random_portfolios.R") # constructs random portfolios
source("fns/take_the_best.R") # constructs 'add-the-best' type portfolios
source("fns/dominance.R") # constructs a portfolio with the "Pareto heuristic"
source("fns/lex.R") # constructs portfolio with 'unit value with synergy' heuristic

# function putting it all together and running the PDA
source("fns/run_PDA.R")

# function running run_PDA many times (for simulation)
source("fns/run_many_PDA.R")

# parameter combinations for experiment

# first pars with no interactions (so don't need to vary pars related to interactions)
nproj <- c(50)
budget <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
my_alpha <- c(0)
my_gamma <- c(0)
my_selprob <- c("equal")
interaction_pool <- c(10)
random_nested = c(0, 1)
neg_int_nint_multiplier <- c(1)
neg_int_BC_multiplier <- -c(0)
bp_type <- c("uni", "pos", "neg")

pars_noint <- expand.grid(nproj = nproj, budget = budget, my_selprob = my_selprob, my_alpha = my_alpha, random_nested = random_nested, 
                          interaction_pool = interaction_pool, my_gamma = my_gamma,
                          neg_int_nint_multiplier = neg_int_nint_multiplier, neg_int_BC_multiplier = neg_int_BC_multiplier,
                          bp_type = bp_type, stringsAsFactors = FALSE)

nproj <- c(50)
budget <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
my_alpha <- c(3, 6)
my_gamma <- c(0.5, 1)
my_selprob <- c("equal","prop","invprop")
interaction_pool <- c(10)
random_nested = c(0, 1)
neg_int_nint_multiplier <- c(0.5, 1)
neg_int_BC_multiplier <- -c(0.5, 1)
bp_type <- c("uni", "pos", "neg")

pars_withint <- expand.grid(nproj = nproj, budget = budget, my_selprob = my_selprob, my_alpha = my_alpha, random_nested = random_nested, 
                            interaction_pool = interaction_pool, my_gamma = my_gamma,
                            neg_int_nint_multiplier = neg_int_nint_multiplier, neg_int_BC_multiplier = neg_int_BC_multiplier,
                            bp_type = bp_type, stringsAsFactors = FALSE)

pars <- rbind(pars_noint, pars_withint)

# save these to concatenate later with results
save(pars, file = "output/simstudy_pars.RData")
all_pars <- pars

# # illustrating running the simulation, once, and many times with purrr
# 
# # run once
# run_PDA(nproj = pars$nproj[1], my_budget_perc = pars$budget[1], my_selprob = pars$my_selprob[1], my_alpha = pars$my_alpha[1],
#         random_nested = pars$random_nested[1], interaction_pool = pars$interaction_pool[1], my_gamma = pars$my_gamma[1],
#         neg_int_nint_multiplier = pars$neg_int_nint_multiplier[1], neg_int_BC_multiplier = pars$neg_int_BC_multiplier[1],
#         bp_type = pars$bp_type[1])
# 
# # run N times
# map_dfr(.x = 1:10, .f = run_PDA, nproj = pars$nproj[1], my_budget_perc = pars$budget[1], my_selprob = pars$my_selprob[1], my_alpha = pars$my_alpha[1],
#         random_nested = pars$random_nested[1], interaction_pool = pars$interaction_pool[1], my_gamma = pars$my_gamma[1],
#         neg_int_nint_multiplier = pars$neg_int_nint_multiplier[1], neg_int_BC_multiplier = pars$neg_int_BC_multiplier[1],
#         bp_type = pars$bp_type[1])
# 
# # another way to run N times
# run_many_PDA(nproj = pars$nproj[1], my_budget_perc = pars$budget[1], my_selprob = pars$my_selprob[1], my_alpha = pars$my_alpha[1],
#              random_nested = pars$random_nested[1], interaction_pool = pars$interaction_pool[1], my_gamma = pars$my_gamma[1],
#              neg_int_nint_multiplier = pars$neg_int_nint_multiplier[1], neg_int_BC_multiplier = pars$neg_int_BC_multiplier[1],
#              bp_type = pars$bp_type[1])

# run the whole experiment, but divide the pars into a few sets so that you don't lose everything if it crashes
#all_pars <- pars
start_row <- 1
n_to_run <- 20
pars <- all_pars[start_row:(start_row + n_to_run - 1), ]
res1 <- list()
start_time <- Sys.time()
print(start_time)
for (i in 1:n_to_run){
pars1<-pars[i,]
res1[[i]] = run_many_PDA(nproj = pars1$nproj, my_budget_perc = pars1$budget, my_selprob = pars1$my_selprob, my_alpha = pars1$my_alpha, 
                         random_nested = pars1$random_nested, interaction_pool = pars1$interaction_pool, my_gamma = pars1$my_gamma,
                         neg_int_nint_multiplier = pars1$neg_int_nint_multiplier, neg_int_BC_multiplier = pars1$neg_int_BC_multiplier,
                         bp_type = pars1$bp_type)
print(paste("Combination",i))
}

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time )
res <- res1 %>% bind_rows()


saveRDS(res, file = paste0("output/PDA_simulation_results_",
                           start_row,"_",(start_row + n_to_run - 1),".rds"))

start_row <- 21
n_to_run <- 20
pars <- all_pars[start_row:(start_row + n_to_run - 1), ]
res1 <- list()
start_time <- Sys.time()
print(start_time)
for (i in 1:n_to_run){
  pars1<-pars[i,]
  res1[[i]] = run_many_PDA(nproj = pars1$nproj, my_budget_perc = pars1$budget, my_selprob = pars1$my_selprob, my_alpha = pars1$my_alpha, 
                           random_nested = pars1$random_nested, interaction_pool = pars1$interaction_pool, my_gamma = pars1$my_gamma,
                           neg_int_nint_multiplier = pars1$neg_int_nint_multiplier, neg_int_BC_multiplier = pars1$neg_int_BC_multiplier,
                           bp_type = pars1$bp_type)
  print(paste("Combination",i))
}

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time )
res <- res1 %>% bind_rows()


saveRDS(res, file = paste0("output/PDA_simulation_results_",
                           start_row,"_",(start_row + n_to_run - 1),".rds"))


start_row <- 41
n_to_run <- 20
pars <- all_pars[start_row:(start_row + n_to_run - 1), ]
res1 <- list()
start_time <- Sys.time()
print(start_time)
for (i in 1:n_to_run){
  pars1<-pars[i,]
  res1[[i]] = run_many_PDA(nproj = pars1$nproj, my_budget_perc = pars1$budget, my_selprob = pars1$my_selprob, my_alpha = pars1$my_alpha, 
                           random_nested = pars1$random_nested, interaction_pool = pars1$interaction_pool, my_gamma = pars1$my_gamma,
                           neg_int_nint_multiplier = pars1$neg_int_nint_multiplier, neg_int_BC_multiplier = pars1$neg_int_BC_multiplier,
                           bp_type = pars1$bp_type)
  print(paste("Combination",i))
}

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time )
res <- res1 %>% bind_rows()


saveRDS(res, file = paste0("output/PDA_simulation_results_",
                           start_row,"_",(start_row + n_to_run - 1),".rds"))

start_row <- 61
n_to_run <- 20
pars <- all_pars[start_row:(start_row + n_to_run - 1), ]
res1 <- list()
start_time <- Sys.time()
print(start_time)
for (i in 1:n_to_run){
  pars1<-pars[i,]
  res1[[i]] = run_many_PDA(nproj = pars1$nproj, my_budget_perc = pars1$budget, my_selprob = pars1$my_selprob, my_alpha = pars1$my_alpha, 
                           random_nested = pars1$random_nested, interaction_pool = pars1$interaction_pool, my_gamma = pars1$my_gamma,
                           neg_int_nint_multiplier = pars1$neg_int_nint_multiplier, neg_int_BC_multiplier = pars1$neg_int_BC_multiplier,
                           bp_type = pars1$bp_type)
  print(paste("Combination",i))
}

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time )
res <- res1 %>% bind_rows()


saveRDS(res, file = paste0("output/PDA_simulation_results_",
                           start_row,"_",(start_row + n_to_run - 1),".rds"))


start_row <- 81
n_to_run <- 20
pars <- all_pars[start_row:(start_row + n_to_run - 1), ]
res1 <- list()
start_time <- Sys.time()
print(start_time)
for (i in 1:n_to_run){
  pars1<-pars[i,]
  res1[[i]] = run_many_PDA(nproj = pars1$nproj, my_budget_perc = pars1$budget, my_selprob = pars1$my_selprob, my_alpha = pars1$my_alpha, 
                           random_nested = pars1$random_nested, interaction_pool = pars1$interaction_pool, my_gamma = pars1$my_gamma,
                           neg_int_nint_multiplier = pars1$neg_int_nint_multiplier, neg_int_BC_multiplier = pars1$neg_int_BC_multiplier,
                           bp_type = pars1$bp_type)
  print(paste("Combination",i))
}

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time )
res <- res1 %>% bind_rows()


saveRDS(res, file = paste0("output/PDA_simulation_results_",
                           start_row,"_",(start_row + n_to_run - 1),".rds"))
