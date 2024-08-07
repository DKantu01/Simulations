### The goal of this script is to give an example of running the PDA simulation.
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

# set up parameters for simulation (see paper for details)
nproj <- 50
budgets <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
my_alpha <- c(0, 6)
my_gamma <- c(0, 0.5)
my_selprob <- c("equal","prop","invprop")
interaction_pool <- c(10)
random_nested = c(0, 1)
neg_int_nint_multiplier <- c(0.5, 1.5)
neg_int_BC_multiplier <- -c(0.5, 1.5)
bp_type <- c("uni", "pos", "neg")

# make a data frame with all parameter combinations
pars = expand.grid(nproj= nproj, budget = budgets, selprob = my_selprob, alpha = my_alpha, 
                   random_nested = random_nested, interaction_pool = interaction_pool, 
                   gamma = my_gamma, neg_int_nint_multiplier = neg_int_nint_multiplier,
                   neg_int_BC_multiplier = neg_int_BC_multiplier, bp_type = bp_type)

# run_PDA once
set.seed(123)
irun = 1 # select parameter set
res = run_PDA(nproj = pars$nproj[irun], 
              my_budget_perc = pars$budget[irun],
              my_alpha = pars$alpha[irun],
              my_selprob = pars$selprob[irun],
              random_nested = pars$random_nested[irun], 
              interaction_pool = pars$interaction_pool[irun],
              my_gamma = pars$gamma[irun],
              # size of neg synergies are some multiple of pos synergies 
              # must be <= 0 (=0 for no negative interactions)
              # number of neg synergies are some multiple of pos synergies,
              # kind of neg interactions are as for pos, 
              # neg synergy projects involve a new set of projects
              neg_int_nint_multiplier = pars$neg_int_nint_multiplier[irun],
              neg_int_BC_multiplier = pars$neg_int_BC_multiplier[irun],
              bp_type = pars$bp_type[irun])

# run_PDA many times (as in a simulation study)
set.seed(123)
irun = 1
res <- run_many_PDA(nproj = pars$nproj[irun], 
                    my_budget_perc = pars$budget[irun],
                    my_alpha = pars$alpha[irun],
                    my_selprob = pars$selprob[irun],
                    random_nested = pars$random_nested[irun], 
                    interaction_pool = pars$interaction_pool[irun],
                    my_gamma = pars$gamma[irun],
                    # size of neg synergies are some multiple of pos synergies 
                    # must be <= 0 (=0 for no negative interactions)
                    # number of neg synergies are some multiple of pos synergies,
                    # kind of neg interactions are as for pos, 
                    # neg synergy projects involve a new set of projects
                    neg_int_nint_multiplier = pars$neg_int_nint_multiplier[irun],
                    neg_int_BC_multiplier = pars$neg_int_BC_multiplier[irun],
                    bp_type = pars$bp_type[irun])

