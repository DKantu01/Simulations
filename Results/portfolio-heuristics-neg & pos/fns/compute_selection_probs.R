####################
####################
### Function: compute_selection_probs
### Inputs:   selprobs,bp,cp
### Author:   Ian Durbach (indurbach@gmail.com)
### Update:   5/2/2016
###
### Generates selection probabilities for initial choice of interacting 
### alternatives. Can be equal ("equal"), proportional to bp/cp ("prop"),
### or inversely proportional to cp/bp ("invprop").
####################
####################

compute_selection_probs = function(selprobs,bp,cp){
  
  n = length(bp)
  
  if(selprobs=='equal'){
    probs = rep(1,n)
  } else if(selprobs=='prop'){
    probs = bp/cp
  } else if(selprobs=='invprop'){
    probs = cp/bp
  } else {
    stop("check selection probability condition")
  }
  
  return(probs)
  
}