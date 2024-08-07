####################
####################
### Function: create_interdependencies 
### Inputs:   starting_proj,n_int_proj,order_int_proj,use_ipp_str
### Author:   Ian Durbach (indurbach@gmail.com)
### Update:   2/2/2016
###
### Creates the project interdepencies, either in a "structured" or "random way
### 0) First, note that the desired number of kappa-way interdependencies is 
###    given in the vector "n_int_proj". The maximum value of kappa is given 
###    by nint. Say n_int_proj = c(1,3,5,6,10) and nint = 6. This results in 
###    1 x 6-way interaction, 3 x 5-way, 5 x 4-way, 6 x 3-way, 10 x 2-way.
###    Note that the first element of n_int_proj gives the number of interactions
###    of order nint -- there will only ever by one (nint choose nint). The 
###    remaining elements are *always* associated with 5-, 4-, 3-, and 2-way 
###    interactions.
### 1) Start by randomly sampling order_int_proj[1] projects from the full set. 
###    This set constitutes the largest interdependency in both "structured" and
###    "random" interdependencies.
### 2) "Structured" interdependencies: to generate a new interaction involving 
###     kappa projects, randomly choose one of the interactions involving (kappa
###     + 1)-projects, and randomly delete one of the projects. This ensures a 
###     simple *nested* structure in the interdependencies i.e. lower-order 
###     interactions only appear between projects that also interact at higher
###     levels. This means sequential approaches to portfolio construction will
###     probably do ok.
### 3)  "Random" interdependencies: this simply generates a kappa-way interaction
###     by randomly drawing from the set of nint positively-related projects.
###     Thus interdependencies at one level may have little relation to those at 
###     higher levels.
### 4)  Checks are in place to ensure each interdependency is unique.
####################
####################

create_interdependencies = function(starting_proj,n_int_proj,order_int_proj,use_ipp_str){
  
  int_proj = starting_proj
  
  # start by generating the largest interdependency
  interacting_proj = c()
  interaction_order = order_int_proj[1]
  interacting_proj[[1]] = matrix(sample(starting_proj,interaction_order),nrow=interaction_order)

  # repeat the above procedure until you have the required number of kappa-way
  # interdependencies
  j = 0
  while(ncol(interacting_proj[[1]])<n_int_proj[1]){
    # bind to the existing ones
    interacting_proj[[1]] = cbind(interacting_proj[[1]],
                                  matrix(sample(starting_proj,interaction_order),nrow=interaction_order))
    # sort projects in ascending order
    interacting_proj[[1]] = apply(interacting_proj[[1]],2,sort)
    # check that no interdependencies are repeated
    interacting_proj[[1]] = unique(interacting_proj[[1]],MARGIN=2)
    j = j + 1
    # if you ask for impossible numbers of interdependencies you get an error
    if(j == 100) stop("couldn't create project interdependencies")
  }
  
  if(use_ipp_str==1){
    
    ############
    ### Create network of "structured" interacting projects
    ############
    
    for(i in 2:length(n_int_proj)){
      # get the order of the current interaction (i.e. number of projects involved)
      interaction_order = order_int_proj[i]
      # choose one of the interdepencies at random from the previous step
      choose_col = sample(1:ncol(interacting_proj[[i-1]]),1000,replace=T)
      # compute how many projects to exclude and exclude at random
      n_to_excl = order_int_proj[i-1] - order_int_proj[i]
      excl_row = matrix(0,nrow=n_to_excl,ncol=1000)
      for(j in 1:1000){
        excl_row[,j] = sample(1:nrow(interacting_proj[[i-1]]),n_to_excl)    
      }
      # the result is the new interdependency
      interacting_proj[[i]] = matrix(interacting_proj[[i-1]][-excl_row[,1],choose_col[1]],ncol=1)
      j = 2
      # repeat the above procedure until you have the required number of kappa-way
      # interdependencies
      while(ncol(interacting_proj[[i]])<n_int_proj[i]){
        # bind to the existing ones
        interacting_proj[[i]] = cbind(interacting_proj[[i]],
                                          interacting_proj[[i-1]][-excl_row[,j],choose_col[j]])
        # sort projects in ascending order
        interacting_proj[[i]] = apply(interacting_proj[[i]],2,sort)
        # check that no interdependencies are repeated
        interacting_proj[[i]] = unique(interacting_proj[[i]], MARGIN=2)
        j = j + 1
        # if you ask for impossible numbers of interdependencies you get an error
        if(j == length(excl_row)) stop("couldn't create project interdependencies")
      }
    }
    interacting_proj
    
  }else{
    
    ############
    ### Create network of "random" interacting projects
    ############
    
    for(i in 2:length(n_int_proj)){
      # get the order of the current interaction (i.e. number of projects involved)
      interaction_order = order_int_proj[i]
      # randomly choose an interdependency from the set of nint projects
      interacting_proj[[i]] = matrix(sample(starting_proj,interaction_order),ncol=1)
      j = 2
      # repeat the above procedure until you have the required number of kappa-way
      # interdependencies
      while(ncol(interacting_proj[[i]]) < n_int_proj[i]){
        # bind to the existing ones
        interacting_proj[[i]] = cbind(interacting_proj[[i]],
                                           sample(starting_proj,interaction_order))
        # sort projects in ascending order
        interacting_proj[[i]] = apply(interacting_proj[[i]],2,sort)
        # check that no interdependencies are repeated
        interacting_proj[[i]] = unique(interacting_proj[[i]],MARGIN=2)
        j = j + 1
        # if you ask for impossible numbers of interdependencies you get an error
        if(j == 100) stop("couldn't create project interdependencies")
      }
    }
    interacting_proj
    
  }  
  
  return(interacting_proj)
}
