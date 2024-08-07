####################
####################
### Function: solve_portfolio
### Inputs:   ipp,order_int_proj,bp,Bi,cp,Ci,budget
### Author:   Ian Durbach (indurbach@gmail.com)
### Update:   5/2/2016
###
### Solves portfolio optimization problem as a MILP
####################
####################

solve_portfolio = function(ipp,order_int_proj,bp,Bi,cp,Ci,budget,max=TRUE){
  
  n_int_proj = c()
  
  for(i in length(ipp):1)
  {
    n_int_proj[i] = ncol(ipp[[i]])
  }
  #browser()
  K = sum(n_int_proj) # number of interactions
  n = length(bp) # number of projects
  
  # a: indicator variables assigning projects to interactions
  k = 1
  a = matrix(0,nrow=K,ncol=n)
  for(i in 1:length(n_int_proj))
  {
    for(j in 1:n_int_proj[i])
    {
      a[k,ipp[[i]][,j]] = 1
      k = k + 1
    }
  }
  
  # objective function
  obj = c(bp,Bi)
  
  # cost constraint (budget constraint)
  mat1 = c(cp,Ci)
  dir1 = c("<=")
  rhs1 = budget 
  
  # cost constraint: if solving a min problem for nadir portfolio, 
  # require at least 95% of budget is spent (else nothing implemented)
  if(max == FALSE){
    mat1n = c(cp,Ci)
    dir1n = c(">=")
    rhs1n = 0.95*budget
    
    mat1 = rbind(mat1,mat1n)
    dir1 = rbind(dir1,dir1n)
    rhs1 = rbind(rhs1,rhs1n)
  }

  # interaction constraints (lower)
  mat2 = cbind(a,-n*diag(K))
  dir2 = rep("<=",K)
  rhs2 = rep(order_int_proj,times=n_int_proj) - 1
  
  # interaction constraints (upper)
  mat3 = cbind(a,-n*diag(K))
  dir3 = rep(">=",K)
  rhs3 = rep(order_int_proj,times=n_int_proj) - n
  
  # all constraints
  mat = rbind(mat1,mat2,mat3)
  dir = c(dir1,dir2,dir3)
  rhs = c(rhs1,rhs2,rhs3)
  
  # other Rsymphony options
  types = rep("B",n+K)
  
  # solve
  #output = Rsymphony_solve_LP(obj, mat, dir, rhs, types = types, max = max)
  if(max){
    direction = "max"
  }else{
    direction = "min"
  }
  #lp = lp(objective.in = obj, const.mat = mat, all.int = T, all.bin = T, const.dir = dir, const.rhs = rhs, direction = direction)
  #lp = Rglpk_solve_LP(obj = obj, mat = mat, dir = dir, rhs = rhs, max = max, types = rep("B",length(obj)))
  lp = Rglpk_solve_LP(obj = obj, mat = mat, dir = dir, rhs = rhs, max = max, types = rep("B",length(obj)))
  #print(lp)
  lp$solution = lp$solution  
  lp$objval = lp$optimum
  return (lp)
  #return(lp)
}
