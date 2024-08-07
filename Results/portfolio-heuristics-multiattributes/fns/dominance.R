#projects = cbind(x$bp, -x$cp)
#projects = cbind(-x$cp, x$bp)

#simple <- paretoSimple(projects)
#cumulative <- paretoCumulative(projects)
#table(simple)
#table(cumulative)

#table(simple & cumulative)

dominates_ <- function(x, y, order = c()){
  if(length(x) != length(y))
    stop("Vectors of different length")
  
  dom = 0;
  domPossible = 1;
  for (i in (1:length(x))) {
    if(x[i] > y[i]){
      dom = 1
    }else if(x[i] < y[i]){
      domPossible = 0
    }
  }
  if(dom == 1 & domPossible == 1){
    return (1)
  }
  return (0)
}

dominates <- function(x, y, order = c()){
  if(length(x) != length(y))
    stop("Vectors of different length")
  xdelta = x - y
  positive = xdelta > 0
  negative = xdelta < 0
  if(TRUE %in% positive && !(TRUE %in% negative)){
    return (1)
  }
  return (0)
}


paretoSimple <- function(x){
  dominated <- array(1, dim = nrow(x))
  for(i in (1:nrow(x))){
    for(j in (i:nrow(x))){
      if(dominates(x[i,],x[j,]) == 1){
        dominated[j] <- 0
      }
      else if(dominates(x[j,],x[i,]) == 1){
        dominated[i] <- 0
      }
    } 
  }
  return(dominated)
}

cumDominates <- function(x, y, order){
  if(length(x) != length(y))
    stop("Vectors of different length")
  
  xcumprofile = cumsum(c(x[order]))
  ycumprofile = cumsum(c(y[order]))
  dominates(xcumprofile, ycumprofile)
}

paretoCumulative <- function(x){
  dominated <- array(1, dim = nrow(x))
  for(i in (1:nrow(x))){
    for(j in (i:nrow(x))){
      if(cumDominates(x[i,],x[j,]) == 1){
        dominated[j] <- 0
      }
      else if(cumDominates(x[j,],x[i,]) == 1){
        dominated[i] <- 0
      }
    } 
  }
  return(dominated)
}



construct_dombased_portfolios <- function(nRP = 100, nCV, ipp, nor_bp, bp, Bi, nor_cp,cp, Ci, budget, calculateDomPrevalence = F){
  n = length(bp)
  final_z = matrix(0,nrow=nRP,ncol=n)
  benefit = c() 
  cost = c() 
  feasible = c()
  pareto_counts = list()
  g = matrix(0,nrow=nRP,ncol=length(Bi))
  k = 1
  while(k <= nRP){
    my_propsol = domBased(nCV, ipp, nor_bp,bp, Bi, nor_cp,cp, Ci, budget, calculateDominancePrevalence = calculateDomPrevalence)
    final_z[k,] = my_propsol$final_z
    benefit[k] = my_propsol$benefit 
    cost[k] = my_propsol$cost 
    feasible[k] = my_propsol$feasible
    g[k,] = my_propsol$g
    pareto_counts[[k]] <- my_propsol$pareto_simple_count
    k = k+1
  }
 
  return(list(final_z=final_z,benefit=benefit,cost=cost,feasible=feasible,g=g, benefit_bare = my_propsol$benefit_bare, pareto_counts = pareto_counts))
}

construct_cumdombased_portfolios <- function(nRP = 100, nCV, ipp, nor_bp, bp, Bi, nor_cp,cp, Ci, budget, cueOrder){
  n = length(bp)
  final_z = matrix(0,nrow=nRP,ncol=n)
  benefit = c() 
  cost = c() 
  feasible = c()
  g = matrix(0,nrow=nRP,ncol=length(Bi))
  k = 1
  while(k <= nRP){
    my_propsol = domBased(nCV, ipp, nor_bp,bp, Bi, nor_cp,cp, Ci, budget, domFunction = cumDominates, order = cueOrder)
    final_z[k,] = my_propsol$final_z
    benefit[k] = my_propsol$benefit 
    cost[k] = my_propsol$cost 
    feasible[k] = my_propsol$feasible
    g[k,] = my_propsol$g 
    k = k+1
  }
  
  return(list(final_z=final_z,benefit=benefit,cost=cost,feasible=feasible,g=g))
}

#Test:
#debug(cueValidity)
#cueValidity(cost = c(10,13,14,15),value = c(5,5,2,10),10)
#calculates validity of cost and value when predicting cost/value ratio in pairwise comparisons.
cueValidity <- function(cost, value, budget){
  cv_ratio = cost/value
  pairwise <- pairwiseComparisons(cbind(cost, -value, cv_ratio))
  validities <- c()
  for(i in seq(1,2)){
    validityNum <- sum(ifelse((pairwise[,i] * pairwise[,3] == 0) 
                              |(pairwise[,i] * pairwise[,3]) / abs(pairwise[,i] * pairwise[,3]) != 1, 0, 1))
    validityDem <- length(pairwise)
    validity = validityNum/validityDem
    validities[i] = validity
  }
  validities
}

#Picks a project randomly from the pareto simple set. 
#It constructs the set based on three cues:
# 1. Value (+) 1
# 2. Cost (-) 2
# 3. Is involved in Positive Interaction (+)
# 4. Return (Value/Cost) (+)
# Is in budget (+)
domBased = function(nCV,ipp,nor_bp,bp,Bi,nor_cp,cp,Ci,budget, domFunction = dominates, cue_order = c(1,2,3), calculateDominancePrevalence = F){
  cue_order = cue_order + 1
  t_budget = budget
  remaining_budget = budget
  n = length(bp)
  proj <- as.data.frame(cbind(idx = seq(1,n),bp,-cp))
  proj_nor <- as.data.frame(cbind(idx = seq(1,n),nor_bp,-nor_cp))
  proj$in_budget <- ifelse(remaining_budget - proj$V3 > 0,1,0)
  
  proj_nor$involved_in_positive = ifelse(positiveInteractions(proj, ipp, Bi) > 0, 1, 0)
  proj_nor$positive_interactions = positiveInteractions(proj, ipp, Bi)
  proj_nor$return = proj$bp/(-proj$V3)
  proj_nor$in_budget = proj$in_budget
  proj_nor$dominated <- rep(0, n)
  paretoSimpleCt <- c()
  proj_nondom <- proj_nor
  my_z <- rep(0,n)
  cv = 0 #consecutive constraint violations
  if(calculateDominancePrevalence) paretoSimpleCt <- c(paretoSimpleCt, (sum(paretoSimple(proj_nor[, c(2,3)]))))
  
  proj_nondom <- subset(proj_nondom, proj_nondom$in_budget == 1)
  while(cv < nCV){
    validProposal  = 0
    proj$in_budget <- ifelse(remaining_budget - proj$V3 > 0,1,0)
    proj_nor$in_budget = proj$in_budget
    proj_nondom <- proj_nor
    
    while(validProposal != 1){
      #check that proposed is not dominated:
      proj_nondom <- subset(proj_nor, dominated == 0)
      proposed_id = sample(proj_nondom$idx, 1)
      validProposal  = 1
      #proj_nor[proj_nondom$idx,c(2,3)] - proj_nor[proposed_id,c(2,3)]
      delta <-  proj_nor[proj_nondom$idx,cue_order] - proj_nor[rep(proposed_id,nrow(proj_nor[proj_nondom$idx,cue_order])),c(2,3,4)]
      positive = delta > 0
      negative = delta < 0
      positive <- positive[,1] | positive[,2] | positive[,3]
      negative <- negative[,1] | negative[,2] | negative[,3]
      dominates <- !negative & positive
      if((TRUE %in% dominates)){ ## is it dominated?
        proj_nor[proposed_id,"dominated"] = 1 #proposed is marked as dominated
        validProposal = 0
      }
    }
    
    proj_nor$dominated <- rep(0, n)
    my_z[proposed_id] = 1
    proj_nor[proposed_id,"dominated"] = 1
    
    # evaluate proposed portfolio
    my_propsol = evaluate_z(z = my_z, ipp = ipp, bp  = bp, Bi = Bi, 
                            cp = cp, Ci = Ci, budget = budget)
    
    if(my_propsol$feasible == 0){
      cv = cv +1
      my_z[proposed_id] = 0
      proj_nor$dominated <- (my_z)
    }else{
      if(calculateDominancePrevalence){
        if(sum(my_z) > 0){
          paretoSimpleCt <- c(paretoSimpleCt, (sum(paretoSimple(proj_nor[-which(my_z == 1), c(2,3)]))))
        }else{
          paretoSimpleCt <- c(paretoSimpleCt, (sum(paretoSimple(proj_nor[, c(2,3)]))))
        }
      }
      cv = 0
      proj_nor$dominated <- (my_z)
      remaining_budget = remaining_budget + proj[proposed_id,3]
    }
  }
  final_z = my_z
  final_res = evaluate_z(z = my_z, ipp = ipp, bp  = bp, Bi = Bi, 
                         cp = cp, Ci = Ci, budget = budget, decompose = T) 
  benefit = final_res$benefit 
  cost = final_res$cost 
  feasible = final_res$feasible
  g = final_res$g 
  #print(final_z)
  return(list(final_z=final_z,benefit=benefit,cost=cost,feasible=feasible,g=g, benefit_bare = final_res$benefit_bare, pareto_simple_count = paretoSimpleCt))
}

#returns the number of interactions in which proj participates
positiveInteractions <- function(proj, ipp, Bi){
  numInteractions = rep(0,length(proj$idx))
  cnt <- 1
  for (i in c(1:length(ipp))){
    if(Bi[cnt] > 0){
      involved = ifelse(proj$idx %in% ipp[[i]], 1, 0)
      numInteractions = numInteractions + involved 
    }
    cnt <- cnt + ncol(ipp[[i]])
  }
  return(numInteractions)
}


pairwiseComparisons <- function(x){
  idx  = 1
  comp <- matrix(0, nrow = ((nrow(x))*(nrow(x)-1)), ncol = ncol(x))
  for(i in (1:nrow(x))){
    for(j in (1:nrow(x))){
      if(i != j){
        comp[idx ,] <- (x[i,] - x[j,])
        idx = idx + 1
      }
    } 
  }
  return (comp)
}



