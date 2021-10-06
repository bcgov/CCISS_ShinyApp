## Kiri Daust, July 2021
## Implimentation of portfolio analysis using nloptr for optimisation


#' Optimise Portfolio
#' @param returns A data.table of tree growth
#' @param cov_matrix A correlation matrix of trees
#' @param boundDat A data.table of bounds for each species
#' @param minTot A double; minimum acceptable weight
#' @details The function uses the nloptr library to optimise variance over the efficient frontier
#' @return A data.table with optimised weights across the frontier, the variance, and the return
#' @import data.table
#' @author Kiri Daust
#' @export
optimise_portfolio <- function(returns, cov_matrix, boundDat, minTot){
  
  # Declare binding for checks
  if (FALSE) {
    Spp <- sharpe <- NULL
  }
  
  spp <- colnames(cov_matrix)
  sppUse <- spp
  mean_returns <- colMeans(returns)
  bndNew <- boundDat[Spp %chin% spp,] 
  
  ##remove low weight species and re-optimise
  while(length(mean_returns) > 1){
    ##maybe temp
    target <- .set_target(mean_returns,cov_matrix)
    testRet <- target[1]+0.5 ## test both ends of the frontier
    testPort <- .efficient_return(mean_returns,cov_matrix,testRet,bndNew)$par
    if(any(testPort < minTot)){
      sppUse <- sppUse[testPort > minTot]
      cov_matrix <- cov_matrix[sppUse,sppUse]
      mean_returns <- mean_returns[sppUse]
      bndNew <- bndNew[Spp %chin% sppUse,]
    }else{
      break
    }
  }
  
  target <- .set_target(mean_returns,cov_matrix)
  frontier_weights <- setnames(data.table(matrix(nrow = 0, ncol = length(sppUse))),sppUse)
  frontier_sd <- numeric(length = length(target))
  for(i in 1:length(target)){
    result <- .efficient_return(mean_returns, cov_matrix, target[i], bndNew)
    frontier_weights <- rbind(frontier_weights,t(as.matrix(result$par)),use.names = F)
    frontier_sd[i] <- result$value
  }
  w_df <- cbind(frontier_weights,frontier_sd,return = target)
  min_risk <- target[1]
  w_df[,sharpe := (return - min_risk)/frontier_sd]
  return(w_df)
}

#' @importFrom nloptr slsqp
#' @import data.table
#' @noRd
.set_target <- function(mean_returns, cov_matrix){
  num_ass <- length(mean_returns)
  eq_constr <- function(x){
    sum(x) - 1
  } 
  min_var_w <- slsqp(x0 = rep(1/num_ass,num_ass),fn = .portfolio_volatility,
                     lower = rep(0,num_ass), upper = rep(1,num_ass),
                     heq = eq_constr, mean_returns = mean_returns, cov_matrix = cov_matrix)
  min_var <- .portfolio_return(min_var_w$par,mean_returns)
  target <- seq(min_var,max(mean_returns),length.out = 20)
  return(target)
}

#' @importFrom nloptr slsqp
#' @import data.table
#' @noRd
.efficient_return <- function(mean_returns, cov_matrix, target_ret, bounds){
  num_ass <- length(mean_returns)
  ##set constraints
  eq_constr_ret <- function(x){
    temp <- c(sum(mean_returns*x)-target_ret,sum(x)-1)
    if(any(is.na(temp))) return(NULL)
    return(temp)
  }
  start <- rep(1/num_ass,num_ass)
  start <- pmin(start,bounds$maxWt)
  start <- pmax(start,bounds$minWt)
  result <- slsqp(x0 = start,fn = .portfolio_volatility,
                  lower = bounds$minWt, upper = bounds$maxWt,
                  heq = eq_constr_ret, mean_returns = mean_returns, 
                  cov_matrix = cov_matrix)
  return(result)
}

#' @import data.table
#' @noRd
.portfolio_volatility <- function(weights, mean_returns, cov_matrix){
  return(t(weights) %*% cov_matrix %*% weights)
}

#' @import data.table
#' @noRd
.portfolio_return <- function(weights, mean_returns){
  return(sum(weights*mean_returns))
}