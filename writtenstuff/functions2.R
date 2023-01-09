
fit_logistic_lasso <- function(x, y, lambda, beta0 = NULL, eps = 0.0001, iter_max = 100){
  
  ## This function minimizes the negative log-likelihood plus a L1 norm penalty term for
  ## logistic regression using coordinate descent and IRLS. It will return the coefficients that will 
  ## minimize it and they can be used to classify future data.
  ## 
  ## Input:
  ## -x: A matrix that contains observations as rows and this must not include the intercept
  ##     term (column of 1's)
  ##
  ## -y: A factor that contains the binary response. So for example, factor(c(1,0,1)).
  ##
  ## -lambda: The penalty to take into account
  ##
  ## -beta0: Vector initial guess of what the coefficients are including the intercept.
  ##         Default is all 0's if the user does not enter any initial guess.
  ##
  ## -eps: Parameter for stopping criterion, so when should the algorithm stop.
  ##
  ## -iter_max: The maximum number of iterations that the algorithm can run.
  ##
  ## Output:
  ## - A list that contains the beta and intercept that minimizes the negative log-likelihood
  ##   with a L1 norm penalty. The list also has factor levels and the penalty amount. 
  ##
  ## Example:
  ##
  ## x <- matrix_of_predictors
  ## y <- factor(c(1,0,0,1,0,1,0,1,0,0,0,1,1,0,0,1))
  ## 
  ## ret <- fit_logistic_lasso(x, y, lambda = 0.3)
  ##
  
  n <- dim(x)[1]
  num_pred <- dim(x)[2]
  
  if (is.null(beta0)){
    updated_intercept_term <- 0
    updated_beta <- rep(0, num_pred)
    } else {
      updated_intercept_term <- beta0[1]
      updated_beta <- beta0[-1]
    }
  
  fct_levels <- levels(y)
  y <- as.numeric(y) - 1
  
  intercept_term <- updated_intercept_term
  beta <- updated_beta
  
  for(i in 1:iter_max){
    linear_func <- (x %*% beta + intercept_term) %>% as.numeric()
    p <- 1 / (1 + exp(-linear_func))
    w <- p * (1-p)
    z <- linear_func + (y - p) / w
    sqrt_w <- sqrt(w)
    
    combined_beta_old <- c(intercept_term, beta)
    
    for(j in 1:500){
      
      for(k in 1:num_pred){
        rj = (sqrt_w*z) - (intercept_term*sqrt_w) - sqrt_w * (as.matrix(x[,-k])%*%beta[-k])
        beta[k] <- sign(t(rj)%*%(sqrt_w*(x[,k])))*max(abs(t(rj)%*%(sqrt_w*(x[,k])))- n*lambda,0) / sum((sqrt_w*(x[,k]))^2)
      }
      
      rj2 <- (sqrt_w*z) - (sqrt_w * (x %*% beta))
      intercept_term <- ((t(rj2) %*% sqrt_w) / sum(sqrt_w^2)) %>% as.vector()
      
      if(abs(intercept_term-updated_intercept_term) < eps & max(abs(beta-updated_beta)) < eps){
        combined_beta <- c(intercept_term, beta)
        break
      }
      
      combined_beta <- c(intercept_term, beta)
      updated_beta <- beta
      updated_intercept_term <- intercept_term
        
    }
    
    beta <- beta %>% as.numeric()
    intercept_term <- intercept_term %>% as.numeric()
    names(beta) <- colnames(x)
    names(intercept_term) <- c("Intercept")
    
    if(sqrt(as.numeric(crossprod(combined_beta - combined_beta_old))) < eps){
      return(list(intercept = intercept_term, beta = beta, 
                  lambda = lambda, fct_levels = fct_levels))
    }
    
  }
  
  warning(paste("Algorithm might not have converged in", iter_max, "iterations", sep = " "))
  
  return(list(intercept = intercept_term, beta = beta, lambda = lambda, fct_levels = fct_levels))
  
}


predict_logistic_lasso <- function(fit, new_x){
  
  ## This function takes the output from fit_logistic_lasso() which is the beta and intercept
  ## and uses these parameters to predict or classify new data. It will predict which class multiple
  ## observations are from, not just one.
  ##
  ## Input:
  ## - fit: A list, which must be the output from fit_logistic_lasso() function.
  ##
  ## - new_x: A matrix of new observations to predict at. Observations are rows.
  ##
  ## Output:
  ## - A factor of 0 and 1's which are the predicted class of each observation in new_x. 
  ##
  ## Example:
  ## new_x <- matrix_of_new_data
  ## 
  ## ret <- fit_logistic_lasso(x, y, lambda = 0.3)
  ## predict <- predict_logistic_lasso(ret, new_x) 
  ##
  
  intercept_term <- fit$intercept
  beta <- fit$beta
  
  numeric_pred <- as.numeric(intercept_term + new_x %*% beta >= 0)
  
  return(fit$fct_levels[numeric_pred + 1] %>% factor)
  
}




