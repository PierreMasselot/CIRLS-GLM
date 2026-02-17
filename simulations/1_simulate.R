################################################################################
#
# Simulation study
# 2. Perform simulations
#
################################################################################

#--------------------
# Initialise and loop over scenarios
#--------------------

# Intialise progress
cat("Starting simulations: ", format(Sys.time(), "%Y-%m-%d %X"), "\n", 
  file = "simulations/trace.txt")


# Plan parallelisation
plan(multisession)
futpars <- list(seed = 1234, packages = packlist)

# Loop 
simures <- foreach(sc = iter(scenarios, by = "row"), 
  .options.future = futpars) %dofuture%
{
  
  #--------------------
  # Generate data
  #--------------------
  
  # Create the true coefficient vector
  Beta <- dgmlist[[sc$dgm]]$specBeta(sc$par)
  betavec <- unlist(Beta)
  
  # Predictor matrix
  X <- dgmlist[[sc$dgm]]$genX(sc$n, length(Beta$slopes))
  
  # nsim response vectors
  Y <- replicate(nsim, dgmlist[[sc$dgm]]$genY(X, Beta, sc$s2))
  
  #--------------------
  # Fit models and extract results
  #--------------------
  
  #----- Fit all available models
  
  # List of models available
  modfuns <- grep("fit", names(dgmlist[[sc$dgm]]), value = T)
  modlabs <- gsub("fit", "", modfuns)
  
  # Go through models to fit
  modres <- lapply(modfuns, function(f){
    
    # Fit on all generated y vectors
    simfit <- apply(Y, 2, function(y) do.call(dgmlist[[sc$dgm]][[f]], 
      list(y = y, X = X)))
    
    #----- Estimation performances
    
    # Extract coefficients
    coefs <- sapply(simfit, coef)
    
    # Bias
    meanest <- rowMeans(coefs)
    bias <- meanest - betavec
    
    # Standard error
    empse <- apply(coefs, 1, sd)
    
    # MSE
    mse <- rowMeans((coefs - betavec)^2)
    
    #----- Precision performances
    
    # Extract variances and compute variance error
    vars <- sapply(simfit, function(x) diag(vcov(x)))
    modse <- sqrt(rowMeans(vars))
    seerr <- 100 * ((modse / empse) - 1)
    
    # Confidence intervals and coverage
    cis <- suppressMessages(lapply(simfit, confint) |> abind(along = 3))
    cover <- rowMeans(apply(cis, 3, function(x) between(betavec, x[,1], x[,2])))
    becover <- rowMeans(apply(cis, 3, 
      function(x) between(meanest, x[,1], x[,2])))
    
    #----- Degrees of freedom
    
    # Get dispersion
    disp <- simfit[[1]]$family$dispersion
    
    # Estimated degrees of freedom
    dfs <- sapply(simfit, edf)[-1,]
    dfmean <- rowMeans(dfs) - is.na(disp)
    # dfsum <- apply(dfs, 1, quantile, probs = c(0, .25, .5, .75, 1), na.rm = T)
    
    # "True" degrees of freedom
    yhat <- sapply(simfit, predict)
    den <- ifelse(is.na(disp), sc$s2, disp)
    truedf <- sum(diag(cov(t(Y), t(yhat)))) / den
    
    # Error in degrees of freedom
    dfbias <- dfmean - truedf
    dfse <- apply(dfs, 1, sd)
    dfmse <- rowMeans((dfs - truedf)^2)
    
    #----- Return everything
    list(bias = bias, empse = empse, mse = mse, 
      seerr = seerr, cover = cover, becover = becover,
      dfmean = dfmean, truedf = truedf, 
      dfbias = dfbias, dfse = dfse, dfmse = dfmse)
  })
  
  
  #----- Put together and return
  
  # Trace
  cat("Completed scenario", sc$sc, "-", format(Sys.time(), "%Y-%m-%d %X"), "\n",
    file = "simulations/trace.txt", append = T)
  
  # Return
  sc$lab <- dgmlist[[sc$dgm]]$lab
  names(modres) <- modlabs
  c(list(sc = sc), modres)
}

# Save results
save(simures, file = "simulations/results.RData")