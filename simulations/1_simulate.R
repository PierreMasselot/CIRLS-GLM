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
  .errorhandling = "pass", .options.future = futpars) %dofuture%
{
  
  # Let's add a time limit so that it fails after some time
  setTimeLimit(elapsed = 10000)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  
  # Get the current state of RNG
  rngstate <- .Random.seed
  
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
    
    # cnt <- 0
    
    #----- Fit and extract results for all generated y vectors
    simfit <- apply(Y, 2, simplify = F, FUN = function(y){
      
      # cat(cnt <<- cnt + 1, "")
      
      # Fit model
      fit <- do.call(dgmlist[[sc$dgm]][[f]], list(y = y, X = X))
      
      # Coefficients
      coefs <- coef(fit)
      
      # Change depending on the type of model
      if (inherits(fit, "cirls")){
        
        # Simulate first and then compute 
        sims <- simulCoef(fit, nsim = 10000)
        v <- diag(vcov(sims))
        ci <- confint(sims)
      } else {
        
        # Use usual methods (tryCatch is because profiling can fail)
        v <- diag(vcov(fit))
        ci <- tryCatch(suppressMessages(confint(fit)), 
          error = function(e) matrix(NA, length(coefs), 2))
        colnames(ci) <- c("low", "high")
      }
      
      # Expected degrees of freedom
      dfs <- edf(fit)[-1]
      
      # Predictions (to compute true complexity)
      yhat <- predict(fit)
      
      # Return everything
      list(fit = fit, coefs = coefs, v = v, ci = ci, dfs = dfs, yhat = yhat)
    })
    
    # Initialise performance results
    coefres <- data.frame(coef = seq_along(simfit[[1]]$coef), 
      true = betavec)
    
    #----- Estimation of performances
    
    # Extract coefficients
    coefs <- sapply(simfit, "[[", "coefs")
    
    # Compute error measures: bias, SE and MSE
    coefres <- mutate(coefres, 
      meanest = rowMeans(coefs, na.rm = T),
      bias = meanest - betavec,
      empse = apply(coefs, 1, sd, na.rm = T),
      mse = rowMeans((coefs - betavec)^2, na.rm = T)
    )

    #----- Precision performances
    
    # Extract variances
    vars <- sapply(simfit, "[[", "v")
    
    # Extract whether CIs cover coefficients
    incl <- sapply(simfit, \(x) between(betavec, x$ci[,1], x$ci[,2]))
    inclmean <- sapply(simfit, 
      \(x) between(coefres$meanest, x$ci[,1], x$ci[,2]))
    
    # Compute performances: se error, coverage and bias-corrected coverage
    coefres <- mutate(coefres,
      modse = sqrt(rowMeans(vars, na.rm = T)),
      seerr = 100 * ((modse / empse) - 1),
      cover = rowMeans(incl, na.rm = T),
      becover = rowMeans(inclmean, na.rm = T)
    )
    
    #----- Degrees of freedom
    
    # Get dispersion
    disp <- simfit[[1]]$fit$family$dispersion
    
    # Estimated degrees of freedom
    dfs <- sapply(simfit, "[[", "dfs")
    
    # "True" degrees of freedom
    yhat <- sapply(simfit, "[[", "yhat")
    den <- ifelse(is.na(disp), sc$s2, disp)
    truedf <- sum(diag(cov(t(Y), t(yhat)))) / den
    
    # Initialise data.frame to store df results
    dfres <- data.frame(df = c("o", "e"),
      true = truedf,
      mean = rowMeans(dfs) - is.na(disp)
    )
    
    # Compute performance results
    dfres <- mutate(dfres,
      bias = mean - true,
      se = apply(dfs, 1, sd, na.rm = T),
      mse = rowMeans((dfs - truedf)^2, na.rm = T)
    )
    
    #----- Return both data.frames
    list(coefs = coefres, dfs = dfres)
  })
  
  #----- Put together and return
  
  # Right labels
  names(modres) <- modlabs
  
  # Bind data.frames
  boundres <- list_transpose(modres) |> lapply(bind_rows, .id = "model") |>
    lapply(remove_rownames)
  
  # Trace
  cat("Completed scenario", sc$sc, "-", format(Sys.time(), "%Y-%m-%d %X"), "\n",
    file = "simulations/trace.txt", append = T)
  
  # Return
  c(list(sc = sc, rngstate = as.data.frame(t(rngstate))), boundres)
}

#----- Save everything

save(simures, file = "simulations/results.RData")

# Prepare results folder
respath <- "simulations/results"
unlink(respath, recursive = TRUE)
dir.create(respath, recursive = T)

# Loop through elements to save in csvs

list_transpose(simures) |> 
  imap(\(res, lab) bind_rows(res, .id = "sc") |> 
      write.csv(file = sprintf("%s/%s.csv", respath, lab), 
      row.names = F, quote = F))

