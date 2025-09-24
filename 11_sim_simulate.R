################################################################################
#
# Simulation study
# 2. Perform simulations
#
################################################################################

#-------------------
# Loop across scenarios to perform simulations
#-------------------

#----- Initialise

# Intialise progress
if(!dir.exists("temp")) dir.create("temp")
cat("sc,i", "\n", file = "temp/trace_sim.txt")

# Plan parallelisation
plan(multisession)
futpars <- list(seed = 1234, packages = packlist)

#----- Loop 
simures <- foreach(sc = iter(scenarios, by = "row"), .combine = rbind,
    .options.future = futpars) %:%
  foreach(i = icount(nsim), .combine = rbind,
    .options.future = modifyList(futpars, list(seed = TRUE))) %dofuture%
{
  
  # devtools::load_all("C:\\Users\\PierreMasselot\\OneDrive - London School of Hygiene and Tropical Medicine\\Research\\Projects\\CIRLS\\Subprojects\\cirls")

  
  #----- Generate data and fit models
  dgmfun <- dgmlist[[sc$dgm]]$generate
  dgmres <- do.call(dgmfun, sc[c("n", "s2", "par")])
  
  #----- Extract results
  modres <- lapply(dgmres$fit, function(modfit){
    
    # Extract coefficients
    coefs <- coef(modfit)
    
    # Extract variances
    vars <- diag(vcov(modfit))
    
    # Confidence intervals
    suppressMessages(cis <- confint(modfit))
    colnames(cis) <- c("low", "high")
    
    # Degrees of freedom
    dfsim <- edf(modfit)[-1]
    
    # Put into data.frame
    data.frame(sim = i, sc = sc$sc, coef = seq_along(coefs) - 1, 
      true = dgmres$true, est = coefs, v = vars, cis, t(dfsim))
  })
  
  #----- Tidy and return
  
  # Trace
  if ((i %% 100) == 0) cat(format(Sys.time(), "%m-%d %X"), 
    "scenario: ", sc$sc, " - iteration ", i,  "\n",
    file = "temp/trace_sim.txt", append = T)
  
  # bind model results and return
  bind_rows(modres, .id = "mod")
}

# Save results
save(simures, file = "temp/simures.RData")