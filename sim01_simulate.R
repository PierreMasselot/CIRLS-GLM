################################################################################
#
# Simulations
#
################################################################################

# List of packages
packages <- c("MASS", "foreach", "ggplot2", "rngtools", "dplyr", "doParallel", 
  "devtools", "scico", "patchwork")
lapply(packages, library, character.only = T) |> invisible()

# Dev version of cirls
# cirlsdir <- "C:/Users/lshpm4/OneDrive - London School of Hygiene and Tropical Medicine/Research/Projects/CIRLS/Subprojects/cirls"
cirlsdir <- "C:/Users/PierreMasselot/OneDrive - London School of Hygiene and Tropical Medicine/Research/Projects/CIRLS/Subprojects/cirls"
allfiles <- list.files(sprintf("%s/R", cirlsdir), full.names = T)
lapply(allfiles, source) |> invisible()

#-------------------
# Parameters
#-------------------

#----- Simulation parameters

# Number of simulations
nsim <- 500

# Number of observations
ns <- c(500, 1000, 5000)

# Error standard deviation
sigmas <- 3

#----- Data-generating mechanisms

dgmlist <- list(
  
  # First scenario: very simple nonnegative regression
  nonneg = list(
    lab = "Non-negative regression",
    datafun = function(n, p, sigma){
      x <- mvrnorm(n, rep(0, p), diag(p))
      beta <- rep(0.5, p + 1)
      y <- rpois(n, exp(cbind(1, x) %*% beta))
      list(x = x, y = y, beta = beta)
    },
    modfuns = list(
      uncons = function(x, y) glm(y ~ x, family = "poisson"),
      cons = function(x, y){
        Cmat <- shapeConstr(x, shape = "pos")
        glm(y ~ x, family = "poisson", method = "cirls.fit", 
          Cmat = list(x = Cmat), lb = c(0.4, 0.5))
      },
      over = function(x, y){
        Cmat <- shapeConstr(x, shape = "pos")
        glm(y ~ x, family = "poisson", method = "cirls.fit", 
          Cmat = list(x = Cmat), lb = c(0.5, 0.6))
      }
    )
  ),
  
  # Second scenario, increasing strata means (Oliva-Avilès et al. 2019)
  survey = list(
    lab = "Non-decreasing strata",
    datafun = function(n, p, sigma){
      x <- factor(sample.int(p, n, replace = T))
      beta <- plogis(seq(-p/2, p/2, length.out = p), scale = p/20) - .5
      y <- rnorm(n, beta[x], sigma)
      list(x = x, y = y, beta = beta)
    },
    modfuns = list(
      uncons = function(x, y) glm(y ~ 0 + x),
      cons = function(x, y){
        Cmat <- diff(diag(nlevels(x)))
        glm(y ~ 0 + x, method = "cirls.fit", Cmat = Cmat)
      },
      over = function(x, y){
        p <- nlevels(x)
        Cmat <- rbind(diff(diag(nlevels(x))), diag(nlevels(x)))
        Cmat <- Cmat[-checkCmat(Cmat)$redundant,]
        glm(y ~ 0 + x, method = "cirls.fit", Cmat = Cmat)
      }
    )
  )
)

#-------------------
# Simulate
#-------------------

#----- Prepare all senarios

# Data-generating scenarios
dgmsc <- data.frame(dgm = names(dgmlist), p = c(2, 5))
dgmsc <- cbind(dgmsc[rep(seq_along(dgmlist), c(1, length(sigmas))),], 
  sigma = c(1, sigmas))

# Full factorial design with n
scenarios <- expand.grid(n = ns, dgm = 1:nrow(dgmsc))
scenarios <- cbind(subset(scenarios, select = -dgm), dgmsc[scenarios$dgm,])
nsc <- nrow(scenarios)

#----- Prepare simulations

# Prepare seeds, also working in parallel
rnglist <- replicate(nsc, RNGseq(nsim, 1111), simplify = F)

# Prepare parallelisation
ncores <- pmax(detectCores() - 2, 1)
cl <- makeCluster(ncores)
registerDoParallel(cl)
writeLines(c(""), "temp/trace.txt")

# Loop across simulations
simures <- foreach(sc = iter(scenarios, by = "row"), rngsc = rnglist,
    .packages = packages, .combine = rbind) %:%
  foreach(i = seq(nsim), rng = rngsc, .combine = rbind,
    .packages = packages) %dopar%
{
  
  # Trace
  if (i == 1) cat(as.character(Sys.time()), paste(sc, collapse = ", "), "\n",
    file = "temp/trace.txt", append = T)
  
  # Source cirls functions
  lapply(allfiles, source) |> invisible()
  
  #----- Generate data
  
  # Get data-generating mechanism
  dgm <- dgmlist[[sc$dgm]]
  
  # Set seed
  setRNG(rng)
  
  # Generate this iteration of data
  data <- dgm$datafun(sc$n, sc$p, sc$sigma)
  
  #----- Fit models
  
  # Fit all models
  modfit <- lapply(dgm$modfuns, do.call, list(x = data$x, y = data$y))
  
  # Extracts results from models
  results <- foreach(fit = modfit, lab = names(modfit), .combine = rbind) %do% {
    
    # Coefficients
    coefs <- coef(fit)
    
    # Variances
    vars <- diag(vcov(fit))
    
    # Confidence intervals
    cis <- confint(fit)
    colnames(cis) <- c("low", "high")
    
    # Degrees of freedom and AIC
    dfs <- edf(fit)[-1]
    aic0 <- fit$aic
    aicE <- AIC(fit)
    
    # Put everything together
    data.frame(model = lab, coef = seq_along(coefs) - 1, true = data$beta, 
      est = coefs, v = vars, cis, t(dfs), aic0 = aic0, aicE = aicE)
  }
  
  # Add info about iteration and return
  cbind(sc, sim = i, results)
}

stopCluster(cl)

# Save results
save(simures, file = "temp/simures.RData")


