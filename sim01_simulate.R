################################################################################
#
# Simulations
#
################################################################################

# List of packages
packages <- c("tidyverse", "MASS", "foreach", "rngtools", "doFuture", 
  "devtools", "scico", "patchwork", "iterators", "progressr", "scales",
  "mixmeta")
lapply(packages, library, character.only = T) |> invisible()

# Dev version of cirls
cirlsdir <- "../../cirls"
allfiles <- list.files(sprintf("%s/R", cirlsdir), full.names = T)
lapply(allfiles, source) |> invisible()
cirlsfct <- list.files(sprintf("%s/R", cirlsdir)) |> 
  gsub(pattern = ".R", repl = "", fixed = TRUE)

# EDF function test
source("temp/edf2.r")
source("temp/edfboot.r")

#-------------------
# Parameters & scenarios
#-------------------

#----- General parameters

# Number of simulations
nsim <- 1000

# Common parameters
parlist <- list(
  n = c(500), # Sample size
  s2 = c(50) # Noise level
)

# Initialise lists
lablist <- dgmlist <- fitlist <- sclist <- list()

#----- Data-generation mechanism 1: nonnegative regression

# # Label for this mechanism
# lablist[["nonneg"]] <- "Non-negative regression"
# 
# # Function to generate data: sample size, error (determines intercept), 
# #     true coefficient (constrained and covariates)
# dgmlist[["nonneg"]] <- function(n, s2, par, covar){
#   beta <- c(par, covar)
#   p <- length(beta)
#   x <- mvrnorm(n, rep(0, p), diag(p))
#   eta <- 5 + x %*% beta
#   # y <- rpois(n, exp(eta))
#   y <- rnorm(n, eta, sqrt(s2))
#   list(x = x, y = y, beta = c(s2, beta))
# }
# 
# # Function to fit model: response, predictors, number of non-neg constraints
# fitlist[["nonneg"]] <- list(
#   cirls = function(x, y){
#     p <- ncol(x)
#     Cmat <- cbind(0, diag(1), matrix(0, 1, p - 1))
#     # glm(y ~ x, family = "poisson", method = "cirls.fit", Cmat = Cmat, lb = 0)
#     glm(y ~ x, method = "cirls.fit", Cmat = Cmat, lb = 0)
#   },
#   glm = function(y, x){
#     # glm(y ~ x, family = "poisson")
#     glm(y ~ x)
#   }
# )
# 
# # Scenarios
# sclist[["nonneg"]] <- list(par = c(-1, 0, 1), covar = 1, m = 1)

#----- Data-generation mechanism 1b: nonnegative regression correlated variables

# Label for this mechanism
lablist[["nonneg_cor"]] <- "Non-negative regression"

# Function to generate data: sample size, error (determines intercept), 
#     true coefficient (constrained and covariates)
dgmlist[["nonneg_cor"]] <- function(n, s2, par, covar){
  beta <- c(par, covar)
  p <- length(beta)
  x <- mvrnorm(n, rep(0, p), xpndMat(c(1, .5, 1)))
  eta <- 5 + x %*% beta
  # y <- rpois(n, exp(eta))
  y <- rnorm(n, eta, sqrt(s2))
  list(x = x, y = y, beta = c(5, beta))
}

# Function to fit model: response, predictors, number of non-neg constraints
fitlist[["nonneg_cor"]] <- list(
  cirls = function(x, y){
    p <- ncol(x)
    Cmat <- cbind(0, diag(1), matrix(0, 1, p - 1))
    # glm(y ~ x, family = "poisson", method = "cirls.fit", Cmat = Cmat, lb = 0)
    glm(y ~ x, method = "cirls.fit", Cmat = Cmat, lb = 0)
  },
  glm = function(y, x){
    # glm(y ~ x, family = "poisson")
    glm(y ~ x)
  }
)

# Scenarios
sclist[["nonneg_cor"]] <- list(par = seq(-1, 1, by = .1), covar = 1, m = 1)


#----- Data-generation mechanism 2: Poisson non-decreasing strata
  
# Label for this mechanism
lablist[["survey_fish"]] <- "Non-decreasing strata"

# Function to generate data: sample size, error (determines intercept), 
#     number of strata
dgmlist[["survey_fish"]] <- function(n, s2, int, p, par){
  x <- factor(sample.int(p, n, replace = T))
  strata <- par * (plogis(seq(-1, 1, length.out = p), scale = 1/s2)) + int
  y <- rpois(n, exp(strata[x]))
  list(x = x, y = y, beta = strata)
}

# Function to fit model: response, predictors, number of non-neg constraints
fitlist[["survey_fish"]] <- list(
  cirls = function(x, y){
    p <- nlevels(x)
    Cmat <- diff(diag(p))
    glm(y ~ 0 + x, family = "poisson", method = "cirls.fit", Cmat = Cmat)
  },
  glm = function(y, x){
    glm(y ~ 0 + x, family = "poisson")
  }
)

# Scenarios
sclist[["survey_fish"]] <- list(par = seq(-1, 1, by = .1), p = 5, int = 0)

#----- Data-generation mechanism 3: non-decreasing strata, but Gaussian
# 
# # Label for this mechanism
# lablist[["survey"]] <- "Non-decreasing strata"
# 
# # Function to generate data: sample size, error (determines intercept), 
# #     number of strata
# dgmlist[["survey"]] <- function(n, s2, int, p, par){
#   x <- factor(sample.int(p, n, replace = T))
#   strata <- par * (plogis(seq(-1, 1, length.out = p), scale = 1/s2)) + int
#   y <- rnorm(n, strata[x], sqrt(s2))
#   list(x = x, y = y, beta = strata)
# }
# 
# # Function to fit model: response, predictors, number of non-neg constraints
# fitlist[["survey"]] <- list(
#   cirls = function(x, y){
#     p <- nlevels(x)
#     Cmat <- diff(diag(p))
#     glm(y ~ 0 + x, method = "cirls.fit", Cmat = Cmat)
#   },
#   glm = function(y, x){
#     glm(y ~ 0 + x)
#   }
# )
# 
# # Scenarios
# sclist[["survey"]] <- list(par = c(-1, 0, 1), p = 5, int = 0)


#-------------------
# Simulate
#-------------------

#----- Prepare all senarios

# Expand scenarios, including the common parameters
scenarios <- lapply(sclist, function(x) expand.grid(c(parlist, x)))

# Put together
scenarios <- bind_rows(scenarios, .id = "dgm")
scenarios <- cbind(sc = 1:NROW(scenarios), scenarios)

# Number of total scenarios
nsc <- NROW(scenarios)

#----- Loop across scenarios and iterations

# Intialise progress
cat("sc,i", "\n", file = "temp/trace_sim01.txt")

# Plan parallelisation
plan(multisession)
futpars <- list(seed = 1234, packages = packages,
  globals = c(cirlsfct, "dgmlist", "fitlist", "prog", "nsc", "nsim", "edf2", "edfboot"))

# Loop across simulations
simures <- foreach(sc = iter(scenarios, by = "row"), .combine = rbind,
    .options.future = futpars) %:%
  foreach(i = icount(nsim), .combine = rbind,
    .options.future = modifyList(futpars, list(seed = TRUE))) %dofuture%
{
  
  # Generate this iteration of data
  dgm <- dgmlist[[sc$dgm]]
  dgmpars <- sc[formalArgs(dgm)]
  data <- do.call(dgm, dgmpars)
  
  #----- Fit models and extract results
  modres <- lapply(fitlist[[sc$dgm]], function(f){
    
    # Fit
    modfit <- f(x = data$x, y = data$y)
    
    # Extract coefficients
    coefs <- coef(modfit)
    
    # Extract variances
    vars <- diag(vcov(modfit))
    
    # Confidence intervals
    suppressMessages(cis <- confint(modfit))
    colnames(cis) <- c("low", "high")
    
    # Degrees of freedom and AIC
    dfsim <- edf(modfit)[-1]
    dfboot <- edfboot(modfit)[3]
    
    # This AIC uses the basic degrees of freedom
    aic0 <- logLik(modfit, df = "odf") |> AIC()
    aicE <- logLik(modfit, df = "edf") |> AIC()
    
    # Put into data.frame
    data.frame(sim = i, sc = sc$sc, 
      coef = seq_along(coefs) - 1, true = data$beta, 
      est = coefs, v = vars, cis, 
      t(dfsim), edfboot = dfboot, aic0 = aic0, aicE = aicE)
  })
  
  # Trace
  if ((i %% 100) == 0) cat(format(Sys.time(), "%m-%d %X"), 
    "scenario: ", sc$sc, " - iteration ", i,  "\n",
    file = "temp/trace_sim01.txt", append = T)
  
  # Tidy and return
  bind_rows(modres, .id = "mod")
}

# Save results
save(simures, file = "temp/simures.RData")


