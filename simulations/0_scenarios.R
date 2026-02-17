################################################################################
#
# Simulation study
# 1. Attach packages and define simulation scenarios
#
################################################################################

# I use the librarian package to easily attach
library(librarian)

# Define the list of needed packages
packlist <- c(
  # Main analysis packages
  "cirls", "MASS", 
  
  # Tidyverse and other convenience packages
  "tidyverse", "abind",
  
  # Packages to loop across simulations
  "foreach", "rngtools", "doFuture", "iterators",
  
  # Useful packages for plotting
  "scico", "patchwork",  "scales"
)

# Attach (or install if needed)
librarian::shelf(packlist)

################################################################################
# Parameters & scenarios

#----- General parameters

# Number of simulations
nsim <- 1000

# Common parameters
parlist <- list(
  n = c(500), # Sample size
  s2 = c(50), # Noise level
  par = seq(-1, 1, by = .1) # Feasibility parameter
)

# Initialise lists containing data generating mechanisms
dgmlist <- list()

#--------------------
# Data-generating mechanism 1: nonnegative regression
#--------------------

dgmlist[["nonneg"]] <- list(
  
  # Label for plotting
  lab = "Non-negative regression",
  
  # Specification of true coefficients,
  specBeta = function(par, pc = 1){
    list(int = 5, slopes = c(par, rep(1, pc)))
  },
  
  # Generate predictor matrix
  genX = function(n, p, xrho = .5){
    xvmat <- matrix(xrho, p, p)
    diag(xvmat) <- 1
    mvrnorm(n, rep(0, p), xvmat)
  },
  
  # Generate response vector
  genY = function(X, Beta, s2){
    if (Beta$int) X <- cbind(1, X)
    eta <- X %*% unlist(Beta)
    rnorm(NROW(X), eta, sqrt(s2))
  },
  
  # Fit cirls model: positive constraint on first coefficien
  fitcirls = function(y, X){
    Cmat <- t(c(0, 1, rep(0, ncol(X) - 1)))
    glm(y ~ X, method = "cirls.fit", Cmat = Cmat, lb = 0, ub = Inf)
  },
  
  # Fit benchmark GLM model
  fitglm = function(y, X) glm(y ~ X)
)

#--------------------
# Data-generating mechanism 2: Poisson distributed non-decreasing strata
#--------------------
  
dgmlist[["nondecr"]] <- list(
  
  # Label for plotting
  lab = "Non-decreasing strata",
  
  # Specification of true coefficients,
  specBeta = function(par, p = 5){
    betas <- par * (plogis(seq(-1, 1, length.out = p), scale = 1/50))
    list(slopes = betas - min(betas))
  },
  
  # Generate predictor matrix
  genX = function(n, p) factor(sample.int(p, n, replace = T)),
  
  # Generate response vector
  genY = function(X, Beta, s2){
    eta <- Beta$slopes[X] #+ log(sqrt(s2))
    rpois(NROW(X), exp(eta))
  }, 
  
  # Fit cirls model: non-negative strata
  fitcirls = function(y, X){
    glm(y ~ X - 1, method = "cirls.fit", constr = ~ shape(X, "inc"),
      family = "poisson")
  },
  
  # Fit unconstrained GLM model
  fitglm = function(y, X) glm(y ~ X - 1, family = "poisson")
)

#-------------------
# Prepare scenarios
#-------------------

# Expand scenarios, including the common parameters
scenarios <- expand.grid(c(parlist, list(dgm = names(dgmlist))))

# Add scenario id
scenarios <- cbind(sc = 1:NROW(scenarios), scenarios)

# Number of total scenarios
nsc <- NROW(scenarios)
