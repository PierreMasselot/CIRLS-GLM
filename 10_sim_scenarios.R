################################################################################
#
# Simulation study
# 1. Attach packages and define simulation scenarios
#
################################################################################

#-------------------
# Packages
#-------------------

# I use the librarian package to easily attach
library(librarian)

# Define the list of needed packages
packlist <- c(
  # Main analysis packages
  "cirls", "MASS",
  
  # Tidyverse mostly for dplyr and ggplot2
  "tidyverse",
  
  # Packages to loop across simulations
  "foreach", "rngtools", "doFuture", "iterators",
  
  # Useful packages for plotting
  "scico", "patchwork",  "scales"
)

# Attach (or install if needed)
librarian::shelf(packlist)

#-------------------
# Parameters & scenarios
#-------------------

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

#----- Data-generating mechanism 1: nonnegative regression

dgmlist[["nonneg"]] <- list(
  
  # Label for plotting
  lab = "Non-negative regression",
  
  # Main function: generates data and fits models
  generate = function(n, s2, par, covar = 1, xrho = .5){
    
    #- Data generation -
    # Get dimensions
    p1 <- length(par); p2 <- length(covar)
    p <- p1 + p2
    
    # Generate standardised (but correlated) predictors
    xvmat <- matrix(xrho, p, p); diag(xvmat) <- 1
    x <- mvrnorm(n, rep(0, p), xvmat)
    xmain <- x[, seq_len(p1), drop = F]
    xcovar <- x[, p1 + seq_len(p2), drop = F]
    
    # Generate response vector
    eta <- 5 + xmain %*% par + xcovar %*% covar
    y <- rnorm(n, eta, sqrt(s2))
    
    #- Models -
    fitlist <- list(
      # Fit CIRLS: main covariate nonnegative
      cirls = glm(y ~ xmain + xcovar, method = "cirls.fit", 
        constr = ~ shape(xmain, "pos")),
      
      # Fit unconstrained GLM
      glm = glm(y ~ xmain + xcovar)
    )
    
    #- Return -
    list(true = c(5, par, covar), fit = fitlist)
  }
)

#----- Data-generating mechanism 2: Poisson distributed non-decreasing strata
  
dgmlist[["nondecr"]] <- list(
  
  # Label for plotting
  lab = "Non-decreasing strata",
  
  # Main function: generates data and fits models
  generate = function(n, s2, par, int = 0, p = 5){
    
    #- Data generation -
    
    # Predictor is a factor
    x <- factor(sample.int(p, n, replace = T))
    
    # Linear predictor (strata) and response vector
    strata <- par * (plogis(seq(-1, 1, length.out = p), scale = 1/s2)) + int
    y <- rpois(n, exp(strata[x]))
    
    #- Models -
    fitlist <- list(
      # Fit CIRLS: nondecreasing
      cirls = glm(y ~ 0 + x, family = "poisson", method = "cirls.fit", 
        constr = ~ shape(x, "inc")),
      
      # Fit unconstrained GLM
      glm = glm(y ~ 0 + x, family = "poisson")
    )
    
    #- Return -
    list(true = strata, fit = fitlist)
  }
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

