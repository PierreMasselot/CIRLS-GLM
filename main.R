################################################################################
#
# Main analysis script
#
################################################################################

#-----------------------
# Package management
#-----------------------

# Managed with pacman
library(pacman)

# Potentially update packages
# p_update()

# Load all packages
source("packages.R")

# Save version
ver <- sapply(c("cirls", packlist), \(x){
  sprintf("%s: %s", x, paste(p_version(x), collapse = "."))
})
writeLines(ver, "packageVersions.txt")

#-----------------------
# Simulations
#-----------------------

# Define scenarios and simulate: writes results
source("simulations/0_scenarios.R")
source("simulations/1_simulate.R")

# Produce plots: select scenario
nsel <- 500
source("simulations/2_plots.R")
source("simulations/3_appendix.R")


#-----------------------
# Case studies
#-----------------------

# Get the list of applications
appdir <- "case studies"
appscripts <- list.files(appdir, pattern = ".R$")

# Go through it
for (x in appscripts){
  rm(list = setdiff(ls(), c("appscripts", "appdir", "x")))
  print(x)
  source(paste0(appdir, "/", x))
}


