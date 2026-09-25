################################################################################
#
# Main analysis script
#
################################################################################

#-----------------------
# Package management
#-----------------------

# Load all packages
source("packages.R")

# Potentially update packages
# p_update()

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

# Case study of temperature
source("case studies/1_temperature.R")


