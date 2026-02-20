################################################################################
#
# Main analysis script
#
################################################################################

#-----------------------
# renv
#-----------------------

# Uses renv to store information about package environment used

# To use exactly the right packages, run the following command
renv::restore()

#-----------------------
# Simulations
#-----------------------

# Define scenarios
source("simulations/0_scenarios.R")

# Run simulations - long code, can be traced in simulations/trace.txt
source("simulations/1_simulate.R")

# Create plots - saved in figures/
source("simulations/2_plots.R")

# Create the appendix plots
source("simulations/3_appendix.R")

#-----------------------
# Case studies
#-----------------------

# First case study: global warming
rm(list = ls())
source("case studies/1_warming.R")

# Second: life expectancy
rm(list = ls())
source("case studies/2_lifexp.R")

#-----------------------
# renv
#-----------------------

# Take a snapshot of the current library (if everything works)
renv::snapshot()
