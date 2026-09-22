################################################################################
#
# Packages
#
################################################################################

# At the moment, use the development version in GitHub
pacman::p_load_gh("PierreMasselot/cirls")

# Define the list of needed packages
packlist <- c(
  # Main analysis packages
  "MASS", "dlnm", "tsModel", "splines",
  
  # Tidyverse and other convenience packages
  "tidyverse", "abind",
  
  # Packages to loop across simulations
  "foreach", "rngtools", "doFuture", "iterators",
  
  # Useful packages for plotting
  "scico", "patchwork",  "scales"
)

# Attach (or install if needed)
pacman::p_load(packlist, character.only = TRUE)
