################################################################################
#
# Application 1: Global Warming
#
################################################################################

library(dplyr)
library(ggplot2)

# CIRLS development version
cirlsdir <- "C:/Users/PierreMasselot/OneDrive - London School of Hygiene and Tropical Medicine/Research/Projects/CIRLS/Subprojects/cirls"
devtools::load_all(cirlsdir)

#------------------------
# Data preparation
#------------------------

# Read data
warming <- read.csv("data/warming.csv", header = TRUE)

# Compute decade
warming <- mutate(warming, decade = 10 * floor(year / 10))

# Transform as factors for regression
warming <- mutate(warming,
  fyear = factor(year), 
  fdec = factor(decade))

#------------------------
# Fit non-decreasing regression
#------------------------

#----- Full non-parametric regression

# Build constraint matrix
yCmat <- diff(diag(nlevels(warming$fyear)))

# Fit model
yfit <- glm(anomaly ~ 0 + fyear, data = warming, 
  method = "cirls.fit", Cmat = yCmat)

# Extract predictions
warming <- mutate(warming, ypred = predict(yfit))

## No inference possible because the model is saturated

# We can nonetheless look at "observed" degrees of freedom
edf(yfit)
logLik(yfit, df = "o") |> AIC(yfit)

#----- Decadal regression

# Constraint matrix
dCmat <- diff(diag(nlevels(warming$fdec)))

# Fit model
dfit <- glm(anomaly ~ 0 + fdec, data = warming, method = "cirls.fit",
  Cmat = dCmat)

# Extract predictions
warming <- mutate(warming, dpred = predict(dfit))

# Extract coefs and confidence intervals
coefdf <- data.frame(dec = unique((warming$dec)) + 5, 
  beta = coef(dfit), confint(dfit))

# EDF and AIC
edf(dfit)
AIC(dfit)

#------------------------
# Plot
#------------------------

# Plot observed data
ggplot(warming) + theme_bw() + 
  geom_point(aes(x = year, y = anomaly), size = 3, alpha = .3, stroke = NA) + 
  geom_pointrange(aes(x = dec, y = beta, ymin = low, ymax = high), size = 1,
    linewidth = 1.2, data = coefdf) +
  geom_line(aes(x = year, y = ypred), col = 2, linewidth = .9) + 
  geom_point(aes(x = year, y = ypred), col = 2) + 
  labs(y = "Temperature anomaly", x = "Year")

# Save
ggsave("figures/figApp1_warming.pdf")
