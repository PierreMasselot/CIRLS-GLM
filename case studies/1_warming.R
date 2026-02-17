################################################################################
#
# Application 1: Global Warming
#
################################################################################

library(tidyverse)
library(cirls)
library(scico)
library(splines)

#------------------------
# Data preparation
#------------------------

# Data is included in the cirls package

# Create short periodic strata as a factor
perlen <- 5
warming <- mutate(warming, 
  period = perlen * floor(year / perlen),
  fperiod = factor(period))

#------------------------
# Perform the regression models
#------------------------

#----- Monotone regression

# Fit model
monofit <- glm(anomaly ~ fperiod - 1, data = warming, method = "cirls.fit",
  constr = ~ shape(fperiod, "inc"))

# Extract levels and confidence intervals for each year
coefres <- cbind(coef = coef(monofit),  confint(monofit))
monores <- coefres[warming$fperiod,]

# Extract degrees of freedom
edf(monofit, seed = 1111)

#----- Unconstrained stratified regression for comparison

# Fit the corresponding unconstrained model 
stratfit <- uncons(monofit)

# Extract levels and confidence intervals
coefres <- cbind(coef = coef(stratfit),  confint(stratfit))
stratres <- coefres[warming$fperiod,]

#----- Increasing splines

# Create spline basis
splbasis <- ns(warming$year, df = 10)

# Fit model
monosplfit <- glm(anomaly ~ splbasis, data = warming, method = "cirls.fit",
  constr = ~ shape(splbasis, "inc"))

# Get prediction variance
X <- model.matrix(monosplfit)
v <- diag(X %*% vcov(monosplfit) %*% t(X))

# Put prediction and CI together
pred <- predict(monosplfit)
msplres <- cbind(coef = pred, low = pred - 1.96 * sqrt(v), 
  high = pred + 1.96 * sqrt(v))

# Extract degrees of freedom
edf(monosplfit, seed = 2222)

#----- Unconstrained splines

# Update model
usplfit <- uncons(monosplfit)

# Get prediction variance
v <- diag(X %*% vcov(usplfit) %*% t(X))

# Put prediction and CI together
pred <- predict(usplfit)
usplres <- cbind(coef = pred, low = pred - 1.96 * sqrt(v), 
  high = pred + 1.96 * sqrt(v))

#------------------------
# Plot
#------------------------

#----- Put all results together

# Number of observations
n <- nrow(warming)

# Bind results
allres <- rbind(monores, stratres, msplres, usplres) 

# Add info
resdf <- data.frame(type = factor(rep(c("Strata", "Splines"), each = 2 * n),
    levels = c("Strata", "Splines"), 
    labels = sprintf("%s) %s", letters[1:2], c("Strata", "Splines"))), 
  cons = factor(rep(rep(c("Constrained", "Unconstrained"), each = n), 2),
    levels = c("Unconstrained", "Constrained")), 
  year = rep(warming$year, 4), allres)

#----- Plot

ggplot(resdf, aes(x = year)) + theme_bw() + 
  
  # Split panel by model
  facet_wrap(~ type, axes = "all", axis.labels = "all") +
  
  # Observed anomalies
  geom_point(aes(x = year, y = anomaly), size = 3, alpha = .3, stroke = NA, 
    data = warming, inherit.aes = F) + 

  # Predictions and confidence intervals
  geom_ribbon(aes(ymin = low, ymax = high, fill = cons), alpha = .5,
    data = ~ subset(.x, cons == "Constrained")) +
  geom_line(aes(y = coef, col = cons), linewidth = 1) + 
  
  # Theme
  scale_color_scico_d(name = "", palette = "batlow", end = .7, direction = -1) +
  scale_fill_scico_d(name = "", palette = "batlow", end = .7, direction = -1, 
    drop = F) +
  labs(y = "Temperature anomaly (°C)", x = "") +
  theme(legend.position.inside = c(0.15, .90), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = 17, hjust = 0))

# Save
ggsave("figures/fig4.pdf", width = 11)

