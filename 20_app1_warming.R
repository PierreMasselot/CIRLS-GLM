################################################################################
#
# Application 1: Global Warming
#
################################################################################

library(tidyverse)
library(cirls)

#------------------------
# Data preparation
#------------------------

# Read data
warming <- read.csv("data/warming.csv", header = TRUE)

# Transform as factors for regression
warming <- mutate(warming, fyear = factor(year))

#------------------------
# Isotonic regression
#------------------------

#----- Full non-parametric regression

# Fit model
yfit <- glm(anomaly ~ fyear, data = warming, method = "cirls.fit",
  constr = ~ shape(fyear, "inc"))

# Extract predictions
warming <- mutate(warming, ypred = predict(yfit))

# No inference possible because the model is saturated
# We can nonetheless look at "observed" degrees of freedom
edfres <- edf(yfit, seed = 1111)

#----- Plot predictions

ggplot(warming) + theme_bw() + 
  # Observed anomalies
  geom_point(aes(x = year, y = anomaly, col = "Observed"), size = 3, 
    alpha = .3, stroke = NA) + 
  # Non-decreasing predicted ones
  geom_line(aes(x = year, y = ypred, col = "Estimated"), linewidth = .9) +
  geom_point(aes(x = year, y = ypred, col = "Estimated"), size = 2) +
  # Theme
  scale_color_manual(values = c("Observed" = 1, "Estimated" = 2), name = "",
    breaks = c("Observed", "Estimated"),
    guide = guide_legend(position = "inside")) +
  labs(y = "Temperature anomaly (°C)", x = "Year") +
  theme(legend.position.inside = c(0.15, .90), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15))

# Save
ggsave("figures/fig4_warming.png")

