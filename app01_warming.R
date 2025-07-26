################################################################################
#
# Application 1: Global Warming
#
################################################################################

library(tidyverse)

# CIRLS development version
cirlsdir <- "C:/Users/PierreMasselot/OneDrive - London School of Hygiene and Tropical Medicine/Research/Projects/CIRLS/Subprojects/cirls"
devtools::load_all(cirlsdir)

source("temp/edfboot.r")

#------------------------
# Data preparation
#------------------------

# Read data
warming <- read.csv("data/warming.csv", header = TRUE)

# Compute indicator for longer periods
perlen <- 5
warming <- mutate(warming, period = perlen * floor(year / perlen))

# Transform as factors for regression
warming <- mutate(warming,
  fyear = factor(year), 
  fper = factor(period))

#------------------------
# Fit non-decreasing regression
#------------------------

#----- Full non-parametric regression

# Build constraint matrix
yCmat <- diff(diag(nlevels(warming$fyear)))

# Fit model
yfit <- glm(anomaly ~ fyear, data = warming, method = "cirls.fit",
  constr = ~ shape(fyear, "inc"))

# Extract predictions
warming <- mutate(warming, ypred = predict(yfit))

## No inference possible because the model is saturated

# We can nonetheless look at "observed" degrees of freedom
# Doesn't work in the Bootstrap one
# edfres <- edfboot(yfit, seed = 1111)
# logLik(yfit, df = "o") |> AIC(yfit)

#----- Fit models

# Fit unconstrained model - No intercept for easier interpretation of coefficients
ufit <- glm(anomaly ~ 0 + fper, data = warming)

# Constraint matrix
Cmat <- diff(diag(nlevels(warming$fper)))

# Constrained model
cfit <- glm(anomaly ~ 0 + fper, data = warming, method = "cirls.fit",
  Cmat = Cmat)

# Neither of the two work well
# dfit <- glm(anomaly ~ 0 + fdec, data = warming, method = "cirls.fit",
#   constr = ~ shape(fdec, "inc", int = T))
# dfit <- glm(anomaly ~ 0 + fdec, data = warming, method = "cirls.fit",
#   constr = ~ shape(fdec, "inc"))

# Extract predictions
# warming <- mutate(warming, pred = predict(cfit))

#----- Extract coefficients

coefdf <- data.frame(
  
  # Compute period location
  summarise(warming, perloc = mean(year), .by = period),
  
  # For the unconstrained model
  ubeta = coef(ufit), 
  confint(ufit) |> as.data.frame() |> setNames(c("ulow", "uhigh")),
  
  # For the constrained model
  cbeta = coef(cfit), 
  confint(cfit) |> as.data.frame() |> setNames(c("clow", "chigh"))
)

#----- Extract EDf and AIC

# EDF
edfres <- edfboot(cfit, seed = 1111)

# AICs
AIC(ufit)
logLik(cfit, df = "e") |> AIC()

# #----- Penalised regression (just as a test, not in paper)
# 
# # Penalization matrix
# Dmat <- shapeConstr(warming$fyear, shape = "cvx")
# 
# # Extract data
# augy <- c(yfit$y, rep(0, nrow(Dmat)))
# augx <- model.matrix(yfit)
# 
# # Loop to choose lambda
# lamseq <- exp(seq(-5, 2, length.out = 20))
# lres <- lapply(lamseq, function(lam){
#   augx <- rbind(augx, cbind(0, Dmat * sqrt(lam)))
#   res <- glm(augy ~ augx, method = "cirls.fit", 
#     constr = ~ shape(augx, "inc"))
#   list(res = res, aic = AIC(res))
# })
# 
# # Look at lambda
# aicseq <- sapply(lres, "[[", "aic")
# plot(log(lamseq), aicseq, type = "b")
# 
# # Extract
# smpred <- predict(lres[[which.min(aicseq)]]$res)[seq_len(NROW(warming))]
# warming <- mutate(warming, smpred = smpred)

#------------------------
# Plot
#------------------------

# Plot purely non-parametric predictions
ggplot(warming) + theme_bw() + 
  geom_point(aes(x = year, y = anomaly, col = "Observed"), size = 3, 
    alpha = .3, stroke = NA) + 
  geom_line(aes(x = year, y = ypred, col = "Estimated"), linewidth = .9) +
  geom_point(aes(x = year, y = ypred, col = "Estimated"), size = 2) +
  scale_color_manual(values = c("Observed" = 1, "Estimated" = 2), name = "",
    breaks = c("Observed", "Estimated"),
    guide = guide_legend(position = "inside")) +
  labs(y = "Temperature anomaly (°C)", x = "Year") +
  theme(legend.position.inside = c(0.08, .90))

# Plot coefficients from period strata
# ggplot(warming) + theme_bw() + 
#   geom_point(aes(x = year, y = anomaly), size = 3, alpha = .3, stroke = NA) + 
#   geom_pointrange(aes(x = perloc, y = ubeta, ymin = ulow, ymax = uhigh), 
#     size = 1, linewidth = 2, data = coefdf) +
#   geom_pointrange(aes(x = perloc, y = cbeta, ymin = clow, ymax = chigh), 
#     size = 1, linewidth = 2, col = 2, data = coefdf) +
#   labs(y = "Temperature anomaly", x = "Year")

# Save
ggsave("figures/figApp1_warming.pdf")


#----- Plot edf

# Create data.frame for number of change-points
# actdf <- tibble::enframe(attr(edfres, "actfreq"), 
#     name = "nchg", value = "freq") |>
#   mutate(nchg = edfres["udf"] - as.numeric(nchg), freq = 100 * freq)
# 
# # plot
# ggplot(actdf) + theme_bw() + 
#   geom_col(aes(x = nchg, y = freq)) + 
#   geom_vline(xintercept = edfres[c("odf", "edf")], col = 2) +
#   labs(x = "Number of change-points", y = "Proportion (%)") + 
#   scale_y_continuous(expand = c(0, 0))

