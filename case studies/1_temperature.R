################################################################################
#
# Case study: Air pollution
#
################################################################################

#------------------------
# Some parameters
#------------------------

# Seed for simulations
set.seed(1808)

# Maximum lag for temperature
maxlag <- 3

# Df for spline of temperature
tdf <- 5

# Width of bins in degrees
binr <- 2

# Reference temperature for plotting
tref <- 17

# Df for seasonal component
sdf <- 7

# Color palette (constrained and unconstrained)
colpal <- "lipari"

#------------------------
# Data prep
#------------------------

#----- Data management

# Load data
dat <- read.csv("data/Firenze.csv", check.names = F,
  colClasses = c(date = "Date"))
# dat <- subset(london, year(date) %in% 2001:2003)

# Compute temperature moving average
dat <- mutate(dat, tma = runMean(tmean, 0:maxlag))

# Get number of years
n <- NROW(dat)
ny <- n / 365.25

#----- Initialise model

# Choose response
dat <- mutate(dat, 
  y = deaths_0044 + deaths_4564 + deaths_6574 + deaths_7584 + deaths_85p)
# yvar <- "age0_64"

# Initialise formula
baseform <- sprintf("y ~ ns(date, df = %i) + dow", round(ny * sdf)) |>
  as.formula()

#-----------------------
# Binned model
#-----------------------

#----- Define binning

# Define cutpts at given intervals
trng <- range(dat$tma, na.rm = T)
cutpts <- seq(trng[1] - trng[1] %% binr + binr, trng[2] - trng[2] %% binr,
  by = binr)
cutpts <- c(trng[1], cutpts[-c(1, length(cutpts))], trng[2])

# Create factor
tbins <- cut(dat$tma, cutpts)

# Determine ref and change contrast
binref <- findInterval(tref, cutpts)
contrasts(tbins) <- contr.treatment(nlevels(tbins), binref)

#----- Fit unconstrained model

# Update formula
binform <- update(baseform, ~ . + tbins)

# Fit model
ubm <- glm(binform, data = dat, family = "poisson")

# Extract coefs
ubcoefs <- coef(ubm)

# Extract effect for temperature
tind <- grep("tbin", names(ubcoefs))
utbfun <- ubcoefs[tind]

# Extract confidence intervals
utbci <- confint(ubm, tind)  
colnames(utbci) <- c("low", "high")

# Put together in data.frame
midpts <- rowMeans(cbind(cutpts[-1], cutpts[-length(cutpts)]))
ubres <- data.frame(temp = midpts[-binref], est = utbfun, utbci) |>
  add_row(temp = midpts[binref], est = 0)

#----- Fit constrained model

# Fit model
cbm <- update(ubm, method = "cirls.fit", constr = ~ shape(tbins, "cvx"))

# Extract effect
ctbfun <- coef(cbm)[tind]

# Confidence interval
ctbci <- confint(cbm, tind, nsim = 10000)  
colnames(ctbci) <- c("low", "high")

# Put together in data.frame
cbres <- data.frame(temp = midpts[-binref], est = ctbfun, ctbci) |>
  add_row(temp = midpts[binref], est = 0)

#----- Plot

# Full result data.frame: compute RRs
binres <- list(Unconstrained = ubres, Constrained = cbres) |>
  bind_rows(.id = "mod") |>
  mutate(est = exp(est), low = exp(low), high = exp(high))

# Initialise plot and theme
binplot <- ggplot(binres) + theme_bw() + 
  theme(axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    legend.position = "bottom", legend.direction = "horizontal") + 
  
  # Add CIs and curves
  geom_pointrange(aes(x = temp, y = est, ymin = low, ymax = high, group = mod,
    col = mod), position = position_dodge(width = 1)) +
  # geom_point(aes(x = temp, y = est, group = mod, col = mod)) + 
  # geom_line(aes(x = temp, y = low, group = mod, col = mod)) +
  # geom_line(aes(x = temp, y = high, group = mod, col = mod)) +
  geom_hline(yintercept = 1) + 
  
  # Scales
  scale_color_scico_d(palette = colpal, begin = .2, end = .8, name = "") +
    
  # Axes
  labs(y = "Relative mortality increase", x = "Temperature (C)", 
    title = "a) Binned model")


#-----------------------
# Spline model
#-----------------------

#----- Define spline bases

# Spline basis
tspl <- onebasis(dat$tma, fun = "bs", 
  knots = quantile(london$tma, 1:4/5, na.rm = T))

# Grid and basis for prediction
tgrid <- seq(trng[1], trng[2], length.out = 40)
predspl <- onebasis(tgrid, fun = "bs", 
    knots = quantile(london$tma, 1:4/5, na.rm = T)) |>
  scale(center = onebasis(tref, fun = "bs", 
    knots = quantile(london$tma, 1:4/5, na.rm = T)), scale = F)

#----- Fit unconstrained model

# Update formula
splform <- update(baseform, ~ . + tspl)

# Fit model
usm <- glm(splform, data = dat, family = "poisson")

# Extract coefs
uscoefs <- coef(usm)

# Extract effect for temperature
useff <- crosspred(tspl, usm, cen = tref, at = tgrid)

# Put together in data.frame
usres <- useff[c("predvar", "allRRfit", "allRRlow", "allRRhigh")] |>
  as.data.frame() |>
  rename(temp = predvar, est = allRRfit, low = allRRlow, high = allRRhigh)

#----- Fit constrained model

# Fit model
csm <- update(usm, method = "cirls.fit", constr = ~ shape(tspl, "cvx"))

# Extract effect
sind <- grep("tspl", names(coef(csm)))
cseff <- predspl %*% coef(csm)[sind]

# Confidence interval
csim <- simulCoef(csm, nsim = 10000)
predsim <- predspl %*% t(csim[, sind])
cscis <- apply(predsim, 1, quantile, c(.025, .975)) |> t()
colnames(cscis) <- c("low", "high")

# Put together in data.frame
csres <- data.frame(temp = tgrid, est = exp(cseff), exp(cscis))

#----- Plot

# Full result data.frame: compute RRs
splres <- list(Unconstrained = usres, Constrained = csres) |>
  bind_rows(.id = "mod") |> remove_rownames()

# Initialise plot and theme
splplot <- ggplot(splres) + theme_bw() + 
  theme(axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    legend.position = "bottom", legend.direction = "horizontal") + 
  
  # Add CIs and curves
  geom_ribbon(aes(x = temp, ymin = low, ymax = high, group = mod, fill = mod),
    alpha = .2) +
  geom_line(aes(x = temp, y = est, group = mod, col = mod), linewidth = 1) +
  # geom_line(aes(x = temp, y = low, group = mod, col = mod)) +
  # geom_line(aes(x = temp, y = high, group = mod, col = mod)) +
  geom_hline(yintercept = 1) + 
  
  # Scales
  scale_color_scico_d(palette = colpal, begin = .2, end = .8, name = "") +
  scale_fill_scico_d(palette = colpal, begin = .2, end = .8, name = "") +
    
  # Axes
  labs(y = "Relative mortality increase", x = "Temperature (C)", 
    title = "b) Spline model")


#-----------------------
# Final plot
#-----------------------

# Put plots together
wrap_plots(binplot, splplot, nrow = 1)

# Save
ggsave("figures/Fig4.pdf", width = 10)
