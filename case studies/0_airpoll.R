################################################################################
#
# Case study: Global Warming
#
################################################################################

library(cirls)
library(tidyverse)
library(splines)
library(dlnm)
library(scico)
library(patchwork)

#------------------------
# Some parameters
#------------------------

# Seed for simulations
set.seed(1808)

# Number of quantile levels for stratification
nlev <- 4

# Df for spline of temperature
tdf <- 5

# Df for seasonal component
sdf <- 7

# Color palette (constrained and unconstrained)
colpal <- "lipari"

#------------------------
# Data prep
#------------------------

# The london dataset is taken from the `cirls` package
london <- mutate(london, 
  
  # Stratifying 
  pm10q = cut(pm10, quantile(pm10, 0:nlev/nlev, na.rm = TRUE)),
  o3q = cut(o3, quantile(o3, 0:nlev/nlev, na.rm = TRUE))
)

# We also create a B-spline basis of a moving average for temperature
tbasis <- crossbasis(london$tmean, lag = 3, argvar = list(fun = "bs", df = tdf), 
  arglag = list(fun = "strata"))

#-----------------------
# Model
#-----------------------

#----- Fit models

modlist <- list()

# Constrained model
modlist$Constrained <- glm(death ~ pm10q + o3q + tbasis + ns(date, df = sdf * 8), 
  data = london, family = "quasipoisson", method = "cirls.fit", 
  constr = ~ shape(pm10q, "inc") + shape(o3q, "inc") + 
    shape(tbasis, shape = "cvx"))

# Fit equivalent unconstrained model
modlist$Unconstrained <- uncons(modlist$Constrained)

#----- Extract results

# For each model
reslist <- lapply(modlist, \(x){
  
  # Extract coefficients and CIs
  betas <- coef(x)
  cis <- confint(x)
  
  # Keep only the environmental terms
  ind <- !grepl("date", names(betas))
  
  # put together
  betadf <- cbind(est = betas[ind], cis[ind,]) |> 
    as.data.frame() |> rownames_to_column("coef")
  
  # Extract spline function
  tfun <- crosspred(tbasis, x, cen = 17)
  
  # Return
  list(betas = betadf, temp = tfun)
})


#-----------------------
# Plot
#-----------------------

#----- Pollutant plots

# Extract coefficients and cis
betadf <- lapply(reslist, "[[", "betas") |> bind_rows(.id = "mod")

# First two for pollutants
pollplots <- lapply(c("pm10", "o3"), \(poll){
  
  # Which coefficients
  polldf <- subset(betadf, grepl(poll, coef))
  
  # Initialise plot and theme
  ggplot(polldf) + theme_bw() + 
    
    # Add points and CIS
    geom_pointrange(aes(x = coef, y = est, ymin = low, ymax = high, 
        group = mod, col = mod, shape = mod), 
      position = position_dodge(width = .2), linewidth = 1, size = .7) + 
    geom_hline(yintercept = 0) + 
    
    # Scales
    scale_color_scico_d(palette = colpal, begin = .2, end = .8, name = "") +
    scale_shape_manual(name = "", 
      values = c(Constrained = 16, Unconstrained = 15)) +
    
    # Axes
    scale_x_discrete(name = sprintf("%s quartile", str_to_upper(poll)),
      labels = sprintf("Q%i", 2:nlev)) + 
    labs(y = "Coefficient (ref Q1)", title = str_to_upper(poll))
})

#----- Temperature plot

# Extract functions
fundf <- lapply(reslist, 
    \(x) x[["temp"]][c("predvar", "allfit", "allse")] |> bind_cols()
)
fundf <- bind_rows(fundf, .id = "mod")

# Compute cis
fundf <- mutate(fundf, 
  low = allfit - 1.96 * allse, high = allfit + 1.96 * allse)

# Initialise plot and theme
tplot <- ggplot(fundf) + theme_bw() + 
  
  # Add CIs and curves
  geom_ribbon(aes(x = predvar, ymin = low, ymax = high, group = mod, 
    fill = mod), alpha = .3) + 
  geom_line(aes(x = predvar, y = allfit, group = mod, col = mod), 
    linewidth = 1) +
  geom_hline(yintercept = 0) + 
  
  # Scales
  scale_color_scico_d(palette = colpal, begin = .2, end = .8, name = "") +
  scale_fill_scico_d(palette = colpal, begin = .2, end = .8, name = "") +
  guides(colour = "none", fill = "none") +
    
  # Axes
  labs(y = "Temperature effect", x = "Temperature (C)", title = "Temperature")

#----- Put together

# In a grid
wrap_plots(c(pollplots,list(tplot)), nrow = 1, guides = "collect")

# Export
ggsave("figures/Fig4.pdf", width = 15)
