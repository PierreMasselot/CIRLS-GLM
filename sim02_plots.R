################################################################################
#
# Simulation plots
#
################################################################################

load("temp/simures.RData")

#--------------------
# Parameters
#--------------------

#----- Define parameters

# Error measures
measlabs <- c(bias = "Squared bias", empse = "Standard error", rmse = "RMSE")
meascols <- c(2, 4, 1) 
meassize <- c(2, 2, 4)
names(meascols) <- names(meassize) <- measlabs

# Degrees of freedom
dfcol <- scico(2, palette = "imola", end = .8)
names(dfcol) <- c("odf", "edf")

#----- Some data management

# Add info about the scenarios
simures <- merge(simures, scenarios)

# Models and data-generating mechanisms as factors for ordering
simures <- mutate(simures, 
  dgm = factor(dgm, levels = names(lablist), labels = unlist(lablist)),
  mod = factor(mod, levels = c("cirls", "glm"), 
    labels = c("Constrained", "Unconstrained")),
  coef = factor(coef)
  )

# Select results to show
showres <- subset(simures, n == 500 & s2 == 50 & par %in% c(-1, 0, 1) &
    dgm %in% c("Non-negative regression", 
      "Non-decreasing strata")) |>
  mutate(par = factor(par, levels = c(-1, 0, 1), 
    labels = c("Wrong", "Boundary", "Right")))
  

#--------------------
# Figure 1: Bias-variance
#--------------------

# Select data
estres <- subset(simures, n == 500 & s2 == 50 & 
    dgm %in% c("Non-negative regression", "Non-decreasing strata")) |>
  summarise(
    bias = mean(est - true)^2, # Bias squared
    empse = sd(est), # Standard error
    rmse = sqrt(mean((est - true)^2)), #RMSE
    .by = c("mod", "coef", "dgm", "par")
  ) |>
  
  # Reshape results
  pivot_longer(bias:rmse, names_to = "measure") |>
  pivot_wider(names_from = "mod", values_from = "value") |>
  
  # Compute difference between models
  mutate(diff = Constrained - Unconstrained,
    measure = factor(measure, levels = names(measlabs), labels = measlabs)
  )

# Select for each DGM
est_spl <- list(
  # Prepare results for first DGM
  subset(estres, dgm == "Non-negative regression" & coef != 0) |>
    mutate(coef = 
        factor(coef, labels = c("Main coefficient", "Covariate coefficient"))),
  
  # Prepare results for second DGM
  subset(estres, dgm == "Non-decreasing strata" & coef %in% c(0, 2, 4)) |> 
    mutate(coef = factor(coef, 
      labels = sprintf("%s strata", c("Lower", "Middle", "Higher"))))
)

#----- Plot
estplots <- imap(est_spl, function(d, i) ggplot(d) + theme_bw() + 
    # By coefficient
    facet_grid(cols = vars(coef)) +
    # Result curves
    geom_point(aes(x = par, y = diff, col = measure, group = measure, 
      size = measure)) +
    geom_line(aes(x = par, y = diff, col = measure, group = measure)) +
    # Main delimitations of the plot with labels
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_label(x = 0.1, y = max(d$diff), label = "Feasible", hjust = 0, 
      label.size = 0, label.padding = unit(0, "mm")) +
    geom_label(x = -0.1, y = max(d$diff), label = "Unfeasible", hjust = 1, 
      label.size = 0, label.padding = unit(0, "mm")) +
    # Scales
    scale_color_manual(name = "",
      values = c("Squared bias" = 2, "Standard error" = 4, "RMSE" = 1)) + 
    scale_size_manual(name = "",
      values = c("Squared bias" = 2, "Standard error" = 2, "RMSE" = 4)) +
    # Titles and theme
    labs(x = expression(Feasibility ~ gamma), 
      y = "Increase compared to unconstrained",
      title = sprintf("%s) %s", letters[i], unique(d$dgm))) + 
    theme(panel.grid.minor = element_blank(),
      strip.placement = "outside"))

# Put together and save
resplot <- wrap_plots(estplots, nrow = 2, guides = "collect")
ggsave("figures/RMSEcurves.pdf", plot = resplot, width = 12, height = 10)


#--------------------
# Figure 2: Inference measures
#--------------------

#----- Compute inference mesaures

# Select data
infres <- subset(simures, n == 500 & mod == "Constrained" &
    dgm %in% c("Non-negative regression", "Non-decreasing strata")) |>
  summarise(
    empse = sd(est), # Standard error
    modse = sqrt(mean(v)), # Average standard deviation
    seerr = 100 * ((modse / empse) - 1), # Relative error in modse
    cover = 100 * mean(mean(low <= true & true <= high)),
    becover = 100 * mean(mean(low <= mean(est) & mean(est) <= high)), # Coverage ("bias-eliminated")
    .by = c("mod", "coef", "dgm", "par")
  )

# Select for each DGM
inf_spl <- list(
  # Prepare results for first DGM
  subset(infres, dgm == "Non-negative regression" & coef != 0) |>
    mutate(coef = 
        factor(coef, labels = c("Main coefficient", "Covariate coefficient"))),
  
  # Prepare results for second DGM
  subset(infres, dgm == "Non-decreasing strata" & coef %in% c(0, 2, 4)) |> 
    mutate(coef = factor(coef, 
      labels = sprintf("%s strata", c("Lower", "Middle", "Higher"))))
)

#----- Create plot of SE error

seplots <- imap(inf_spl, function(d, i) ggplot(d) + theme_bw() + 
  # Result curves
  geom_point(aes(x = par, y = seerr, col = coef, group = coef)) +
  geom_line(aes(x = par, y = seerr, col = coef, group = coef)) +
  # Main delimitations of the plot with labels
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_label(x = 0.1, y = min(50, max(d$seerr)), label = "Feasible", hjust = 0, 
    label.size = 0, label.padding = unit(0, "mm")) +
  geom_label(x = -0.1, y = min(50, max(d$seerr)), label = "Unfeasible", hjust = 1, 
    label.size = 0, label.padding = unit(0, "mm")) +
  # Scales
  scale_color_scico_d(name = "", palette = "glasgow", end = .8) + 
  coord_cartesian(ylim = c(max(-50, min(d$seerr)), min(50, max(d$seerr)))) +
  # Titles and theme
  labs(x = expression(Feasibility ~ gamma), y = "Relative Variance error (%)",
    title = sprintf("%s) %s", letters[i], unique(d$dgm))) + 
  theme(panel.grid.minor = element_blank(),
    strip.placement = "outside", legend.position = "bottom"))

#----- Create plot of coverage

coverplots <- map(inf_spl, function(d) ggplot(d) + theme_bw() + 
    # Result curves
    geom_point(aes(x = par, y = cover, col = coef, group = coef)) +
    geom_line(aes(x = par, y = cover, col = coef, group = coef)) +
    # Main delimitations of the plot with labels
    geom_hline(yintercept = 95) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_label(x = 0.1, y = 0, label = "Feasible", hjust = 0, 
      label.size = 0, label.padding = unit(0, "mm")) +
    geom_label(x = -0.1, y = 0, label = "Unfeasible", hjust = 1, 
      label.size = 0, label.padding = unit(0, "mm")) +
    # Scales
    scale_color_scico_d(name = "", palette = "glasgow", end = .8,
      guide = "none") + 
    coord_cartesian(ylim = c(0, 100)) +
    # Titles and theme
    labs(x = expression(Feasibility ~ gamma), y = "Coverage (%)") + 
    theme(panel.grid.minor = element_blank(),
      strip.placement = "outside")
)

# Put together and save
inferplot <- wrap_plots(c(seplots, coverplots), ncol = 2)
ggsave("figures/InferenceCurves.pdf", plot = inferplot, height = 8, width = 10)


#--------------------
# Figure 3: Degrees of freedom
#--------------------

#----- Compute the degrees of freedom

# I select only coef 0 to get one value of df and AIC (repeated for each coef)
dfres <- subset(simures, coef == 0 & mod == "Constrained") |>
  
  # Summarise DF results
  summarise( 
    
    # ODF average
    dfmean = mean(odf),
    
    # EDF distribution
    edfmean = median(edf),
    edflow = quantile(edf, .25),
    edfhigh = quantile(edf, .75),
    
    # Grouping
    .by = all_of(c("dgm", "n", "s2", "par"))
  ) |>
  
  # For labels
  mutate(dgm = factor(dgm, labels = sprintf("%s) %s", 
    letters[seq_along(unique(dgm))], unique(dgm))))

#----- Plot df


# Create plot
dfplot <- ggplot(dfres) + theme_bw() + 
  # By DGM
  facet_wrap(~ dgm, scales = "free") +
  # Result curves
  geom_pointrange(aes(x = par, y = edfmean, ymin = edflow, ymax = edfhigh, 
    col = "edf")) +
  geom_line(aes(x = par, y = edfmean, col = "edf")) +
  geom_point(aes(x = par, y = dfmean, col = "odf")) +
  geom_line(aes(x = par, y = dfmean, col = "odf")) +
  # Delimitation of feasibility
  geom_vline(xintercept = 0, linetype = 2) +
  # Titles and theme
  scale_color_manual(values = dfcol, name = "",
    labels = c("edf (median + IQR)", "odf (mean)")) +
  labs(x = "Lower bound", y = "Degrees of freedom") + 
  theme(panel.grid.minor = element_blank(),
    strip.placement = "outside", legend.position = "bottom",
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text=element_text(size = 15))

# Save
ggsave("figures/dfcurve.pdf", dfplot)
