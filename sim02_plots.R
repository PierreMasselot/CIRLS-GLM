################################################################################
#
# Simulation plots
#
################################################################################

#--------------------
# Parameters
#--------------------

#----- Define parameters

# Models
modlabs <- c(uncons = "Unconstrained", cons = "Constrained", 
  over = "Overconstrained")
modpal <- scico(length(modlabs), palette = "glasgow", end = .8, alpha = .8)
modshp <- 15:17
names(modpal) <- names(modshp) <- modlabs[c("over", "cons", "uncons")]


#----- Some data management

# Models and data-generating mechanisms as factors for ordering
simures <- mutate(simures, 
  model = factor(model, levels = names(modlabs), labels = modlabs),
  dgm = factor(dgm, levels = names(dgmlist), 
    labels = sapply(dgmlist, "[[", "lab")))

#--------------------
# Visual comparison
#--------------------

# Loop on data-generating mechanismes
plot_comp <- foreach(res = isplit(simures, simures[c("dgm", "n")])) %do% {
  
  # Plot results as boxplot
  ggplot(res$value) + theme_bw() + 
    geom_boxplot(aes(x = coef, y = est, group = interaction(model, coef), 
      fill = model), outlier.fill = NULL, outlier.shape = 21) + 
    geom_segment(aes(x = coef - .45, xend = coef + .45, y = true), 
      linewidth = 1) + 
    geom_vline(xintercept = unique(res$value$coef)[-1] - .5) +
    scale_fill_manual(values = modpal, labels = modlabs, name = "Model") + 
    labs(title = sprintf("%s, n = %i", res$key$dgm, res$key$n),
      x = "Coefficient", y = "Estimate")
    # facet_wrap(~ n, labeller = "label_both")
  
}

# Show results
lapply(plot_comp, print)

# Export some plots
wrap_plots(plot_comp[3:4], guides = "collect", widths = c(3, 5))
ggsave("figures/figSim1_ResBoxplots.pdf", width = 12)

#--------------------
# All results at coefficient level
#--------------------

#----- Compute errors

# Grouping variables
byvar <- c("model", "coef", "dgm", "n", "sigma")

# Labels of measures
measlabs <- c(bias = "Bias", empse = "Standard error", 
  mse = "Mean squared error")

# Compute estimation error measures and their SE
coeferror <- summarise(simures, 
  # Bias
  bias = mean(est - true), 
  bias_se = sqrt(var(est) / nsim),
  
  # Standard error
  empse = sd(est),
  empse_se = empse / sqrt(2 * (nsim - 1)), 
  
  # RMSE
  mse = mean((est - true)^2),
  mse_se = sqrt(var((est - true)^2) / nsim),
  
  # Coefficients sd and relative error
  modse = sqrt(mean(v)), 
  modse_se = sqrt(var(v) / (4 * nsim * modse^2)),
  seerr = 100 * ((modse / empse) - 1),
  seerr_se = 100 * (modse / empse) * 
    sqrt(var(v) / (4 * nsim * modse^4) + 1 / (2 * (unique(n) + 1))),
  
  # Coverage and bias-corrected coverage
  cover = mean(low <= true & true <= high),
  cover_se = sqrt(cover * (1 - cover) / nsim),
  becover = mean(mean(low <= mean(est) & mean(est) <= high)),
  becover_se = sqrt(becover * (1 - becover) / nsim),
  
  # Grouping
  .by = all_of(byvar))

#----- Loop over measures and data-generating mechanisms

# plot_error <- foreach (meas = c("bias", "empse", "mse")) %:% 
#   foreach(errdat = isplit(coeferror, coeferror$dgm)) %do% 
# {
#   
#   # Plot for dgm and measure
#   ggplot(errdat$value) + theme_classic() +
#     geom_line(aes(x = coef, y = .data[[meas]], group = model, col = model),
#       linewidth = 1) +
#     geom_pointrange(aes(x = coef, group = model, col = model, shape = model,
#         y = .data[[meas]],
#         ymin = .data[[meas]] - .data[[sprintf("%s_se", meas)]],
#         ymax = .data[[meas]] + .data[[sprintf("%s_se", meas)]])) +
#     geom_hline(yintercept = 0) +
#     scale_colour_manual(values = modpal, labels = modlabs, name = "Model") +
#     scale_shape(name = "Model") +
#     labs(x = "Coefficient", y = measlabs[meas]) +
#     facet_wrap(~ n, labeller = "label_both") + 
#     labs(title = paste(meas, errdat$key))
# 
#   # Plot for dgm and measure    
#   ggplot(errdat$value) + theme_classic() +
#     geom_pointrange(aes(x = factor(n), group = model, col = model,
#       y = .data[[meas]],
#       ymin = .data[[meas]] - .data[[sprintf("%s_se", meas)]],
#       ymax = .data[[meas]] + .data[[sprintf("%s_se", meas)]])) +
#     geom_hline(yintercept = 0) +
#     scale_colour_manual(values = modpal, labels = modlabs, name = "Model") +
#     labs(x = "Number of observation", y = measlabs[meas]) +
#     facet_wrap(~ coef) +
#     labs(title = paste(meas, errdat$key))
# }

# # Show results
# lapply(plot_error, print)

#----- Plot MSE

# Across DGM
plot_mse <- foreach(dat = isplit(coeferror, coeferror[c("dgm", "n")])) %do% 
{
  
  # Limits
  # lims <- subset(uncdat$value, model != "over") |> summarise(range(seerr)) |> 
  #   unlist() * 2
  
  # Plot
  ggplot(dat$value) + theme_bw() + 
    geom_pointrange(aes(x = factor(coef), group = model, col = model, 
      shape = model, y = mse, ymin = mse - 2 * mse_se, ymax = mse + 2 * mse_se), 
      size = 1, linewidth = .9) +
    # geom_hline(yintercept = 0) + 
    scale_colour_manual(values = modpal, labels = modlabs, name = "Model") + 
    scale_shape_manual(values = modshp, labels = modlabs,name = "Model") +
    labs(title = sprintf("%s, n = %i", dat$key$dgm, dat$key$n),
      x = "Coefficient", y = "MSE") + 
    scale_y_log10() + 
    theme(panel.grid.minor = element_blank())
}

# Show results
lapply(plot_mse, print)

# Export some plots
wrap_plots(plot_mse[3:4], guides = "collect", widths = c(3, 5))
ggsave("figures/figSim2_MSE.pdf", width = 10)

#----- Variance estimation

# Across DGM
plot_variance <- foreach(dat = isplit(coeferror, coeferror[c("dgm", "n")])) %do% 
{
  
  # Limits
  lims <- subset(dat$value, model != "over" & empse > 0 & seerr < 100) |>
    reframe(range(seerr)) |>
    unlist()
  
  # Plot
  ggplot(dat$value) + theme_bw() + 
    geom_pointrange(aes(x = coef, group = model, col = model, shape = model,
      y = seerr, ymin = seerr - seerr_se, ymax = seerr + seerr_se)) +
    geom_hline(yintercept = 0) + 
    scale_colour_manual(values = modpal, labels = modlabs, name = "Model") + 
    scale_shape_manual(values = modshp, labels = modlabs,name = "Model") +
    labs(title = sprintf("%s, n = %i", dat$key$dgm, dat$key$n),
      x = "Coefficient", y = "Relative % error in SE") +
    coord_cartesian(ylim = lims) + 
    theme(panel.grid.minor = element_blank())
}

# Show results
lapply(plot_variance, print)

# Export some plots
wrap_plots(plot_variance[3:4], guides = "collect", widths = c(3, 5))
ggsave("figures/figSim3_SEerr.pdf", width = 10)

#----- Coverage

# Across DGM
plot_cover <- foreach(dat = isplit(coeferror, coeferror[c("dgm", "n")])) %do% 
{
  
  # Plot
  ggplot(dat$value) + theme_bw() + 
    geom_pointrange(aes(x = coef, group = model, col = model, shape = model,
      y = becover, ymin = becover - becover_se, ymax = becover + becover_se)) +
    geom_hline(yintercept = 0.95) + 
    scale_colour_manual(values = modpal, labels = modlabs, name = "Model") + 
    scale_shape_manual(values = modshp, labels = modlabs,name = "Model") +
    labs(title = sprintf("%s, n = %i", dat$key$dgm, dat$key$n),
      x = "Coefficient", y = "Bias-eliminated coverage") +
    theme(panel.grid.minor = element_blank())
}

# Show results
lapply(plot_cover, print)

# Export some plots
wrap_plots(plot_cover[3:4], guides = "collect", widths = c(3, 5))
ggsave("figures/figSim4_BEcover.pdf", width = 10)


#--------------------
# Coefficient inference
#--------------------

#----- Compute uncertainty

# Grouping variables
byvar <- c("model", "coef", "dgm", "n", "sigma")

# Compute inference
coefuncert <- summarise(simures, 
  
  
  # Coverage and bias-corrected coverage
  cover = mean(low <= true & true <= high),
  cover_se = sqrt(cover * (1 - cover) / nsim),
  becover = mean(mean(low <= mean(est) & mean(est) <= high)),
  becover_se = sqrt(becover * (1 - becover) / nsim),

  # Grouping
  .by = all_of(byvar))

#----- Plot error in Model SE

# Across DGM
plot_variance <- foreach(uncdat = isplit(coefuncert, coefuncert$dgm)) %do% {
    
  # Limits
  lims <- subset(uncdat$value, model != "over") |> summarise(range(seerr)) |> 
    unlist() * 2
  
  # Plot
  ggplot(uncdat$value) + theme_classic() + 
    geom_pointrange(aes(x = coef, group = model, col = model, shape = model,
      y = seerr, ymin = seerr - seerr_se, ymax = seerr + seerr_se)) +
    geom_hline(yintercept = 0) + 
    scale_colour_manual(values = modpal, labels = modlabs, name = "Model") + 
    scale_shape(name = "Model") +
    labs(x = "Coefficient", y = "Relative % error in SE") + 
    facet_wrap(~ n, labeller = "label_both") + 
    labs(title = sprintf("Variance (%s)", uncdat$key)) + 
    ylim(lims)
}

# Show results
lapply(plot_variance, print)

#----- Plot coverage

plot_cover <- foreach (meas = c("cover", "becover")) %:% 
  foreach(errdat = isplit(coefuncert, coefuncert$dgm)) %do% 
{
  
  # Plot for dgm and measure
  ggplot(errdat$value) + theme_classic() +
    geom_line(aes(x = coef, y = .data[[meas]], group = model, col = model),
      linewidth = 1) +
    geom_pointrange(aes(x = coef, group = model, col = model, shape = model,
      y = .data[[meas]],
      ymin = .data[[meas]] - .data[[sprintf("%s_se", meas)]],
      ymax = .data[[meas]] + .data[[sprintf("%s_se", meas)]])) +
    geom_hline(yintercept = 0) +
    scale_colour_manual(values = modpal, labels = modlabs, name = "Model") +
    scale_shape(name = "Model") +
    labs(x = "Variable", y = measlabs[meas]) +
    facet_wrap(~ n, labeller = "label_both") + 
    labs(title = paste(meas, errdat$key))

}

# Show results
lapply(plot_error, print)