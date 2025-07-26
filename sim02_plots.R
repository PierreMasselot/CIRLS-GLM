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

# Models
modpal <- scico(2, palette = "glasgow", end = .8, alpha = .8)
modshp <- c(16, 15)
names(modpal) <- names(modshp) <- c("Constrained", "Unconstrained")

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
# Visual comparison
#--------------------

# # Loop on data-generating mechanismes
# plot_comp <- foreach(res = isplit(simures, simures[c("dgm", "n")])) %do% {
#   
#   # Plot results as boxplot
#   ggplot(res$value) + theme_bw() + 
#     geom_boxplot(aes(x = coef, y = est, group = interaction(model, coef), 
#       fill = model), outlier.fill = NULL, outlier.shape = 21) + 
#     geom_segment(aes(x = coef - .45, xend = coef + .45, y = true), 
#       linewidth = 1) + 
#     geom_vline(xintercept = unique(res$value$coef)[-1] - .5) +
#     scale_fill_manual(values = modpal, labels = modlabs, name = "Model") + 
#     labs(title = sprintf("%s, n = %i", res$key$dgm, res$key$n),
#       x = "Coefficient", y = "Estimate")
#     # facet_wrap(~ n, labeller = "label_both")
#   
# }
# 
# # Show results
# lapply(plot_comp, print)
# 
# # Export some plots
# wrap_plots(plot_comp[3:4], guides = "collect", widths = c(3, 5))
# ggsave("figures/figSim1_ResBoxplots.pdf", width = 12)


# #----- Compute summaries
# 
# # Grouping variables
# byvar <- c("mod", "coef", "dgm", "n", "s2", "par")
# 
# # Labels of measures
# measlabs <- c(bias = "Squared bias", empse = "Standard error", 
#   mse = "Mean squared error")
# 
# # Compute estimation error measures and their SE
# coeferror <- summarise(simures, 
#   # Bias
#   bias = mean(est - true)^2, 
#   bias_se = sqrt(var(est) / nsim),
#   
#   # Standard error
#   empse = sd(est),
#   empse_se = empse / sqrt(2 * (nsim - 1)), 
#   
#   # RMSE
#   mse = mean((est - true)^2),
#   mse_se = sqrt(var((est - true)^2) / nsim),
#   
#   # Coefficients sd and relative error
#   modse = sqrt(mean(v)), 
#   modse_se = sqrt(var(v) / (4 * nsim * modse^2)),
#   seerr = 100 * ((modse / empse) - 1),
#   seerr_se = 100 * (modse / empse) * 
#     sqrt(var(v) / (4 * nsim * modse^4) + 1 / (2 * (unique(n) + 1))),
#   
#   # Coverage and bias-corrected coverage
#   cover = mean(low <= true & true <= high),
#   cover_se = sqrt(cover * (1 - cover) / nsim),
#   becover = mean(mean(low <= mean(est) & mean(est) <= high)),
#   becover_se = sqrt(becover * (1 - becover) / nsim),
#   
#   # Grouping
#   .by = all_of(byvar)
# )

# #--------------------
# # Coefficients estimation
# #--------------------
# 
# #----- Prepare data
# 
# # Labels of measures
# measlabs <- c(bias = "Squared bias", empse = "Standard error", 
#   rmse = "RMSE")
# 
# # Compute the errors
# errormeas <- summarise(showres,
#     bias = mean(est - true)^2, # Bias squared
#     empse = sd(est), # Standard error
#     rmse = sqrt(mean((est - true)^2)), #RMSE
#     .by = c("mod", "coef", "dgm", "par")
#   ) |>
#   
#   # Reshape results
#   pivot_longer(bias:rmse, names_to = "measure") |>
#   pivot_wider(id_cols = coef:measure, 
#     names_from = "mod", values_from = "value") |>
#   
#   # Compute relative difference between models
#   mutate(diff = Constrained - Unconstrained,
#     measure = factor(measure, levels = names(measlabs), labels = measlabs)
#   )
# 
# # Split
# errormeas_spl <- list(
#   # Prepare results for first DGM
#   subset(errormeas, dgm == "Non-negative regression" & coef != 0) |>
#     mutate(coef = factor(coef, labels = c("Main", "Covariate")),
#       diffshr = pmin(diff, .025),
#       shrind = diff > .025),
# 
#   # Prepare results for second DGM
#   subset(errormeas, dgm == "Non-decreasing strata") |> 
#     mutate(coef = factor(as.numeric(coef)),
#       diffshr = pmin(diff, .01),
#       shrind = diff > .01)
# )
# 
# #----- Plot 
# 
# # Plot layouts
# errplots <- lapply(errormeas_spl, function(d){
#   ggplot(d) + theme_bw() + 
#     facet_grid(rows = vars(measure), switch = "y", scales = "free") +
#     geom_hline(yintercept = 0) +
#     geom_point(aes(x = factor(par), y = diffshr, col = coef, shape = shrind), 
#       size = 4, position = position_dodge(width = .8)) +
#     geom_segment(aes(x = factor(par), y = 0, yend = diffshr, col = coef), 
#       linewidth = 1.5, position = position_dodge(width = .8),
#       show.legend = F) +
#     scale_color_scico_d(palette = "glasgow", name = "Coefficient",
#       end = .8) + 
#     labs(x = "Sub-scenario", y = NULL) + 
#     guides(shape = guide_none()) +
#     theme(panel.grid.minor = element_blank(),
#       strip.placement = "outside", 
#       strip.background = element_rect(fill = NA, color = NA),
#       legend.position = "bottom")
# })
# 
# # Make changes for DGM1
# errplots[[1]] <- errplots[[1]] + 
#   # geom_segment(aes(x = nlevels(factor(par)) + .5, y = .05, yend = .25), 
#   #   col = 2, arrow = arrow(length = unit(0.1, "inches")), linewidth = 1) +
#   # geom_segment(aes(x = nlevels(factor(par)) + .5, y = -.05, yend = -.25), 
#   #   col = 3, arrow = arrow(length = unit(0.1, "inches")), linewidth = 1) +
#   # geom_text(aes(x = nlevels(factor(par)) + .5, y = .15, label = "Worsened"), 
#   #   col = 2, angle = 90, size = 3) + 
#   # geom_text(aes(x = nlevels(factor(par)) + .5, y = -.15, label = "Improved"), 
#   #   col = 3, angle = 90, size = 3) + 
#   # scale_y_continuous(transform = transform_modulus(-3)) + 
#   # coord_cartesian(ylim = c(-.4, .4)) + 
#   labs(title = "a) Non-negative regression")
# 
# # Make changes for DGM2
# errplots[[2]] <- errplots[[2]] + 
#   # geom_text(aes(x = nlevels(factor(par)) + .5, y = .06, label = "Worsened"), 
#   #   col = 2, angle = 90, size = 3) + 
#   # geom_text(aes(x = nlevels(factor(par)) + .5, y = -.06, label = "Improved"), 
#   #   col = 3, angle = 90, size = 3) + 
#   # scale_y_continuous(transform = transform_modulus(-3)) + 
#   # coord_cartesian(ylim = c(-.15, .15)) + 
#   labs(title = "b) Non-decreasing strata") + 
#   guides(colour = guide_legend(title = "Strata"))
# 
# # Put together and save
# wrap_plots(errplots, ncol = 2)
# ggsave("figures/RMSEres.pdf", height = 10, width = 10)
# 
# 
# #--------------------
# # Inference
# #--------------------
# 
# #----- Compute measures
# 
# # Select results
# infmeas <- subset(showres, mod == "Constrained" & par != "Wrong") |>
#   
#   # Compute inference measures
#   summarise(
#     empse = sd(est), # Standard error
#     modse = sqrt(mean(v)), # Average standard deviation
#     seerr = 100 * ((modse / empse) - 1), # Relative error in modse
#     becover = mean(mean(low <= mean(est) & mean(est) <= high)), # Coverage ("bias-eliminated")
#     .by = c("mod", "coef", "dgm", "par"))
# 
# infmeas_spl <- list(
#   # Prepare results for first DGM
#   subset(infmeas, dgm == "Non-negative regression" & coef != 0) |>
#     mutate(coef = factor(coef, labels = c("Main", "Covariate"))),
#   
#   # Prepare results for second DGM
#   subset(infmeas, dgm == "Non-decreasing strata") |> 
#     mutate(coef = factor(as.numeric(coef)))
# )
# 
# #----- Plot
# 
# # Prepare error in standard error plots
# seplots <- lapply(infmeas_spl, function(d){
#   ggplot(d) + theme_bw() + 
#     geom_hline(yintercept = 0) +
#     geom_point(aes(x = factor(par), y = seerr, col = coef),
#       size = 5, position = position_dodge(width = .8)) +
#     geom_segment(aes(x = factor(par), y = 0, yend = seerr, col = coef),
#       linewidth = 1.5, position = position_dodge(width = .8),
#       show.legend = F) +
#     scale_color_scico_d(palette = "glasgow", name = "Coefficient",
#       end = .8) +
#     labs(x = "", y = "Relative Variance error (%)") +
#     theme(panel.grid.minor = element_blank())
# })
# 
# # Modify standard error plot
# selims <- c(-5, 10)
# seplots[[1]] <- seplots[[1]] + coord_cartesian(ylim = selims) + 
#   labs(title = "a) Non-negative regression") + 
#   guides(color = guide_none())
# seplots[[2]] <- seplots[[2]] + coord_cartesian(ylim = selims) + 
#   labs(title = "b) Non-decreasing strata") + 
#   guides(color = guide_none()) + 
#   guides(colour = guide_legend(title = "Strata"))
# 
# # Prepare coverage plots
# covplots <- lapply(infmeas_spl, function(d){
#   ggplot(d) + theme_bw() + 
#     geom_hline(yintercept = 95) +
#     geom_point(aes(x = factor(par), y = 100 * becover, col = coef),
#       size = 5, position = position_dodge(width = .8)) +
#     geom_segment(aes(x = factor(par), y = 0.95, yend = becover, col = coef),
#       linewidth = 1.5, position = position_dodge(width = .8),
#       show.legend = F) +
#     scale_color_scico_d(palette = "glasgow", name = "Coefficient",
#       end = .8) +
#     labs(x = "", y = "Coverage (%)") +
#     theme(panel.grid.minor = element_blank())
# })
# 
# # Modify coverage plots
# covlims <- c(80, 100)
# covplots[[1]] <- covplots[[1]] + coord_cartesian(ylim = covlims) + 
#   theme(legend.position = "bottom")
# covplots[[2]] <- covplots[[2]] + coord_cartesian(ylim = covlims) + 
#   theme(legend.position = "bottom") + 
#   guides(colour = guide_legend(title = "Strata"))
# 
# # Put everything together and save
# wrap_plots(c(seplots, covplots), ncol = 2)
# ggsave("figures/Coverageres.pdf", height = 8, width = 10)


#--------------------
# Appendix: a finer look at bias-variance
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
estplots <- Map(function(d, lab) ggplot(d) + theme_bw() + 
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
    labs(x = expression(gamma), y = "Increase compared to unconstrained",
      title = lab) + 
    theme(panel.grid.minor = element_blank(),
      strip.placement = "outside"), 
  # strip.background = element_rect(fill = NA, color = NA),
  # legend.position = "bottom")
est_spl, sprintf("%s) %s", letters[1:2], 
  c("Non-negative regression", "Non-decreasing strata")))


# Put together and save
resplot <- wrap_plots(estplots, nrow = 2, guides = "collect")
ggsave("figures/RMSEcurves.pdf", plot = resplot, width = 12, height = 10)


#--------------------
# Appendix: Inference measures
#--------------------

#----- Compute inference mesaures

# Select data
infres <- subset(simures, n == 500 & s2 == 50 & mod == "Constrained" &
    dgm %in% c("Non-negative regression", "Non-decreasing strata")) |>
  summarise(
    empse = sd(est), # Standard error
    modse = sqrt(mean(v)), # Average standard deviation
    seerr = 100 * ((modse / empse) - 1), # Relative error in modse
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

seplots <- Map(function(d, lab) ggplot(d) + theme_bw() + 
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
  scale_color_scico_d(name = "Coefficient", palette = "glasgow", end = .8) + 
  coord_cartesian(ylim = c(max(-50, min(d$seerr)), min(50, max(d$seerr)))) +
  # Titles and theme
  labs(x = expression(gamma), y = "Relative Variance error (%)",
    title = lab) + 
  theme(panel.grid.minor = element_blank(),
    strip.placement = "outside", legend.position = "bottom"),
inf_spl, sprintf("%s) %s", letters[1:2], 
    c("Non-negative regression", "Non-decreasing strata")))


#----- Create plot of coverage

coverplots <- lapply(inf_spl, function(d) ggplot(d) + theme_bw() + 
    # Result curves
    geom_point(aes(x = par, y = becover, col = coef, group = coef)) +
    geom_line(aes(x = par, y = becover, col = coef, group = coef)) +
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
    labs(x = expression(gamma), y = "Coverage (%)") + 
    theme(panel.grid.minor = element_blank(),
      strip.placement = "outside")
)

# Put together and save
inferplot <- wrap_plots(c(seplots, coverplots), ncol = 2)
ggsave("figures/InferenceCurves.pdf", plot = inferplot, height = 8, width = 10)


#--------------------
# Degrees of freedom and model selection
#--------------------

#----- Summarise simulation results

# Aesthetic
dfcol <- scico(2, palette = "imola", end = .8)
dfshp <- 15:16
names(dfcol) <- names(dfshp) <- c("odf", "edf")
parcol <- scico(3, palette = "lipari")
names(parcol) <- levels(simures$par)

# I select only coef 0 to get one value of df and AIC (repeated for each coef)
modres <- subset(simures, coef == 0) 

# Summarise DF results
dfres <- summarise(modres, 

  # ODF average
  dfmean = mean(odf),
  
  # EDF distribution
  edfmean = mean(edf),
  edflow = quantile(edf, .025),
  edfhigh = quantile(edf, .975),

  # Grouping
  .by = all_of(c("mod", "dgm", "n", "s2", "par"))
)

# Extract model selection for each simulation
aicres <- summarise(modres, 
    bestmod0 = mod[which.min(aic0)],
    bestmodE = mod[which.min(aicE)],
    .by = all_of(c("sim", "dgm", "n", "s2", "par"))
  ) |>
  
  # And then compare proportion of each selected model
  summarise(
    odf = sum(bestmod0 == "Constrained"),
    edf = sum(bestmodE == "Constrained"),
    .by = all_of(c("dgm", "n", "s2", "par"))
  )

# Alternative, compare the AIC for each type of model
aicparres <- subset(simures, mod == "Constrained") |>
  
  # Compare AICs for each model
  summarise( 
    bestpar0 = par[which.min(aic0)],
    bestparE = par[which.min(aicE)],
    .by = all_of(c("sim", "dgm", "n", "s2"))
  ) |>
  
  # And then compare proportion of each selected model
  reframe(
    merge(enframe(c(table(bestpar0))), enframe(c(table(bestparE))), 
      by = "name", suffixes = c("0", "E")),
    .by = all_of(c("dgm", "n", "s2"))
  ) |>
  rename(odf = "value0", edf = "valueE")

# Loop on DGM and relevant scenarios for plotting
plot_sel <- foreach(dgml = lablist) %:%
  foreach(sc = iter(unique(scenarios[, names(parlist)]), by = "row")) %do%
{
  #----- DF
  
  # Select scenarios
  ind <- mapply(function(val, nm) dfres[[nm]] == val, sc, names(sc)) |>
    apply(1, all)
  datdf <- dfres[ind,] |>
    subset(dgm == dgml & mod == "Constrained")
  
  # Plot error in estimating standard error
  dfplot <- ggplot(datdf) + theme_bw() + 
    geom_pointrange(aes(x = factor(par), 
      ymin = edflow, ymax = edfhigh, y = edfmean), size = 2, 
      shape = dfshp["edf"], col = dfcol["edf"], linewidth = 2,
      position = position_nudge(x = .1)) +
    geom_point(aes(x = factor(par), y = dfmean), size = 6, 
      shape = dfshp["odf"], col = dfcol["odf"],
      position = position_nudge(x = -.1)) +
    labs(x = "", y = "Degrees of freedom") + 
    theme(panel.grid.minor = element_blank())
  
  #----- Model selection
  
  # Select scenarios
  ind <- mapply(function(val, nm) aicres[[nm]] == val, sc, names(sc)) |>
    apply(1, all)
  dataic <- aicres[ind,] |>
    subset(dgm == dgml) |>
    pivot_longer(cols = ends_with("df"), 
      names_to = "dftype", values_to = "sel") |>
    mutate(sel = 100 * sel / nsim)
    
  # Plot selection
  modselplot <- ggplot(dataic) + theme_bw() + 
    geom_point(aes(x = factor(par), y = sel, group = dftype, 
      col = dftype, shape = dftype), 
      size = 6, position = position_dodge(width = .2)) +
    labs(x = "", y = "Constrained model selected (%)") + 
    scale_color_manual(values = dfcol, name = "DF type") + 
    scale_shape_manual(values = dfshp, name = "DF type") +
    theme(panel.grid.minor = element_blank())
    
  #----- Par selection
  
  # Select scenarios
  ind <- mapply(function(val, nm) aicparres[[nm]] == val, sc, names(sc)) |>
    apply(1, all)
  datpar <- aicparres[ind,] |>
    subset(dgm == dgml) |>
    pivot_longer(cols = ends_with("df"), 
      names_to = "dftype", values_to = "sel") |>
    mutate(sel = 100 * sel / nsim, 
      name = factor(name, levels = levels(simures$par)))
  
  # Plot
  parselplot <- ggplot(datpar) + theme_bw() + 
    geom_col(aes(x = dftype, y = sel, fill = name), 
      position = "stack") +
    labs(x = "", y = "Constrained model selected (%)") + 
    scale_fill_manual(values = parcol, name = "Sub-scenario",
      breaks = names(parcol)) + 
    theme(panel.grid.minor = element_blank())
  
  # Export
  wrap_plots(dfplot, modselplot, parselplot) |>
    ggsave(width = 15, filename = sprintf("figures/df_%s_%s.pdf", 
      dgml, paste(sc, collapse = "_")))
}
