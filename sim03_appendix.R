################################################################################
#
# Additional plots
#
################################################################################


#--------------------
# Showing true coefficients
#--------------------

# Coefficient labels
coeflabs <- list(
  nonneg_cor = c("Intercept", "Main", "Covariate"),
  survey_fish = c(sprintf("%s strata", c("Lower", "2nd", "Middle", "4th", "Higher")))
)

#----- Compute true coefficients

# Loop through scenarios to generate true coefficients
truecoefs <- foreach(sc = iter(scenarios, by = "row"), .combine = rbind) %do% {
  
  # Get the data-generating function
  dgmfun <- dgmlist[[sc$dgm]]
  
  # Call it with relevant parameters and extract betas
  betas <- do.call(dgmfun, sc[names(sc) %in% formalArgs(dgmfun)])$beta
  data.frame(sc, 
    coef = factor(coeflabs[[sc$dgm]], levels = coeflabs[[sc$dgm]]), 
    true = betas)
}

# Remove some coefficients
truecoefs <- subset(truecoefs, coef != "Intercept") |>
  # And tidy variables
  mutate(dgm = factor(dgm, 
      labels = sprintf("%s) %s", letters[seq_along(unique(dgm))], lablist)))

#----- Plot

ggplot(truecoefs) + theme_bw() + 
  # By DGM
  facet_wrap(~ dgm, scales = "free_x") +
  # Coefficients
  geom_hline(yintercept = 0) +
  geom_point(aes(x = coef, y = true, group = par, col = par)) + 
  geom_line(aes(x = coef, y = true, group = par, col = par, linewidth = dgm),
    show.legend = F) +
  # Scales
  scale_color_gradient2(name = expression(Feasibility ~ gamma), 
    low = "darkred", mid = grey(.9), high = "darkgreen") + 
  scale_linewidth_manual(values = c(NA, 1)) +
  # Labels
  labs(x = "Coefficient", y = "Generated value") + 
  # Theme
  theme(panel.grid.minor = element_blank(),
    strip.placement = "outside", legend.position = "bottom",
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text=element_text(size = 15))

# Save
ggsave("figures/genCoefs.pdf")


#--------------------
# Bias-eliminated coverage
#--------------------

# Go through the DGMs
becoverplots <- imap(inf_spl, function(d, i) ggplot(d) + theme_bw() + 
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
    scale_color_scico_d(name = "", palette = "glasgow", end = .8) + 
    coord_cartesian(ylim = c(0, 100)) +
    # Titles and theme
    labs(x = expression(Feasibility ~ gamma), y = "Coverage (%)",
      title = sprintf("%s) %s", letters[i], unique(d$dgm))) + 
    theme(panel.grid.minor = element_blank(),
      strip.placement = "outside", legend.position = "bottom")
)

# Put together and save
wrap_plots(becoverplots, ncol = 2)
ggsave("figures/BEcoverage.pdf", plot = inferplot, height = 5, width = 10)
