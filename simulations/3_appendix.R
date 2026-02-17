################################################################################
#
# Simulation study
# 4. Appendices
#
################################################################################


#--------------------
# Showing true coefficients
#--------------------

# Coefficient labels
coeflabs <- list(
  nonneg = c("Intercept", "Main", "Covariate"),
  nondecr = c(sprintf("Strata %i", 1:5))
)

#----- Compute true coefficients

# Loop through scenarios to generate true coefficients
truecoefs <- foreach(sc = iter(scenarios, by = "row"), .combine = rbind) %do% {
  
  # Get the data-generating function
  dgmfun <- dgmlist[[sc$dgm]]
  
  # Call it with relevant parameters and extract betas
  betas <- dgmfun$specBeta(sc$par)
  data.frame(sc, 
    coef = factor(coeflabs[[sc$dgm]], levels = coeflabs[[sc$dgm]]), 
    true = unlist(betas))
}

# Remove some coefficients
truecoefs <- subset(truecoefs, coef != "Intercept") |>
  # And tidy variables
  mutate(dgm = factor(dgm, 
      labels = sprintf("%s) %s", letters[seq_along(unique(dgm))], 
        sapply(dgmlist, "[[", "lab"))))

#----- Plot

ggplot(truecoefs) + theme_bw() + 
  # By DGM
  facet_wrap(~ dgm, scales = "free") +
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
ggsave("figures/app_genCoefs.pdf", width = 10)


#--------------------
# Bias-eliminated coverage
#--------------------

# Select the performance measures and only cirls
beres <- select(perfres, !all_of(names(unlist(measlabs[-3])))) |>
  subset(model == "cirls")

#----- Create plot of Bias-eliminated coverage

# Plot outline
ggplot(beres) + theme_bw() + 
  facet_wrap(vars(dgm)) +

  # Main delimitations of the plot with labels
  geom_hline(yintercept = .95) +
  geom_vline(xintercept = 0, linetype = 2) +
  
  # Add lines and points
  geom_line(aes(x = par, y = becover, col = coef, group = coef)) +
  geom_point(aes(x = par, y = becover, col = coef, group = coef)) +
  
  # Feasibility labels
  geom_label(x = 0.1, y = 0, label = "Feasible", hjust = 0, 
    linewidth = 0, label.padding = unit(0, "mm")) +
  geom_label(x = -0.1, y = 0, label = "Unfeasible", hjust = 1, 
    linewidth = 0, label.padding = unit(0, "mm")) +
  
  # Scales
  scale_color_manual(name = "", values = coefpal) +
  
  # Titles and theme
  labs(x = expression(Feasibility ~ gamma), y = "") + 
  theme(panel.grid.minor = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.position = "bottom")

# Save
ggsave("figures/app_BEcoverage.pdf", height = 5, width = 10)


#--------------------
# Other figures for degrees of freedom
#--------------------

#----- Data wrangling

# Labels
dflabs <- c(dfbias = "Bias", dfse = "Standard error")

# Pivot performance criteria (removing mse)
dfres2 <- select(dfres, !dfmse) |>
  pivot_longer(cols = names(dflabs), names_to = "measure") |>
  mutate(measure = factor(measure, names(dflabs), dflabs))

# Prepare labels for feasibility
ytxt <- subset(dfres2, measure == measure[1]) |>
  summarise(y = max(value), .by = dgm)
xtxt <- data.frame(txt = c("Feasible", "Unfeasible"), x = c(0.1, -0.1),
  hjust = c(0, 1), measure = dfres2$measure[1])
feasdf <- cbind(xtxt[rep(1:2, 2),], ytxt[rep(1:2, each = 2),])

#----- Plot

# Plot outline
plout <- ggplot(dfres2) + theme_bw() + 
  facet_grid(rows = vars(measure), scales = "free", switch = "y") +

  # Main delimitations of the plot with labels
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  
  # Scales
  scale_color_manual(name = "", values = dfcol) +
  
  # Titles and theme
  labs(x = expression(Feasibility ~ gamma), y = "") + 
  theme(panel.grid.minor = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.position = "bottom")

# Create plots for each DGM
dgmplots <- lapply(dgmlabs, function(lb){

  plout + 
    
    # Feasibility labels
    geom_label(aes(x = x, y = y, label = txt, hjust = hjust), 
      linewidth = 0, label.padding = unit(0, "mm"), size = 3, 
      data = subset(feasdf, dgm == lb)) +
    
    # Add lines and points to plot by selecting data
    geom_line(aes(x = par, y = value, col = type, group = type),
      data = ~ subset(.x, dgm == lb)) +
    geom_point(aes(x = par, y = value, col = type, group = type),
      data = ~ subset(.x, dgm == lb)) +
    
    # And the title
    labs(title = lb)
})

# Put together
wrap_plots(dgmplots, nrow = 1)

# Save
ggsave("figures/app_dfres.pdf", height = 6, width = 8)