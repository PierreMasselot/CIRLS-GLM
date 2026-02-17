################################################################################
#
# Simulation study
# 3. Plot results
#
################################################################################

# Get results and re-load packages (if new session)
source("simulations/0_scenarios.R")
load("simulations/results.RData")
librarian::shelf(packlist)

#--------------------
# Parameters
#--------------------

#----- Define parameters

# Data-generating mechanisms
dgmlabs <- sapply(dgmlist, "[[", "lab")
names(dgmlabs) <- names(dgmlist)

# Performance measure labels (separated by figure)
measlabs <- list(
  c(bias = "Squared Bias", empse = "Standard error", mse = "RMSE"),
  c(seerr = "Relative Variance error (%)", cover = "Coverage (%)"),
  c(becover = "Coverage (%)"))
allmeas <- unlist(measlabs)

# Coefficients
coefpal <- scico(11, palette = "oleron", end = .9)
coefpal <- coefpal[-c(2, 3, 5, 6)]
coefpal <- append(coefpal, "white", 2)
names(coefpal) <- c("Main", "Covariate", "", sprintf("Strata %i", 1:5))

# Degrees of freedom
dfcol <- scico(3, palette = "batlowK", end = .7)
names(dfcol) <- c("Actual", "odf", "edf")

#----- Extract performances for specific coefficients

# Go across all simulations and extract performance criteria
perfres <- lapply(simures, function(res){
  perf <- imap(res[-1], function(x, idx){
    data.frame(model = idx, coef = names(x$bias), x[names(allmeas)])
  })
  
  # Bind everything into a single data.frame
  cbind(res[[1]], do.call(rbind, perf))
})
perfres <- do.call(rbind, perfres)

# Change labels
perfres <- filter(perfres, !(dgm == "nonneg" & coef == "(Intercept)"),
    !(dgm == "nondecr" & coef == "X2"), 
    !(dgm == "nondecr" & coef == "X4")) |>
  mutate(coef = replace(coef, dgm == "nonneg" & coef == "X1", "Main"),
    coef = replace(coef, dgm == "nonneg" & coef == "X2", "Covariate"),
    coef = ifelse(dgm == "nondecr", gsub("X", "Strata ", coef), coef),
    dgm = factor(dgm, names(dgmlabs), dgmlabs),
    coef = factor(coef, names(coefpal)))

#--------------------
# Figure 1: Bias-variance
#--------------------

#----- Prepare data

# Select the performance measures
mseres <- select(perfres, !all_of(names(unlist(measlabs[-1]))))

# MSE to RMSE and bias to squared bias
mseres <- mutate(mseres, mse = sqrt(mse), bias = bias^2)

# Pivot performance criteria
mseres <- pivot_longer(mseres, cols = names(measlabs[[1]]), 
    names_to = "measure") |>
  mutate(measure = factor(measure, names(measlabs[[1]]), measlabs[[1]]))
mseres <- pivot_wider(mseres, names_from = "model", values_from = "value") |>
  mutate(diff = cirls - glm)

# Prepare labels for feasibility
ytxt <- subset(mseres, measure == measure[1]) |>
  summarise(y = max(diff), .by = dgm)
xtxt <- data.frame(txt = c("Feasible", "Unfeasible"), x = c(0.1, -0.1),
  hjust = c(0, 1), measure = mseres$measure[1])
feasdf <- cbind(xtxt[rep(1:2, 2),], ytxt[rep(1:2, each = 2),])

#----- Plot

# Plot outline
plout <- ggplot(mseres) + theme_bw() + 
  facet_grid(rows = vars(measure), scales = "free", switch = "y") +

  # Main delimitations of the plot with labels
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  
  # Scales
  scale_color_manual(name = "", values = coefpal) +
  
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
    geom_line(aes(x = par, y = diff, col = coef, group = coef),
      data = ~ subset(.x, dgm == lb)) +
    geom_point(aes(x = par, y = diff, col = coef, group = coef),
      data = ~ subset(.x, dgm == lb)) +
    
    # And the title
    labs(title = lb)
})

# Put together
wrap_plots(dgmplots, nrow = 1)

# Save
ggsave("figures/Fig1.pdf", height = 7, width = 8)

#--------------------
# Figure 2: Inference measures
#--------------------

#----- Select data

# Select the performance measures and only cirls
infres <- select(perfres, !all_of(names(unlist(measlabs[-2])))) |>
  subset(model == "cirls")

# Pivot performance criteria
infres <- pivot_longer(infres, cols = names(measlabs[[2]]), 
    names_to = "measure") |>
  mutate(measure = factor(measure, names(measlabs[[2]]), measlabs[[2]]))

# Discard some values for limits
infres <- mutate(infres, value = ifelse(value > 30, NA, value))

# Prepare labels for feasibility
ytxt <- subset(infres, measure == measure[1]) |>
  summarise(y = max(value, na.rm = T), .by = dgm)
xtxt <- data.frame(txt = c("Feasible", "Unfeasible"), x = c(0.1, -0.1),
  hjust = c(0, 1), measure = infres$measure[1])
feasdf <- cbind(xtxt[rep(1:2, 2),], ytxt[rep(1:2, each = 2),])

# To draw lines
linedf <- data.frame(measure = unique(infres$measure), y = c(0, .95))

#----- Create plot of SE error

# Plot outline
plout <- ggplot(infres) + theme_bw() + 
  facet_grid(rows = vars(measure), scales = "free", switch = "y") +

  # Main delimitations of the plot with labels
  geom_hline(aes(yintercept = y), data = linedf) +
  geom_vline(xintercept = 0, linetype = 2) +
  
  # Scales
  scale_color_manual(name = "", values = coefpal) +
  
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
    
    # Add lines and points to plot by selecting data
    geom_line(aes(x = par, y = value, col = coef, group = coef),
      data = ~ subset(.x, dgm == lb)) +
    geom_point(aes(x = par, y = value, col = coef, group = coef),
      data = ~ subset(.x, dgm == lb)) +
    
    # Feasibility labels
    geom_label(aes(x = x, y = y, label = txt, hjust = hjust), 
      linewidth = 0, label.padding = unit(0, "mm"), size = 3, 
      data = subset(feasdf, dgm == lb)) +
    
    # The title and some limits
    labs(title = lb)
})

# Put together
wrap_plots(dgmplots, nrow = 1)

# Save
ggsave("figures/Fig2.pdf", height = 6, width = 8)


#--------------------
# Figure 3: Degrees of freedom
#--------------------

#----- Extract degrees of freedom estimation performances

# Go across all simulations and extract dfs for cirls model
dfres <- lapply(simures, function(res){
  data.frame(res$sc, type = c("odf", "edf"), 
    res$cirls[c("dfbias", "dfse", "dfmse")])
})
dfres <- do.call(rbind, dfres) |>
  mutate(dgm = factor(dgm, names(dgmlabs), dgmlabs))

# Feasibility labels
ytxt <- summarise(dfres, y = max(sqrt(dfmse), na.rm = T), .by = dgm)
xtxt <- data.frame(txt = c("Feasible", "Unfeasible"), x = c(0.1, -0.1),
  hjust = c(0, 1))
feasdf <- cbind(xtxt[rep(1:2, 2),], ytxt[rep(1:2, each = 2),])

#----- Plot
ggplot(dfres) + theme_bw() + 
  
  # By DGM
  facet_wrap(vars(dgm), scales = "free") +
  
  # Result curves
  geom_line(aes(x = par, y = sqrt(dfmse), col = type), linewidth = 1) + 
  geom_point(aes(x = par, y = sqrt(dfmse), col = type), size = 2) + 
  
  # Delimitation of feasibility
  geom_vline(xintercept = 0, linetype = 2) +
  geom_label(aes(x = x, y = y, label = txt, hjust = hjust), 
    linewidth = 0, label.padding = unit(0, "mm"), size = 3, 
    data = feasdf) +
  
  # Titles and theme
  scale_color_manual(values = dfcol, name = "") +
  labs(x = expression(Feasibility ~ gamma), y = "RMSE") + 
  theme(panel.grid.minor = element_blank(),
    strip.placement = "outside", legend.position = "bottom",
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = 15))

# Save
ggsave("figures/Fig3.pdf", width = 10)
