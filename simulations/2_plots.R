################################################################################
#
# Simulation study
# 3. Plot results
#
################################################################################

# Get scenarios and reload packages (if new session)
source("simulations/0_scenarios.R")
pacman::p_load(packlist, character.only = TRUE)

#--------------------
# Parameters
#--------------------

#----- Define parameters

# Data-generating mechanisms
dgmlabs <- sapply(dgmlist, "[[", "lab")
names(dgmlabs) <- names(dgmlist)

# Performance measure labels (separated by figure)
measlabs <- list(
  c(bias = expression(paste(Delta, abs(plain(Bias)))), 
    empse = expression(paste(Delta, plain(SE))),
    mse = expression(paste(Delta, plain(RMSE)))),
  c(seerr = "Relative Variance error (%)", cover = "Coverage (%)"),
  c(becover = "Coverage (%)"))
allmeas <- unlist(measlabs)

# Coefficients
coefpal <- scico(11, palette = "oleron", end = .9)
coefpal <- coefpal[-c(2, 3, 5, 6)]
coefpal <- append(coefpal, "white", 2)
names(coefpal) <- c("Main", "Covariate", "(Intercept)", sprintf("Strata %i", 1:5))

# Degrees of freedom
dfcol <- scico(3, palette = "batlowK", end = .7)
names(dfcol) <- c("Actual", "odf", "edf")

# On feasibility
feaslabs <- c("Well-constrained", "Over-constrained")

#--------------------
# Data loading
#--------------------

#----- Load results from simulations

# Coefficients
coefres <- read.csv("simulations/results/coefs.csv")

# Degrees of freedom
dfres <- read.csv("simulations/results/dfs.csv")

# Add simulations
coefres <- left_join(coefres, scenarios)
dfres <- left_join(dfres, scenarios)

#----- Data management

# Change coefficients labels
coefres <- mutate(coefres, 
  
  # For the first DGM
  coef = replace(coef, dgm == "nonneg" & coef == 1, "(Intercept)"),
  coef = replace(coef, dgm == "nonneg" & coef == 2, "Main"),
  coef = replace(coef, dgm == "nonneg" & coef == 3, "Covariate"),
  
  # Second DGM
  coef = ifelse(dgm == "nondecr", paste0("Strata ", coef), coef),
  
  # Transform as factors
  dgm = factor(dgm, names(dgmlabs), dgmlabs),
  coef = factor(coef, names(coefpal))
)

# For DF results
dfres <- mutate(dfres, 
  
  # Factors
  dgm = factor(dgm, names(dgmlabs), dgmlabs),
  df = factor(df, c("o", "e"), c("odf", "edf"))
)

#--------------------
# Figure 1: Bias-variance
#--------------------

#----- Prepare data

# Create a data.frame for plotting
mseres <- coefres |>
  
  # Select coefficients and scenarios
  subset(!coef %in% c("(Intercept)", "Strata 2", "Strata 4") & 
      n == nsel) |>

  # Select performance measures
  select(model, coef, dgm, par, all_of(names(measlabs[[1]]))) |>
  
  # Transform some criteria for easier plotting
  mutate(mse = sqrt(mse), bias = abs(bias))

# Compute difference between constrained and unconstrained
mseres <- pivot_longer(mseres, cols = names(measlabs[[1]]), 
    names_to = "measure") |>
  mutate(measure = factor(measure, names(measlabs[[1]]), measlabs[[1]]))
mseres <- pivot_wider(mseres, names_from = "model", values_from = "value") |>
  mutate(diff = cirls - glm)

# Prepare labels for feasibility
ytxt <- subset(mseres, measure == measure[1]) |>
  summarise(y = max(diff), .by = dgm)
xtxt <- data.frame(txt = feaslabs, x = c(0.1, -0.1), hjust = c(0, 1), 
  measure = mseres$measure[1])
feasdf <- cbind(xtxt[rep(1:2, 2),], ytxt[rep(1:2, each = 2),])

#----- Plot

# Plot outline
plout <- ggplot(mseres) + theme_bw() + 
  facet_grid(rows = vars(measure), scales = "free", switch = "y",
    labeller = label_parsed) +

  # Main delimitations of the plot with labels
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  
  # Scales
  scale_color_manual(name = "", values = coefpal) +
  # scale_alpha_manual(values = c("-1" = 1, "1" = .2), guide = "none") +
  # scale_shape_manual(values = c("-1" = 19, "1" = 20), guide = "none")
  
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
      # alpha = factor(sign(diff))),
      data = ~ subset(.x, dgm == lb)) +
    geom_point(aes(x = par, y = diff, col = coef, group = coef), 
      # alpha = factor(sign(diff))),
      # shape = factor(sign(diff))),
      data = ~ subset(.x, dgm == lb)) +
    
    # And the title
    labs(title = lb)
})

# Put together
wrap_plots(dgmplots, nrow = 1)

# Save
ggsave(sprintf("figures/%s.pdf", ifelse(nsel == 500, "Fig1", "FigA4")), 
  height = 7, width = 8)

#--------------------
# Figure 2: Inference measures
#--------------------

#----- Select data

# Create a data.frame for plotting
infres <- coefres |>
  
  # Select coefficients and scenarios
  subset(!coef %in% c("(Intercept)", "Strata 2", "Strata 4") & 
      n == nsel & model == "cirls") |>

  # Select performance measures
  select(model, coef, dgm, par, all_of(names(measlabs[[2]]))) |>
  
  # Transform some criteria for easier plotting
  # Discard very high values for seerr
  mutate(cover = 100 * cover, seerr = ifelse(seerr > 30, NA, seerr))

# Pivot performance criteria
infres <- pivot_longer(infres, cols = names(measlabs[[2]]), 
    names_to = "measure") |>
  mutate(measure = factor(measure, names(measlabs[[2]]), measlabs[[2]]))

# Prepare labels for feasibility
ytxt <- subset(infres, measure == measure[1]) |>
  summarise(y = max(value, na.rm = T), .by = dgm)
xtxt <- data.frame(txt = feaslabs, x = c(0.1, -0.1), hjust = c(0, 1), 
  measure = infres$measure[1])
feasdf <- cbind(xtxt[rep(1:2, 2),], ytxt[rep(1:2, each = 2),])

# To draw lines
linedf <- data.frame(measure = unique(infres$measure), y = c(0, 95))

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
ggsave(sprintf("figures/%s.pdf", ifelse(nsel == 500, "Fig2", "FigA5")), 
  height = 7, width = 8)


#--------------------
# Figure 3: Degrees of freedom
#--------------------

#----- Prepare data

# Go across all simulations and extract dfs for cirls model
# dfres <- lapply(simures, function(res){
#   data.frame(res$sc, type = c("odf", "edf"), 
#     res$cirls[c("dfbias", "dfse", "dfmse")])
# })
# dfres <- do.call(rbind, dfres) |>
#   mutate(dgm = factor(dgm, names(dgmlabs), dgmlabs))

# Select cirls and n
dfperf <- subset(dfres, model == "cirls" & n == nsel) |>
  
  # Compute RMSE
  mutate(rmse = sqrt(mse))

# Feasibility labels
ytxt <- summarise(dfperf, y = max(sqrt(mse), na.rm = T), .by = dgm)
xtxt <- data.frame(txt = feaslabs, x = c(0.1, -0.1),
  hjust = c(0, 1))
feasdf <- cbind(xtxt[rep(1:2, 2),], ytxt[rep(1:2, each = 2),])

#----- Plot
ggplot(dfperf) + theme_bw() + 
  
  # By DGM
  facet_wrap(vars(dgm), scales = "free") +
  
  # Result curves
  geom_line(aes(x = par, y = rmse, col = df), linewidth = 1) + 
  geom_point(aes(x = par, y = rmse, col = df), size = 2) + 
  
  # Delimitation of feasibility
  geom_vline(xintercept = 0, linetype = 2) +
  geom_label(aes(x = x, y = y, label = txt, hjust = hjust), 
    linewidth = 0, label.padding = unit(0, "mm"), size = 3, 
    data = feasdf) +
  
  # Titles and theme
  scale_color_manual(values = dfcol, name = "") +
  labs(x = expression(Feasibility ~ gamma), y = "df RMSE") + 
  theme(panel.grid.minor = element_blank(),
    strip.placement = "outside", legend.position = "bottom",
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = 15))

# Save
ggsave(sprintf("figures/%s.pdf", ifelse(nsel == 500, "Fig3", "FigA6")), 
  height = 5, width = 9)
