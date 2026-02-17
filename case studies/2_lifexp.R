################################################################################
#
# Application 2: Gut microbiome data and IBD
# Data from Lu et al. 2019 Biometrics
#
################################################################################

library(dplyr)
library(ggplot2)
library(robCompositions)
library(stringr)
library(cirls)

#-------------------
# Data prep
#-------------------

# Load data
data("lifeExpGdp")

# Create compositional data
compdata <- mutate(lifeExpGdp,
  gdp = dplyr::select(lifeExpGdp, agriculture:other) |> rowSums(),
  across(agriculture:other, ~ .x / gdp))

#-------------------
# Regression
#-------------------

#----- Fit regression model and extract coefs

# Extract design matrix
xlogcomp <- dplyr::select(compdata, agriculture:other) |>
  as.matrix() |>
  log()
xgdp <- scale(compdata$gdp)

# Names of the variables
gdpnames <- colnames(xlogcomp) |> str_to_title()

# Fit regression model for both sexes
reslist <- dplyr::select(compdata, starts_with("lifeExp")) |>
  lapply(function(y){ 
    res <- glm(y ~ xgdp + xlogcomp, method = "cirls.fit", 
      constr = ~ zerosum(xlogcomp))
    data.frame(cat = gdpnames, coef = coef(res)[-(1:2)], confint(res)[-(1:2),])
  })

# Put together
names(reslist) <- gsub("lifeExp", "", names(reslist))
resdf <- bind_rows(reslist, .id = "sex") |>
  mutate(cat = factor(cat, gdpnames))

#----- Plot coefficients

# Create plot
ggplot(resdf) + theme_bw() + 
  geom_pointrange(aes(x = coef, y = cat, xmin = low, xmax = high)) + 
  facet_wrap(~ sex) + 
  geom_vline(xintercept = 0) + 
  scale_y_discrete(name = "", limits = rev) + 
  labs(x = "Coefficient")

# Save
ggsave("figures/fig5.pdf")
