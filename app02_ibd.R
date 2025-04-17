################################################################################
#
# Application 2: Gut microbiome data and IBD
# Data from Lu et al. 2019 Biometrics
#
################################################################################

library(dplyr)
library(ggplot2)

cirlsdir <- "C:/Users/PierreMasselot/OneDrive - London School of Hygiene and Tropical Medicine/Research/Projects/CIRLS/Subprojects/cirls"
devtools::load_all(cirlsdir)

#-------------------
# Data prep
#-------------------

#----- Load and clean

# Load data
ibd <- read.csv("data/ibd.csv")

# Clean response
ibd <- mutate(ibd, X = ifelse(substr(X, 1, 1) == "4", 0, 1)) |>
  rename(ibd = "X") |>
  rename_with( ~ gsub("s__", "", .x))

# Extract response and microbiome info
y <- ibd$ibd
micro <- data.matrix(select(ibd, !ibd))

# Info about species
speciesinfo <- strsplit(colnames(micro), "_") |> 
  do.call(what = rbind) |> 
  as.data.frame() |>
  rename(Genus = "V1", Species = "V2") |>
  mutate(avail = colMeans(micro != 0))
genus <- unique(speciesinfo[,1])

# Overall minimum value for zero filling
minval <- min(micro[micro != 0])

#----- Prepare composition as Lu et al. (2019)

# Threshold to keep species
# cutoff <- sort(speciesinfo$avail, decreasing = T)[10]
cutoff <- .2

# Remove Bacteria with less than 20% non-zero values and fill zero values
comp <- micro[, speciesinfo$avail >= cutoff]
comp <- ifelse(comp == 0, minval / 2, comp)

# Compute composition
comp <- comp / rowSums(comp)

# Take the logarithm
logcomp <- log(comp)

#----- Prepare aggregated composition

# Aggregate by taxon
aggr <- aggregate(t(micro), by = list(genus = speciesinfo[,1]), sum)
aggcomp <- t(aggr[,-1])
colnames(aggcomp) <- aggr[,1]

# Genus info data.frame
genusinfo <- data.frame(genus = aggr[,1], avail = colMeans(aggcomp != 0),
  abund = apply(aggcomp, 2, median))

# Select genera and fill zero values
keepgenus <- genusinfo$abund > 0.1
aggcomp <- aggcomp[, keepgenus]
aggcomp <- ifelse(aggcomp == 0, minval / 2, aggcomp)

# Compute composition
aggcomp <- aggcomp / rowSums(aggcomp)

# Take the logarithm
logaggcomp <- log(aggcomp)

#-------------------
# Fit models
#-------------------

#----- Full composition

# Constraint matrix
compCmat <- t(rep(1, ncol(logcomp)))

# Fit model
compfit <- glm(y ~ logcomp, family = "binomial", method = "cirls.fit", 
  control = list(maxit = 50, Cmat = list(logcomp = compCmat)))

# Confidence intervals
compci <- confint(compfit)

#----- Multiple subcomposition

# # Constraint matrix, composition for the genera with more than one species
# genlist <- names(table(speciesinfo[,1])[table(speciesinfo[,1]) > 1])
# multiCmat <- matrix(0, length(genlist), ncol(logcomp))
# for (i in seq_along(genlist)) multiCmat[i, which(speciesinfo[,1] == genlist[i])] <- 1
# 
# # Fit model
# multifit <- glm(y ~ logcomp, family = "binomial", method = "cirls.fit", 
#   control = list(maxit = 50, Cmat = list(logcomp = multiCmat)))
# 
# # Confidence intervals
# multici <- confint(multifit)

#----- Aggregated composition

# Constraint matrix
aggCmat <- t(rep(1, ncol(logaggcomp)))

# Fit model
aggfit <- glm(y ~ logaggcomp, family = "binomial", method = "cirls.fit",
  control = list(maxit = 50, Cmat = list(logaggcomp = aggCmat)))

# Confidence intervals
aggci <- confint(aggfit)

#-------------------
# Plot results
#-------------------

#----- Put together results

# By model
resdf <- subset(genusinfo, keepgenus, genus) |>
  cbind(coef = coef(aggfit)[-1], aggci[-1,])

# # Add to info data.frame
# resdf <- left_join(genusinfo, resdf)

#----- Plot coefficients

ggplot(resdf) + theme_bw() + 
  geom_pointrange(aes(y = genus, x = coef, xmin = low, xmax = high)) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  scale_y_discrete(limits = rev) + 
  labs(x = "Coefficient", y = "")

# Save
ggsave("figures/figApp2_ibd.pdf")


#-------------------
# Model selection
#-------------------

# # Vector of cutoff points
# cutoffs <- seq(.2, .8, by = .05)
# 
# # Loop on cutoff points
# aics <- sapply(cutoffs, function(x){
#   cat(x, " ")
#   comp <- micro[, speciesinfo$avail >= x]
#   comp <- comp / rowSums(comp)
#   logcomp <- log(comp)
#   Cmat <- t(rep(1, ncol(logcomp)))
#   compfit <- try(glm(y ~ logcomp, family = "binomial", method = "cirls.fit", 
#     control = list(maxit = 50, Cmat = list(logcomp = Cmat))), silent = T)
#   if (inherits(compfit, "try-error")) NA else AIC(compfit)
# })
# 
# plot(cutoffs, aics)