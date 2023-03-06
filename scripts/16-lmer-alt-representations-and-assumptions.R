##################################################
### Load libraries
##################################################

library(broom.mixed)
library(educate)
library(lme4) #for fitting mixed-effects models
library(patchwork)
library(tidyverse)



##################################################
### Import data
##################################################

mpls = read_csv("https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/minneapolis.csv")
mpls



##################################################
### Fit Model 2
##################################################

lmer.2 = lmer(reading_score ~ 1 + I(grade-5) + special_ed + 
                (1 + I(grade-5) | student_id), data = mpls, REML = FALSE)




##################################################
### Augment the model to get the Level-1 residuals and fitted values
##################################################

out_2 = augment(lmer.2)


# View augmented data
out_2



##################################################
### Plots of the level-1 residuals
##################################################

# Density plot of the level-1 residuals
p1 = ggplot(data = out_2, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-1 residuals")


# Scatterplot of the Level-1 residuals versus the fitted values
p2 = ggplot(data = out_2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


# Plot side-by-side
p1 | p2



##################################################
### Density plot of the random-effects
##################################################

# Obtain a data frame of the random-effects
level_2 = tidy(lmer.2, effects = "ran_vals")


# View random-effects
level_2


# Density plot of the RE for intercept
ggplot(data = level_2, aes(x = estimate)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals") +
  facet_wrap(~term)



##################################################
### Drop random-effect of grade level
##################################################

# Fit model
lmer.2_0 = lmer(reading_score ~ 1 + I(grade-5) + special_ed + 
                (1 | student_id), data = mpls, REML = FALSE)


# Obtain Level-1 residuals and fitted vzalues
out_2_0 = augment(lmer.2_0)


# Obtain Level-2 residuals
level_2 = tidy(lmer.2, effects = "ran_vals")


# Density plot of the level-1 residuals
p1 = ggplot(data = out_2_0, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-1 residuals")


# Scatterplot of the Level-1 residuals versus the fitted values
p2 = ggplot(data = out_2_0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


# Density plot of the RE for intercept
p3 = ggplot(data = level_2, aes(x = estimate)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals")


# Plot side-by-side
p1 | p2 | p3



##################################################
### Log-transform the outcome
##################################################

# Fit model
lmer.3 = lmer(log(reading_score) ~ 1 + I(grade-5) + special_ed + 
                (1 | student_id), data = mpls, REML = FALSE)

# Augment model
out_3 = augment(lmer.3)


# Obtain a data frame of the random-effects
level_2 = tidy(lmer.3, effects = "ran_vals")


# Density plot of the level-1 residuals
p1 = ggplot(data = out_3, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-1 residuals")


# Scatterplot of the Level-1 residuals versus the fitted values
p2 = ggplot(data = out_3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


# Density plot of the level-2 residuals (RE of intercept)
p3 = ggplot(data = level_2, aes(x = estimate)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals")

# Plot side-by-side
p1 | p2 | p3


# Interpret coefficients
tidy(lmer.3)


# Back-transform the coefficients
exp(fixef(lmer.3))


