##################################################
### Load libraries
##################################################

library(educate)
library(tidyverse)
library(broom)



##################################################
### Import data
##################################################

mammal = read_csv("https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/mammals.csv")
head(mammal)


##################################################
### Plot raw data
##################################################

ggplot(data = mammal, aes(x = brain_weight, y = body_weight)) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE)



##################################################
### Transform using Rule of the Bulge
##################################################


ggplot(data = mammal, aes(x = brain_weight, y = log(body_weight))) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE)



##################################################
### Transform again using Rule of the Bulge
##################################################


ggplot(data = mammal, aes(x = log(brain_weight), y = log(body_weight))) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE)



##################################################
### Fit model
##################################################

# Fit model
lm.1 = lm(log(body_weight) ~ 1 + log(brain_weight), data = mammal)


# Check residuals
residual_plots(lm.1)


# Model-level output
glance(lm.1)


# Coefficient-level output
tidy(lm.1)


# Plot fitted curve
ggplot(data = mammal, aes(x = brain_weight, y = body_weight)) +
  geom_point(alpha = 0.3, size = 4) +
  geom_function(fun = function(x){exp(-2.51 + 1.22*log(x))}) +
  theme_light()




