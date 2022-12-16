# Load libraries
library(tidyverse)


# Import data
mammal = read_csv("https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/mammal.csv")
head(mammal)


# Plot
ggplot(data = mammal, aes(x = brain_weight, y = body_weight)) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE)

ggplot(data = mammal, aes(x = brain_weight, y = log(body_weight))) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE)

ggplot(data = mammal, aes(x = log(brain_weight), y = log(body_weight))) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE)


lm.1 = lm(log(body_weight) ~ 1 + log(brain_weight), data = mammal)
library(broom)
glance(lm.1)

tidy(lm.1)
-2.


ggplot(data = mammal, aes(x = brain_weight, y = body_weight)) +
  geom_point(alpha = 0.3, size = 4) +
  geom_function(fun = function(x){exp(-2.51 + 1.22*log(x))}) +
  theme_light()


out1 = augment(lm.1)

ggplot(data = out1, aes(x = .std.resid)) +
  geom_density() +
  educate::stat_density_confidence(model = "normal")

