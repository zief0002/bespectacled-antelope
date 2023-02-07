##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(educate) 
library(patchwork)
library(texreg)
library(tidyverse)



##################################################
### Import data
##################################################

carbon = read_csv(file = "https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/carbon.csv")
carbon



##################################################
### Exploration
##################################################

p1 = ggplot(data = carbon, aes(x = co2)) +
  geom_density() +
  theme_bw() +
  xlab("Carbon emissions (metric tons per person)") +
  ylab("Probability density")

# Marginal distribution of wealth (predictor)
p2 = ggplot(data = carbon, aes(x = wealth)) +
  geom_density() +
  theme_bw() +
  xlab("Wealth") +
  ylab("Probability density")

# Scatterplot
p3 = ggplot(data = carbon, aes(x = wealth, y = co2)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Wealth") +
  ylab("Carbon emissions (metric tons per person)") +
  annotate(geom = "text", x = 6.4, y = 38, label = "Quatar", size = 3, hjust = 1) +
  annotate(geom = "text", x = 4.6, y = 31.3, label = "Trinidad and Tobago", size = 3, hjust = 1) 

# Place figures side-by-side
(p1 / p2) |  p3



##################################################
### Fit model and evaluate residuals
##################################################

# Fit model
lm.1 = lm(co2 ~ 1 + wealth, data = carbon)

# Obtain residual plots
residual_plots(lm.1)



##################################################
### Log-transform CO2 vs wealth
##################################################

ggplot(data = carbon, aes(x = wealth, y = log(co2))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Wealth") +
  ylab("ln(Carbon emissions)") +
  annotate(geom = "text", x = 6.4, y = 3.64, label = "Quatar", size = 3, hjust = 1) +
  annotate(geom = "text", x = 4.6, y = 3.44, label = "Trinidad and Tobago", size = 3, hjust = 1) 




##################################################
### Fit regression model and evaluate residuals
##################################################

# Fit model
lm.2 = lm(log(co2) ~ 1 + wealth, data = carbon)

# Obtain residual plots
residual_plots(lm.2)



##################################################
### Interpret output
##################################################

glance(lm.2) |> # Model-level output
  print(width = Inf)

tidy(lm.2)   # Coefficient-level output



##################################################
### Back-transform coefficients
##################################################

coef(lm.2)

# Obtain back-transformed interpretations
exp(coef(lm.2))



##################################################
### Plot the fitted curve
##################################################

ggplot(data = carbon, aes(x = wealth, y = co2)) +
  geom_point(alpha = 0) +
  geom_function(fun = function(x) {exp(2.20) * exp(0.824*x)} ) +
  theme_bw() +
  xlab("Wealth") +
  ylab("Predicted CO2 emissions (in metric tones per person)")



##################################################
### Model remaining non-linearity
##################################################

ggplot(data = carbon, aes(x = wealth, y = log(co2))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Wealth") +
  ylab("ln(Carbon emissions)") +
  annotate(geom = "text", x = 6.4, y = 3.64, label = "Quatar", size = 3, hjust = 1) +
  annotate(geom = "text", x = 4.6, y = 3.44, label = "Trinidad and Tobago", size = 3, hjust = 1) 


# Fit log-log model
lm.log = lm(log(co2) ~ 1 + log(wealth), data = carbon)

# Fit polynomial model
lm.poly = lm(log(co2) ~ 1 + wealth + I(wealth^2), data = carbon)

# Residual plots
p1 = residual_plots(lm.log)
p2 = residual_plots(lm.poly)


# Plot top/bottom
p1 / p2



##################################################
### Relationship between log-budget and running time for both models
##################################################

p1 = ggplot(data = carbon, aes(x = wealth, y = log(co2))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_function(fun = function(x) {log(exp(-1.144) * x^(1.719))},
                color = "red", linetype = "dashed") +
  theme_bw() +
  xlab("Wealth") +
  ylab("ln(Carbon emissions)") +
  ggtitle("Log-Log Model")

# polynomial model
p2 = ggplot(data = carbon, aes(x = wealth, y = log(co2))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_function(fun = function(x) {-2.8994 + 1.3824*x -0.0837*x^2},
                color = "red", linetype = "dashed") +
  theme_bw() +
  xlab("Wealth") +
  ylab("ln(Carbon emissions)")

# Plot side-by-side
p1 | p2



##################################################
### Model evidence
##################################################

# Table of model evidence
aictab(
  cand.set = list(lm.log, lm.poly),
  modnames = c("Log-Log model", "Polynomial model")
)



##################################################
### Polynomial model
##################################################

glance(lm.poly) %>% # Model-level output
  print(width = Inf)

tidy(lm.poly)   # Coefficient-level output



##################################################
### Plot back-transformed fitted curve
##################################################

ggplot(data = carbon, aes(x = wealth, y = co2)) +
  geom_point(alpha = 0) +
  geom_function(fun = function(x) {exp(-2.90) * exp(1.38*x) * exp(-0.0837*x^2)} ) +
  theme_bw() +
  xlab("Wealth") +
  ylab("Predicted CO2 (in metric tons per person)")



##################################################
### Controlling for urbanization
##################################################

# Fit model
lm.3 = lm(log(co2) ~ 1 + wealth + I(wealth^2) + urbanization, data = carbon)


# Table of model evidence
aictab(
  cand.set = list(lm.poly, lm.3),
  modnames = c("Wealth, Wealth^2", "Wealth, Wealth^2, Urbanization")
)


glance(lm.3) |> # Model-level output
  print(width = Inf)

tidy(lm.3)   # Coefficient-level output



##################################################
### Model interpretation
##################################################

# b-fold difference
exp(coef(lm.3))

# Untrustworthy interpretation of percent change
coef(lm.3)

# Actual percent change
abs(1 - exp(1.002587))



##################################################
### Include effects of running time and genre
##################################################

# Fit the model (non-action is reference group)
lm.4 = lm(Lbudget ~ 1 + length + I(length^2) + action, data = movies)

# Likelihood ratio test (partial effect of genre)
lrtest(lm.3, lm.4)

# Likelihood ratio test (partial quadratic effect of running time)
lm.5 = lm(Lbudget ~ 1 + length + action, data = movies)
lrtest(lm.5, lm.4)


glance(lm.4) %>% # Model-level output
  print(width = Inf)

tidy(lm.4)   # Coefficient-level output

# Exponentiate coefficients
exp(coef(lm.4))


##################################################
### Plot the fitted curves
##################################################

ggplot(data = movies, aes(x = length, y = budget)) +
  geom_point(alpha = 0) +
  geom_function(fun = function(x) {exp(-5.56) * exp(0.12*x) * exp(-0.0004*x^2)}, 
                color = "black", linetype = "dashed") +
  geom_function(fun = function(x) {exp(-4.75) * exp(0.12*x) * exp(-0.0004*x^2)}, 
                color = "red", linetype = "solid") +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Predicted budget (in millions of dollars)") 



##################################################
### Evaluate interaction effects b/w running time and genre
##################################################

# Fit the first interaction model
lm.6 = lm(Lbudget ~ 1 + length + I(length^2) + action + length:action, data = movies)

# Fit the second interaction model
lm.7 = lm(Lbudget ~ 1 + length + I(length^2) + action + length:action + I(length^2):action, data = movies)

# Likelihood ratio tests
lrtest(lm.4, lm.6, lm.7)


