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
  geom_point(alpha = .4) +
  geom_function(fun = function(x) {exp(-2.20) * exp(0.824*x)} ) +
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

residual_plots(lm.3)


glance(lm.3) |> # Model-level output
  print(width = Inf)

tidy(lm.3)   # Coefficient-level output


# PLot fitted curves
ggplot(data = carbon, aes(x = wealth, y = co2)) +
  geom_point(alpha = 0) +
  geom_function(fun = function(x) {.0634 * exp(1.34*x) * exp(-0.08*x^2)}, color = "#56b4e9") + #Low urbanization
  geom_function(fun = function(x) {.054 * exp(1.34*x) * exp(-0.08*x^2)}, color = "#0072b2") +  #High urbanization
  theme_bw() +
  xlab("Wealth") +
  ylab("Predicted CO2 (in metric tons per person)")



##################################################
### RQ3: Do effects vary by world region
##################################################

# Create dummy variables for world region
carbon = carbon |>
  mutate(
    africa = if_else(region == "Africa", 1, 0),
    asia = if_else(region == "Asia", 1, 0),
    americas = if_else(region == "Americas", 1, 0),
    europe = if_else(region == "Europe", 1, 0),
    oceania = if_else(region == "Oceania", 1, 0),
  )

# View data
carbon


# Fit model
lm.4 = lm(log(co2) ~ 1 + wealth + I(wealth^2) + urbanization + 
            asia + africa + americas + europe, 
          data = carbon)


lm.4 = lm(log(co2) ~ 1 + wealth + I(wealth^2) + urbanization + 
            region, 
          data = carbon)


aictab(
  cand.set = list(lm.3, lm.4),
  modnames = c("No world region", "Main effect of region")
)




# Fit model
lm.5 = lm(log(co2) ~ 1 + wealth + I(wealth^2) + urbanization + 
            asia + africa + americas + europe +
            asia:wealth + africa:wealth + americas:wealth + europe:wealth +
            asia:I(wealth^2) + africa:I(wealth^2) + americas:I(wealth^2) + europe:I(wealth^2), 
          data = carbon)


# Table of model evidence
aictab(
  cand.set = list(lm.3, lm.4, lm.5),
  modnames = c("No world region", "Main effect of region", "Interaction between region and wealth")
)






##################################################
### 
##################################################

glance(lm.4) |> # Model-level output
  print(width = Inf)

tidy(lm.4)   # Coefficient-level output


# Load library to include color in caption
library(ggtext)

# Plot
ggplot(data = carbon, aes(x = wealth, y = co2)) +
  geom_point(alpha = 0) +
  geom_function(fun = function(x) {.082 * exp(1.28*x) * exp(-0.07*x^2)}, color = "#1b2a41") + #Asia
  geom_function(fun = function(x) {.067 * exp(1.28*x) * exp(-0.07*x^2)}, color = "#99c24d") + #Africa
  geom_function(fun = function(x) {.053 * exp(1.28*x) * exp(-0.07*x^2)}, color = "#d30c7b") + #Americas
  geom_function(fun = function(x) {.047 * exp(1.28*x) * exp(-0.07*x^2)}, color = "#adcad6") + #Europe
  geom_function(fun = function(x) {.096 * exp(1.28*x) * exp(-0.07*x^2)}, color = "#bc69aa") + #Oceania
  #annotate(geom = "text", x = 6.5, y = 21, label = "Oceania", color = "#bc69aa", hjust = 0) +
  theme_bw() +
  xlab("Wealth") +
  ylab("Predicted CO2 (in metric tons per person)") +
  labs(
    title = "Region: <b style = 'color:#1b2a41;'>Asia</b>, <b style = 'color:#99c24d;'>Africa</b>, <b style = 'color:#d30c7b;'>Americas</b>, <b style = 'color:#adcad6;'>Europe</b>, and <b style = 'color:#bc69aa;'>Oceania</b>. "
  ) +
  theme(
    plot.title = element_textbox_simple()
  )



