##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(educate) 
library(lmtest)
library(patchwork)
library(texreg)
library(tidyverse)


##################################################
### Import data
##################################################

mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/mn-schools.csv")
head(mn)



##################################################
### Scatterplot - graduation rate vs. median SAT
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(size = 4) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Fit linear effect model and look at residuals
##################################################

# Fit linear model
lm.1 = lm(grad ~ 1 + sat, data = mn)


# Obtain residuals
out = augment(lm.1)


# Examine residuals for linearity
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Fit quadratic effect model
##################################################

# Create quadratic term in the data
mn = mn %>%
  mutate(
    sat_quadratic = sat ^ 2
  )


# View data
head(mn)


# Fit quadratic model
lm.2 = lm(grad ~ 1 + sat + sat_quadratic, data = mn)

tidy(lm.2)



# Likelihood ratio test to compare linear and quadratic models
lrtest(lm.1, lm.2)

aictab(
  cand.set = list(lm.1, lm.2), 
  modnames = c("Linear", "Quadratic")
)

# Model-level output
glance(lm.2) %>%
  print(width = Inf)



##################################################
### Examine residuals from quadratic effects model
##################################################

# Obtain residuals
out_2 = augment(lm.2)


# Examine residuals for normality (linear)
p1 = ggplot(data = out_2, aes(x = .std.resid)) +
  stat_density_confidence() +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Standardized residuals") +
  ylab("Probability density")


# Examine residuals for linearity
p2 = ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals")


# Display plots side-by-side
p1 | p2



##################################################
### Plot the fitted curve
##################################################

# Coefficient-level output
tidy(lm.2)


# Scatterplot
ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  geom_function(
    fun = function(x) {-366.34 + 62.72*x - 2.15 * x^2},
    color = "red"
    ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  geom_function(fun = compute_grad(x)) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Find vertex
##################################################

# x-coordinate
- 62.722 / (2 * -2.15)

# y-coordinate
-366.34 + 62.72 * 14.58 - 2.15 * 14.58^2



##################################################
### Alternative syntax to fit quadratic model
##################################################

# Fit model using I() function
lm.2 = lm(grad ~ 1 + sat + I(sat ^ 2), data = mn)

glance(lm.2) # Model-level output
tidy(lm.2)   # Coefficient-level output



##################################################
### Adding covariates: Main effects model
##################################################

# Fit model
lm.3 = lm(grad ~ 1 + sat + I(sat^2) + public, data = mn)


# Compare Model 2 and Model 3
lrtest(lm.2, lm.3)


# Model-level output
glance(lm.3)


# Coefficient-level output
tidy(lm.3)



##################################################
### Plot the fitted curve
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  # Public schools
  geom_function(
    fun = function(x) {-384.16 + 67.04*x - 2.37 * x^2},
    color = "#2ec4b6",
    linetype = "dashed"
  ) +
  # Private schools
  geom_function( 
    fun = function(x) {-393.29 + 67.04*x - 2.37 * x^2},
    color = "#ff9f1c",
    linetype = "solid"
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Interaction models
##################################################

# Interaction between sector and linear effect of SAT
lm.4 = lm(grad ~ 1 + sat + I(sat^2) + public + public:sat, data = mn)


# Interaction between sector and linear and quadratic effects of SAT
lm.5 = lm(grad ~ 1 + sat + I(sat^2) + public + public:sat + public:I(sat^2), data = mn)


# Likelihood ratio tests
lrtest(lm.3, lm.4, lm.5)



##################################################
### Summarizing Model 4
##################################################

# Model-level output
glance(lm.4)


# Coefficient-level output
tidy(lm.4)


# Plot of the fitted model
ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  # Public schools
  geom_function(
    fun = function(x) {-378.73 + 67.54 * x - 2.54 * x^2},
    color = "#2ec4b6",
    linetype = "dashed"
  ) +
  # Private schools
  geom_function(
    fun = function(x) {-413.80 + 71.65 * x - 2.54 * x^2},
    color = "#ff9f1c",
    linetype = "solid"
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Model selection with information criteria
##################################################

aictab(
  cand.set = list(lm.1, lm.2, lm.3, lm.4, lm.5),
  modnames = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
)



##################################################
### Table of likelihood ratio test results
##################################################

data.frame(
  model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  df = c(3, 4, 5, 6, 7),
  log_lik = c(-113.55, -109.56, -101.80, -100.28, -100.06),
  lr = c(NA, 54.05, 2344.90, 4.57, 1.25),
  lr_test = c(NA, "$\\chi^2(1)=7.98$, $p=.005$", "$\\chi^2(1)=15.51$, $p<.001$", "$\\chi^2(1)=3.05$, $p=.081$", "$\\chi^2(1)=0.45$, $p=.504$")
) %>%
  kable(
    caption = "Results from a set of likelihood ratio tests (LRT) to compare a set of nested candidate models.",
    col.names = c("Model", "*df*", "Log-likelihood", "*LR*", "*LRT*"),
    align = c("l", rep("c", 5)),
    table.attr = "style='width:80%;'",
    escape = FALSE
  )  %>%
  kable_styling()



##################################################
### Table of regression coefficents, SEs, model-level summatries
##################################################

htmlreg(
  l = list(lm.1, lm.2, lm.3, lm.4, lm.5),
  stars = numeric(0),    #No p-value stars
  digits = 2,
  padding = 20,          #Add space around columns (you may need to adjust this via trial-and-error)
  include.adjrs = FALSE, #Omit Adjusted R^2
  include.nobs = FALSE,  #Omit sample size
  include.rmse = TRUE,   #Include RMSE
  custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), 
  custom.coef.names = c("Intercept", "Median SAT score (Linear)", "Median SAT score (Quadratic)", 
                        "Public", "Median SAT score (Linear) x Public", 
                        "Median SAT score (Quadratic) x Public"),
  reorder.coef = c(2:6, 1), #Put intercept at bottom of table
  custom.gof.rows = list(AICc = c(AICc(lm.1), AICc(lm.2), AICc(lm.3), AICc(lm.4), AICc(lm.5))), # Add AICc values
  caption = "Coefficients (standard errors) for five candidate models predicting variation in six-year graduation rates.",
  caption.above = TRUE, #Move caption above table
  inner.rules = 1, #Include line rule before model-level output
  outer.rules = 1  #Include line rules around table
)

