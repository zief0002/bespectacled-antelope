##################################################
### Load libraries
##################################################

library(broom)
library(educate) 
library(gt)
library(lmtest)
library(patchwork)
library(texreg)
library(tidyverse)


##################################################
### Import data
##################################################

mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/mn-schools.csv")
mn



##################################################
### Scatterplot - graduation rate vs. median SAT
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(size = 4) +
  geom_smooth(method = "loess", se = FALSE, color = "#ff2d21") +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Fit linear effect model and look at residuals
##################################################

# Fit linear model
lm.1 = lm(grad ~ 1 + sat, data = mn)


# Residual plot: Scatterplot
residual_plots(lm.1, type = "s")



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

lm.3 = lm(grad ~ 1 + sat_quadratic, data = mn)
lrtest(lm.3, lm.2)

lm.4 = lm(grad ~ 0 + sat + sat_quadratic, data = mn)
lrtest(lm.4, lm.2)


# Model-level output
glance(lm.2) %>%
  print(width = Inf)



##################################################
### Examine residuals from quadratic effects model
##################################################

residual_plots(lm.2)



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
lm.2 = lm(grad ~ 1 + sat + sat ^ 2, data = mn)
tidy(lm.2)


glance(lm.2) # Model-level output
tidy(lm.2)   # Coefficient-level output



##################################################
### Adding covariates: Main effects model
##################################################

# Fit model
lm.2 = lm(grad ~ 1 + sat + I(sat^2),         data = mn)
lm.3 = lm(grad ~ 1 + sat + I(sat^2) + public, data = mn)
lm.4 = lm(grad ~ 1 + sat +            public, data = mn)

# Compare Model 2 and Model 3
lrtest(lm.2, lm.3)
lrtest(lm.4, lm.3)

# Model-level output
glance(lm.3)


# Coefficient-level output
tidy(lm.3)



##################################################
### Plot the fitted curve
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  # Private schools
  geom_function(
    fun = function(x) {-384.16 + 67.04*x - 2.37 * x^2},
    color = "#2ec4b6",
    linetype = "dashed"
  ) +
  # Public schools
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
### Table of likelihood ratio test results
##################################################

# Create data frame of model results
my_models = data.frame(
  model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  df = c(3, 4, 5, 6, 7),
  log_lik = c(-113.55, -109.56, -101.80, -100.28, -100.06),
  lr = c(NA, 54.05, 2344.90, 4.57, 1.25),
  lr_test = c(NA, "$$\\chi^2(1)=7.98,~p=.005$$", "$$\\chi^2(1)=15.51,~p<.001$$", 
              "$$\\chi^2(1)=3.05,~p=.081$$", "$$\\chi^2(1)=0.45,~p=.504$$")
)


# Create table
my_models |>
  gt() |>
  cols_label(
    model = md("*Model*"),
    df = md("*df*"),
    log_lik = md("*Log-likelihood*"),
    lr = md("*LR*"),
    lr_test = md("*LRT*")
  ) |>
  cols_align(
    columns = c(model),
    align = "left"
  ) |>
  cols_align(
    columns = c(df, log_lik, lr, lr_test),
    align = "center"
  ) |>
  fmt_missing(
    missing_text = ""
  )



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
  custom.model.names = c("M1", "M2", "M3", "M4", "M5"),
  custom.coef.names = c("Intercept", "Median SAT score (L)", "Median SAT score (Q)",
                        "Public", "Median SAT score (L) x Public",
                        "Median SAT score (Q) x Public"),
  custom.note = "Note. (L) = Linear effect. (Q) = Quadratic effect.",
  reorder.coef = c(2:6, 1), #Put intercept at bottom of table
  caption.above = TRUE, #Move caption above table
  inner.rules = 1, #Include line rule before model-level output
  outer.rules = 1,  #Include line rules around table
  caption = "Table 2: Coefficients (and standard errors) for five candidate models (M1--M5) predicting variation in six-year graduation rates."
)

