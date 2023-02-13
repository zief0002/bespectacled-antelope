##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(corrr)
library(patchwork)
library(performance)
library(texreg)
library(tidyverse)



##################################################
### Read in data
##################################################

# Read in data and create dummy variable for categorical variables
grad = read_csv(file = "https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/graduation.csv") |>
  mutate(
    got_degree = if_else(degree == "Yes", 1, 0),
    is_firstgen = if_else(first_gen == "Yes", 1, 0),
    is_nontrad = if_else(non_traditional == "Yes", 1, 0),
  )


# View data
grad


# Fit models from previous notes
glm.0 = glm(got_degree ~ 1, data = grad, family = binomial(link = "logit"))
glm.1 = glm(got_degree ~ 1 + act, data = grad, family = binomial(link = "logit"))
glm.2 = glm(got_degree ~ 1 + is_firstgen, data = grad, family = binomial(link = "logit"))


##################################################
### Determine importance of covariates
##################################################

grad |>
  select(got_degree, act, scholarship, ap_courses,is_nontrad) |>
  correlate()



##################################################
### Determine effect of ACT
##################################################

# Obtain the log-odds who obtain a degree for each ACT score
prop_grad = grad |> 
  group_by(act, degree) |> 
  summarize(N = n()) |> 
  mutate(
    Prop = N / sum (N)
  ) |>
  ungroup() |>
  filter(degree == "Yes") |>
  mutate(
    Odds = Prop / (1 - Prop),
    Logits = log(Odds)
  )

# Scatterplot
ggplot(data = prop_grad, aes(x = act, y = Logits)) +
  geom_point(aes(size = N)) +
  geom_smooth(aes(weight = N), method = "loess", se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Log-odds of obtaining a degree")


# Fit non-linear models
glm.1_quad = glm(got_degree ~ 1 + act + I(act^2), data = grad, family = binomial(link = "logit"))
glm.1_log = glm(got_degree ~ 1 + log(act), data = grad, family = binomial(link = "logit"))


# Obtain binned residuals
out.1_quad = binned_residuals(glm.1_quad)
out.1_log = binned_residuals(glm.1_log)


# Binned residual plots
plot(out.1_quad)
plot(out.1_log)


# Model evidence
aictab(
  cand.set = list(glm.1, glm.1_quad, glm.1_log),
  modnames = c("Linear Effect", "Quadratic Effect", "Log-Linear Effect")
)



##################################################
### Quadratic Effect of ACT
##################################################

tidy(glm.1_quad)


# Plot the fitted equation: Odds of obtaining a degree
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {exp(-4.57 + 0.37*x - 0.005*x^2)}
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted odds of obtaining a degree")


# Plot the fitted equation: Probability of obtaining a degree
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {exp(-4.57 + 0.37*x - 0.005*x^2) / (1 + exp(-4.57 + 0.37*x - 0.005*x^2))}
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted probability of obtaining a degree")



##################################################
### Relationship between x and other transformations
##################################################

tidy(glm.1_log)



##################################################
### Log-Linear Effect of ACT
##################################################

# Fit the model
glm.1 = glm(got_degree ~ 1 + act, data = grad, family = binomial(link = "logit"))


# Coefficient-level output
tidy(glm.1)


# Plot the fitted equation: Odds of obtaining a degree
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {exp(-6.94 + 2.50*log(x))}
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted odds of obtaining a degree")



# Plot the fitted equation: Probability of obtaining a degree
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {exp(-6.94 + 2.50*log(x)) / (1 + exp(-6.94 + 2.50*log(x)))}
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted probability of obtaining a degree")



##################################################
### Main effects model: First gen and ln(ACT)
##################################################

# Fit main effects model
glm.3 = glm(got_degree ~ 1 + is_firstgen + log(act), data = grad, family = binomial(link = "logit"))


# Model evidence
aictab(
  cand.set = list(glm.0, glm.2, glm.3),
  modnames = c("Intercept-Only", "First Gen.", "First Gen. + ACT")
)


# Coefficient-level output
tidy(glm.3)


# Plot the fitted equation: Odds of obtaining a degree
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  # Non-first generation students
  geom_function(
    fun = function(x) {exp(-5.91 + 2.07*log(x))},
    linetype = "dashed",
    color = "blue"
  ) +
  # First generation students
  geom_function(
    fun = function(x) {exp(-5.40 + 2.07*log(x))},
    linetype = "solid",
    color = "red"
  )+
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted odds of obtaining a degree")



# Plot the fitted equations: Probability of obtaining a degree
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  # Non-first generation students
  geom_function(
    fun = function(x) {exp(-5.91 + 2.07*log(x)) / (1 + exp(-5.91 + 2.07*log(x)))},
    linetype = "dashed",
    color = "blue"
  ) +
  # First generation students
  geom_function(
    fun = function(x) {exp(-5.40 + 2.07*log(x)) / (1 + exp(-5.40 + 2.07*log(x)))},
    linetype = "solid",
    color = "red"
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted probability of obtaining a degree") +
  ylim(0, 1)



##################################################
### Including Other Covariates in the Model
##################################################

# Log-linear effect of AP courses
glm.4 = glm(got_degree ~ 1 + is_firstgen + log(act) + log(ap_courses + 1), 
            data = grad, family = binomial(link = "logit"))


# Effect of scholarship
glm.5 = glm(got_degree ~ 1 + is_firstgen + log(act) + log(ap_courses + 1) +  log(scholarship + 1), 
            data = grad, family = binomial(link = "logit"))


# Effect of non-traditional student
glm.6 = glm(got_degree ~ 1 + is_firstgen + log(act) + log(ap_courses + 1) + log(scholarship + 1) +
              is_nontrad, data = grad, family = binomial(link = "logit"))


# Evaluate
aictab(
  cand.set = list(glm.2, glm.3, glm.4, glm.5, glm.6),
  modnames = c("FG", "FG + ACT", "FG + ACT + AP", "FG + ACT + AP + Sch",
               "FG + ACT + AP + Sch + NT")
)



##################################################
### Interaction Between ACT Score and First Generation Status
##################################################

# Interaction models
glm.7 = glm(got_degree ~ 1 + is_firstgen + log(act) + log(ap_courses + 1) + log(scholarship + 1) +
              is_nontrad + is_firstgen:log(act), data = grad, family = binomial(link = "logit"))

glm.8 = glm(got_degree ~ 1 + is_firstgen + log(act) + log(ap_courses + 1) + log(scholarship + 1) +
              is_nontrad + is_firstgen:log(act) + is_firstgen:log(ap_courses + 1), 
            data = grad, family = binomial(link = "logit"))

glm.9 = glm(got_degree ~ 1 + is_firstgen + log(act) + log(ap_courses + 1) + log(scholarship + 1) +
              is_nontrad + is_firstgen:log(act) + is_firstgen:log(ap_courses + 1) + 
              is_firstgen:log(scholarship + 1), 
            data = grad, family = binomial(link = "logit"))


glm.10 = glm(got_degree ~ 1 + is_firstgen + log(act) + log(ap_courses + 1) + log(scholarship + 1) +
               is_nontrad + is_firstgen:log(act) + is_firstgen:log(ap_courses + 1) + 
               is_firstgen:log(scholarship + 1) + is_firstgen:is_nontrad, 
             data = grad, family = binomial(link = "logit"))

# Evaluate
aictab(
  cand.set = list(glm.6, glm.7, glm.8, glm.9, glm.10),
  modnames = c("Main Effects", "FG:ACT", "FG:ACT + FG:AP", "FG:ACT + FG:APP + FG:Sch",
               "FG:ACT + FG:APP + FG:Sch + FG:NT")
)



##################################################
### Adopted model
##################################################

# CHeck residuals
out.7 = binned_residuals(glm.7)
plot(out.7)


# Coefficients
tidy(glm.7)


# Plot the fitted equations: Probability of obtaining a degree
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  # Non-first generation students
  geom_function(
    fun = function(x) {exp(-0.66 + 0.54*log(x)) / (1 + exp(-0.66 + 0.5*log(x)))},
    linetype = "dashed",
    color = "blue"
  ) +
  # First generation students
  geom_function(
    fun = function(x) {exp(-3.70 + 1.64*log(x)) / (1 + exp(-3.70 + 1.647*log(x)))},
    linetype = "solid",
    color = "red"
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted probability of obtaining a degree") +
  ylim(0, 1)



##################################################
### Presenting a Table of Logistic Regression Results
##################################################

# NOTE: If you are using quarto, set your caption in the chunk options with tbl-cap:
# In that case, set caption=NULL in the htmlreg() function below.


htmlreg(
  l = list(glm.0, glm.2, glm.3, glm.6, glm.7),
  stars = numeric(0),    #No p-value stars
  digits = 2,
  padding = 20,          #Add space around columns (you may need to adjust this via trial-and-error)
  custom.model.names = c("Model A", "Model B", "Model C", "Model D", "Model E"), 
  custom.coef.names = c("Intercept", "First Generation", "ln(ACT Score)", 
                        "ln(AP Courses + 1)", "ln(Scholarship Amount + 1)", "Non-Traditional Student", 
                        "First Generation x ln(ACT Score)"),
  reorder.coef = c(2:7, 1), #Put intercept at bottom of table
  include.aic = FALSE, #Omit AIC
  include.bic = FALSE, #Omit BIC
  include.nobs = FALSE,  #Omit sample size
  include.loglik = FALSE,   #Omit log-likelihood
  custom.gof.rows = list(
    AICc = c(AICc(glm.0), AICc(glm.2), AICc(glm.3), AICc(glm.6), AICc(glm.7)),
    R2 = (2722.5 - c(NA, 2662.09, 2605.00, 2540.35, 2536.67)) /  2722.5
  ), # Add AICc values
  reorder.gof = c(3, 1, 2),
  caption = "Five candidate models predicting variation in the log-odds of obtaining a degree. The first generation and non-traditional student predictors were dummy-coded.",
  caption.above = TRUE, #Move caption above table
  inner.rules = 1, #Include line rule before model-level output
  outer.rules = 1 , #Include line rules around table
  custom.note = "The $R^2$ value is based on the proportion of reduced deviance from the intercept-only model (Model A)"
)


