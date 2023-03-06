##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom.mixed) #for tidy, glance, and augment functions for lme4 models
library(corrr)
library(educate)
library(lme4) #for fitting mixed-effects models
library(patchwork)
library(texreg)
library(tidyverse)



##################################################
### Read in data
##################################################

mpls = read_csv("https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/minneapolis.csv")
mpls



##################################################
### Spaghetti plot -- Mean and individual profiles
##################################################

ggplot(data = mpls, aes(x = grade, y = reading_score)) +
  geom_line(aes(group = student_id), alpha = 0.3) +  #Add individual profiles
  stat_summary(fun = mean, geom = "line", size = 2, group = 1, color = "#FF2D21") + #Add mean profile line
  theme_light() +
  xlab("Grade") +
  ylab("Reading Score")



##################################################
### Fit unconditional random-intercepts and unconditional linear growth model
##################################################

# Fit unconditional random-intercepts models
lmer.a = lmer(reading_score ~ 1 + (1 | student_id), data = mpls, REML = FALSE)


#Fit unconditional growth model with centered grade
lmer.b = lmer(reading_score ~ 1 + I(grade-5) + (1 | student_id), data = mpls, REML = FALSE) 


# Coefficient-level output (not displayed)
tidy(lmer.a, effects = "fixed")
tidy(lmer.b, effects = "fixed")

# Variance estimates (not displayed)
tidy(lmer.a, effects = "ran_pars")
tidy(lmer.b, effects = "ran_pars")



##################################################
### Individual profiles in separate facets
##################################################

# Get the mean reading score at each grade
# Need to use this to add the average profile in each facet
global = mpls |>
  group_by(grade) |>
  summarize(
    reading_score = mean(reading_score)
  ) |>
  ungroup()

# Plot individual profiles in different panels
# Add average profile to each panel
ggplot(data = mpls, aes(x = grade, y = reading_score)) +
  geom_line(aes(group = student_id), alpha = 0.3) + #Individual profiles
  geom_line(data = global, size = 2, group = 1, color = "#FF2D21") + #Mean profile
  theme_light() +
  xlab("Grade") +
  ylab("Reading Score")  +
  facet_wrap(~student_id)



##################################################
### Unconditional growth model (random intercepts and slopes)
##################################################

# Fit model
lmer.c = lmer(reading_score ~ 1 + I(grade-5) + (1 + I(grade-5) | student_id), data = mpls, REML = FALSE)


# Coefficient-level output
tidy(lmer.c, effects = "fixed")


# Obtain random effects
tidy(lmer.c, effects = "ran_vals")


# Obtain random effects for Student 1
tidy(lmer.c, effects = "ran_vals") |>
  filter(level == 1)


# Obtain student-specific coefficients for Student 2
tidy(lmer.c, effects = "ran_coefs") |>
  filter(level == 2)


# Obtain variance estimates
tidy(lmer.c, effects = "ran_pars")




##################################################
### Group differences in average growth profile
##################################################


ggplot(data = mpls, aes(x = grade, y = reading_score)) +
  geom_line(aes(group = student_id), alpha = 0.3) +  #Add individual profiles
  stat_summary(fun = mean, geom = "line", size = 2, group = 1, color = "#FF2D21") + #Add mean profile line
  theme_light() +
  xlab("Grade") +
  ylab("Reading Score") +
  facet_wrap(~special_ed)



##################################################
### Include special education status in the model
##################################################

# Create dummy-coded special education status
mpls = mpls |>
  mutate(
    sped = if_else(special_ed == "Yes", 1, 0)
  )


# Fit model
lmer.d = lmer(reading_score ~ 1 + I(grade-5) + sped + (1 + I(grade-5) | student_id), data = mpls, REML = FALSE)


# Coefficient-level output
tidy(lmer.d, effects = "fixed")


# Obtain variance estimates
tidy(lmer.d, effects = "ran_pars")


# Obtain student-specific coefficients for Student 2
tidy(lmer.d, effects = "ran_vals") |>
  filter(level == 1)



##################################################
### Plot of the fitted equations
##################################################

ggplot(data = mpls, aes(x = grade-5, y = reading_score)) +
  geom_point(alpha = 0) +  #Add individual profiles
  geom_abline(intercept = 208, slope = 4.36, color = "#D55E00") + #Non-Sped
  geom_abline(intercept = 192.2, slope = 4.36, color = "#0072B2") + #Sped
  geom_abline(intercept = 190.09, slope = 1.85, color = "#0072B2", linetype = "dashed") + #Student 8 (Sped)
  theme_light() +
  scale_x_continuous(
    name = "Grade",
    breaks = c(0, 1, 2, 3),
    labels = c("5th", "6th", "7th", "8th")
  ) +
  ylab("Reading Score")



##################################################
### Interaction model
##################################################

# Fit model
lmer.e = lmer(reading_score ~ 1 + I(grade-5) + sped + I(grade-5):sped + 
                (1 + I(grade-5) | student_id), data = mpls, REML = FALSE)


# Coefficient-level output
tidy(lmer.e, effects = "fixed")


# Obtain variance estimates
tidy(lmer.e, effects = "ran_pars")


# Plot of the fitted equations
ggplot(data = mpls, aes(x = grade-5, y = reading_score)) +
  geom_point(alpha = 0) +  #Add individual profiles
  geom_abline(intercept = 209, slope = 4.11, color = "#D55E00") + #Non-Sped
  geom_abline(intercept = 189.6, slope = 6.00, color = "#0072B2") + #Sped
  theme_light() +
  scale_x_continuous(
    name = "Grade",
    breaks = c(0, 1, 2, 3),
    labels = c("5th", "6th", "7th", "8th")
  ) +
  ylab("Reading Score")



##################################################
### Comntrolling for attendance
##################################################

# Fit model
lmer.f = lmer(reading_score ~ 1 + I(grade-5) + sped + I(attendance - 0.9564) + I(grade-5):sped + 
                (1 + I(grade-5) | student_id), data = mpls, REML = FALSE)


# Coefficient-level output
tidy(lmer.f, effects = "fixed")


# Obtain variance estimates
tidy(lmer.f, effects = "ran_pars")



##################################################
### Adopting a model
##################################################

aictab(
  cand.set = list(lmer.a, lmer.b, lmer.c, lmer.d, lmer.e, lmer.f),
  modnames = c("Model A", "Model B", "Model C", "Model D", "Model E", "Model F")
)



##################################################
### Modify the HTML and CSS syntax to make a better table
##################################################

# Obtain HTML syntax
htmlreg(
  l = list(lmer.a, lmer.b, lmer.c, lmer.d, lmer.e, lmer.f),
  stars = numeric(0),    #No p-value stars
  digits = 2,
  padding = 20, #Add space around columns (you may need to adjust this via trial-and-error)
  custom.model.names = c("Model A", "Model B", "Model C", 
                         "Model D", "Model E", "Model F"), 
  custom.coef.names = c("Intercept", "Grade", "Special Education Status", 
                        "Attendance", "Grade x Special Education Status"),
  reorder.coef = c(2:5, 1), #Put intercept at bottom of table
  include.loglik = FALSE, #Omit log-likelihood
  include.aic = FALSE,    #Omit AIC
  include.bic = FALSE,    #Omit BIC
  include.nobs = FALSE,   #Omit sample size
  include.groups = FALSE, #Omit group size
  include.deviance = TRUE,
  custom.gof.names = c("Deviance", "Level-2 (Intercept)", "Level-1", "Level-2 (Slope)", "Level-2 Covariance"), # Rename variance component rows
  custom.gof.rows = list(
    AICc = c(AICc(lmer.a), AICc(lmer.b), AICc(lmer.c), AICc(lmer.d), AICc(lmer.e), AICc(lmer.f))  # Add AICc values
  ), 
  reorder.gof = c(3, 5, 6, 4, 2, 1),
  caption = "Taxonomy of models predicting longitudinal variation in students' reading scores.",
  caption.above = TRUE, #Move caption above table
  inner.rules = 1, #Include line rule before model-level output
  outer.rules = 1 , #Include line rules around table
  custom.note = "*Note.* The grade predictor was centered at the initial measurement occasion of 5th-grade. Special education status was dummy-coded using non-special education students as the reference group. The attendance variable was mean centered."
)


# Add new syntax
<tr style="border-top: 1px solid #000000;">
  <td style="text-align: center; padding-top:5px; padding-bottom:10px;" colspan="7">Fixed-effects
</tr>


