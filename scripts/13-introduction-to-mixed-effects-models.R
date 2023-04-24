##################################################
### Load libraries
##################################################

library(broom.mixed) #for tidy, glance, and augment for lmer models
library(educate)
library(lme4) #for fitting mixed-effects models
library(patchwork)
library(tidyverse)



##################################################
### Read in data
##################################################

# Import data
mpls = read_csv(file = "https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/minneapolis.csv")

# View data
mpls



##################################################
### Fit fixed-effects model
##################################################

# Fit fixed-effects model
lm.1 = lm(reading_score ~ 1 + grade, data = mpls)

# Model-level output
glance(lm.1)

# Coefficient-level output
tidy(lm.1)



##################################################
### Examine residuals
##################################################

# Obtain the augmented data frame
out = augment(lm.1)

# Scatterplot of standardized residuals vs. fitted values
p1 = ggplot(data = out, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  geom_density() +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")

# Density plot of standardized residuals
p2 = ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals") + 
  ylim(-4, 2)

# Display plots side-by-side
p1 | p2



##################################################
### Examine whether residuals are independent within students
##################################################

# Mutate on student ID and draw random sample
out = out |>
  mutate(student_id = mpls$student_id) 

### Show residuals by student
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Studentized residuals") +
  facet_wrap(~student_id)



##################################################
### Conceptual idea of mixed-effects models
##################################################

# Fit student models
student_models = mpls |>
  group_by(student_id) |>
  summarize(
    tidy(lm(reading_score  ~ 1 + grade))
  ) |>
  ungroup()


# View coefficients from fitted models
student_models



##################################################
### Fit mixed-effects model in practice
##################################################

# Fit mixed-effects regression model
lmer.1 = lmer(reading_score ~ 1 + grade + 
                (1 | student_id), data = mpls)


# Display fixed-effects
tidy(lmer.1, effects = "fixed")


# Display random-effects
tidy(lmer.1, effects = "ran_vals")



##################################################
### Random effect for intercept AND grade level
##################################################

# Fit mixed-effects regression model
lmer.2 = lmer(reading_score ~ 1 + grade + 
                (1 + grade | student_id), data = mpls)


# Display fixed-effects
tidy(lmer.2, effects = "fixed")


# Display random-effects (arrange by ID)
tidy(lmer.2, effects = "ran_vals") |>
  arrange(level)

# Display random-effects for Student 13
tidy(lmer.2, effects = "ran_vals") |>
  filter(level == 13)


# Display random-effects for all students
tidy(lmer.2, effects = "ran_vals") |>
  print(n = Inf)




