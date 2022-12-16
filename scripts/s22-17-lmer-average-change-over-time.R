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

vocabulary = read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/vocabulary.csv")

vocabulary



##################################################
### Convert from wide to long structured data
##################################################

# Convert from wide to long structured data
vocabulary_long = vocabulary %>%
  pivot_longer(cols = vocab_08:vocab_11, names_to = "grade", values_to = "vocab_score") %>%
  arrange(id, grade)

# View data
vocabulary_long

vocabulary %>%
  select(vocab_08:vocab_11) %>%
  correlate()


##################################################
### Spaghetti plot of the individual and mean profiles
##################################################

ggplot(data = vocabulary_long, aes(x = grade, y = vocab_score)) +
  geom_line(aes(group = id), alpha = 0.3) +                      #Add individual profiles
  stat_summary(fun = mean, geom = "line", size = 2, group = 1) + #Add mean profile line
  stat_summary(fun = mean, geom = "point", size = 3) +           #Add mean profile points
  theme_light() +
  scale_x_discrete(
    name = "Grade-level",
    labels = c("8th-grade", "9th-grade", "10th-grade", "11th-grade")
  ) +
  ylab("Vocabulary score")



##################################################
### Unconditional random intercepts model
##################################################

# Fit unconditional random intercepts model
lmer.0 = lmer(vocab_score ~ 1 + (1|id), data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.0, effects = "fixed")


# Obtain random effects
tidy(lmer.0, effects = "ran_vals")


# Obtain student-specific coefficients
tidy(lmer.0, effects = "ran_coefs")



##################################################
### Partitioning unexplained variance
##################################################

# Obtain variance estimates
tidy(lmer.0, effects = "ran_pars")


# Compute variance components
1.35 ^ 2 #Residual (within-student)
1.72 ^ 2 #Intercept (between-student)


# Total unexplained variation
2.96 + 1.82


# Proportion of unexplaind variance
1.8225 / (1.8225 + 2.9584) # Within-student unexplained variance
2.9584 / (1.8225 + 2.9584) # Between-student unexplained variance



##################################################
### Unconditional growth model (categorical grade)
##################################################

# Fit unconditional growth model (RM-ANOVA-like results)
lmer.1 = lmer(vocab_score ~ 1 + grade + (1|id), 
              data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.1, effects = "fixed")


# SD estimates
tidy(lmer.1, effects = "ran_pars")


# Compute variance components
1.791 ^ 2 #Between-student variance
0.899 ^ 2 #Within-Student variance


# Compare to unconditional random intercepts model
(1.8225 - 0.808201) / 1.8225  #Explained variance within-students
(2.9584 - 3.207681) / 2.9584  #Explained variance between-students



##################################################
### RM-ANOVA (Don't use it)
##################################################

# RM-ANOVA violates sphericity in practice
# Compute correlations between repeated measures
vocabulary %>%
  select(vocab_08:vocab_11) %>%
  correlate()



##################################################
### Lookup table
##################################################

# Create lookup table
lookup_table = data.frame(
  grade = c("vocab_08", "vocab_09", "vocab_10", "vocab_11"),
  grade_quant = c(8, 9, 10, 11)
)

# View lookup table
lookup_table



##################################################
### Join data to lookup table
##################################################

# Join the data with the lookup table
vocabulary_long = vocabulary_long %>%
  left_join(lookup_table, by = "grade")

# View data
vocabulary_long



##################################################
### Unconditional growth model (quantitative grade-level predictor)
##################################################

# Fit unconditional growth model
lmer.2 = lmer(vocab_score ~ 1 + grade_quant + (1|id), 
              data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.2, effects = "fixed")


# SD
tidy(lmer.2, effects = "ran_pars")


# Compute variance components
1.784 ^ 2 #Between-student
0.947 ^ 2 #Within-student


# Comparing these values to the unconditional random intercepts model
(1.8225 - 0.896809) / 1.8225  #Explained variance within-students
(2.9584 - 3.182656) / 2.9584  #Explained variance between-students



##################################################
### Unconditional growth model (quantitative grade-level predictor centered on 8th grade)
##################################################

# Fit unconditional growth model with centered grade
lmer.3 = lmer(vocab_score ~ 1 + I(grade_quant-8) + (1|id), 
              data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.3, effects = "fixed")


# SD
tidy(lmer.3, effects = "ran_pars")


# Compute variance components
1.784 ^ 2 #Between-students
0.947 ^ 2 #Within-students


# Comparing them to the unconditional random intercepts model
(1.8225 - 0.896809) / 1.8225  #Explained variance within-students
(2.9584 - 3.182656) / 2.9584  #Explained variance between-students



##################################################
### Student-specific profiles
##################################################

# Get student estimated parameters
tidy(lmer.3, effects = "ran_coefs") %>%
  arrange(as.numeric(level))


# Create plot
ggplot(data = vocabulary_long, aes(x = I(grade_quant-8), y = vocab_score)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 1.41, slope = 0.75, color = "black") +     # Average growth curve
  geom_abline(intercept = 1.80, slope = 0.75, color = "#0072B2") +   # Student 1
  geom_abline(intercept = 1.20, slope = 0.75, color = "#E69F00") +   # Student 2
  theme_light() +
  scale_x_continuous(
    name = "Grade-level",
    breaks = c(0, 1, 2, 3),
    labels = c("0\n(8th)", "1\n(9th)", "2\n(10th)", "3\n(11th)")
  ) +
  ylab("Vocabulary score")



##################################################
### Re-visiting the functional form of unconditional growth model
##################################################

# Quadratic model
lmer.4 = lmer(vocab_score ~ 1 + I(grade_quant-8) + I((grade_quant-8)^2) + (1|id),
              data = vocabulary_long, REML = FALSE)


# Log-linear model
lmer.5 = lmer(vocab_score ~ 1 + log((grade_quant-8) + 1) + (1|id),
              data = vocabulary_long, REML = FALSE)



##################################################
### Table of model-evidence
##################################################

# Model-evidence
aictab(
  cand.set = list(lmer.0, lmer.3, lmer.4, lmer.5),
  modnames = c("No change", "Linear growth", "Quadratic growth", "Log-linear growth")
)



##################################################
### Log-linear model
##################################################

# Coefficient-level output
tidy(lmer.5, effects = "fixed")


# SD
tidy(lmer.5, effects = "ran_pars")


# Compute variance components
1.790 ^ 2 #Between-student
0.907 ^ 2 #Within-student


# Comparing them to the unconditional random intercepts model
(1.8225 - 0.822649) / 1.8225  #Explained variance within-students
(2.9584 - 3.2041) / 2.9584    #Explained variance between-students



##################################################
### Plot mean profile for adopted unconditional growth model
##################################################

ggplot(data = vocabulary_long, aes(x = I(grade_quant-8), y = vocab_score)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {1.21 + 1.67 * log(x + 1)},
    color = "blue"
  ) +
  theme_light() +
  scale_x_continuous(
    name = "Grade-level",
    breaks = c(0, 1, 2, 3),
    labels = c("0\n(8th)", "1\n(9th)", "2\n(10th)", "3\n(11th)")
  ) +
  ylab("Vocabulary score")



##################################################
### Student-specific profiles
##################################################

# Get student estimated parameters
tidy(lmer.5, effects = "ran_coefs") %>%
  arrange(as.numeric(level))


# Create plot
ggplot(data = vocabulary_long, aes(x = I(grade_quant-8), y = vocab_score)) +
  geom_point(alpha = 0) +
  # Average growth curve
  geom_function(
    fun = function(x) {1.21 + 1.67 * log(x + 1)},
    color = "black"
  ) +
  # Student 1
  geom_function(
    fun = function(x) {1.60 + 1.67 * log(x + 1)},
    color = "#0072B2"
  ) +
  # Student 2
  geom_function(
    fun = function(x) {1.00 + 1.67 * log(x + 1)},
    color = "#E69F00"
  ) +
  theme_light() +
  scale_x_continuous(
    name = "Grade-level",
    breaks = c(0, 1, 2, 3),
    labels = c("0\n(8th)", "1\n(9th)", "2\n(10th)", "3\n(11th)")
  ) +
  ylab("Vocabulary score")

