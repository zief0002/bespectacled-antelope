##################################################
### Load libraries
##################################################

library(tidyverse)    #Plotting, wrangling, basically everything
library(broom)        #Fitted regression results, creating residuals
library(corrr)        #Correlations
library(educate)      #Evaluate residual plots; NEED VERSION 0.3.01 (or higher)
library(ggrepel)      #For nice plot labels
library(patchwork)    #For layout with more than one plot





##################################################
### Import data
##################################################

woods = read_csv(file = "https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/woods.csv")


# View data
woods



##################################################
### Distributions of outcome and focal predictor
##################################################

# Density plot of outcome
ggplot(data = woods, aes(x = env_prog_str)) +
  geom_density() +
  theme_light() +
  xlab("Environmental program strength") +
  ylab("Probability density")


# Density plot of focal predictor
ggplot(data = woods, aes(x = corrupt)) +
  geom_density() +
  theme_light() +
  xlab("Corruption") +
  ylab("Probability density")




##################################################
### Evaluate Hypothesis 1
##################################################

# Scatterplot
ggplot(data = woods, aes(x = corrupt, y = env_prog_str)) +
  geom_point(size = 4) +
  theme_light() +
  xlab("Corruption") +
  ylab("Environmental program strength")


# Scatterplot with state names
ggplot(data = woods, aes(x = corrupt, y = env_prog_str)) +
  geom_text(aes(label = state), size = 4) +
  theme_light() +
  xlab("Corruption") +
  ylab("Environmental program strength")



##################################################
### Model 1: Unstandardized
##################################################

# Unstandardized model
lm.1 = lm(env_prog_str ~ 1 + corrupt, data = woods)

# model-level output
glance(lm.1)

# Calculate robust standard errors for model coefficients
tidy(lm.1)



##################################################
### Model 1: Standardized
##################################################

# Function to create standardized variables
std.var = function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


# Use function to standardize variables
woods2 = woods |>
  mutate(
    across(
      .cols = where(is.numeric), #Apply to all numeric columns
      .fns = std.var             #Apply std.var function
      )
  ) 


# Fit standardized regression
lm.1 = lm(env_prog_str ~ 0 + corrupt, data = woods2)


# Model-level output
glance(lm.1)


# Coefficient-level output
tidy(lm.1)

  

##################################################
### Evaluate Hypothesis 1 (re-visited)
##################################################

# Summary Statistics
# Means
woods |>
  summarize_at(vars(env_prog_str:public_env), mean, na.rm = TRUE)

# SDs
woods |>
  summarize_at(vars(env_prog_str:public_env), sd, na.rm = TRUE)

# Minimum
woods |>
  summarize_at(vars(env_prog_str:public_env), min, na.rm = TRUE)

# Maximum
woods |>
  summarize_at(vars(env_prog_str:public_env), max, na.rm = TRUE)



##################################################
### Bivariate Correlations
##################################################

woods |>
  dplyr::select(env_prog_str, corrupt, wealth, toxic_waste, dem_party_control, interparty_comp, public_env) |>
  correlate() |>
  shave() |>
  fashion(decimals = 2)


# Use : to select all columns between env_prog and public_env
woods |>
  dplyr::select(env_prog_str:public_env) |>
  correlate() |>
  shave() |>
  fashion(decimals = 2)  



##################################################
### Fit Model 2 (Main-Effects model with covariates)
##################################################

# Model 2: Standardized
lm.2 = lm(env_prog_str ~ 0 + corrupt + wealth + toxic_waste + dem_party_control + interparty_comp + public_env, data = woods2)


# Model-level output
glance(lm.2)


# Coefficient-level output
tidy(lm.2)


# Evaluate assumptions for Model 2
out_2 = augment(lm.2)


# Density plot of standardized residuals
plot_a = ggplot(data = out_2, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  geom_density() +
  theme_light() +
  xlab("Standardized residuals") +
  ylab("Density")


# Scatterplot of standardized residuals versus fitted values
plot_b = ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", linewidth = 0, fill = "skyblue") + #Conf Envelope (where mean should be)
  geom_smooth(method = "loess", se = FALSE) + #Empirical mean pattern
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals")


# Layout plots side-by-side
plot_a | plot_b




##################################################
### Evaluate Hypothesis 2
##################################################

# Fit Model 3 (standardized)
lm.3 = lm(env_prog_str ~ 0 + corrupt + wealth + toxic_waste + dem_party_control + interparty_comp +
            public_env + manuf_groups + corrupt:manuf_groups, data = woods2)


# Model-level output
glance(lm.3)


# Coefficient-level output
tidy(lm.3)


# Evaluate residuals
# Use residual_plots() function from educate
residual_plots(lm.3)


# Plot interaction effect
ggplot(data = woods2, aes(x = corrupt, y = env_prog_str)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = .38, slope = -.18, color = "#56b4e9", size = 1.5) +
  geom_abline(intercept = -.38, slope = -.60, color = "#0072b2", size = 1.5) +
  theme_light() +
  xlab("Political corruption") +
  ylab("Environmental program strength")



##################################################
### Evaluate Hypothesis 3
##################################################

# Fit model
lm.4 = lm(env_prog_str ~ 0 + corrupt + wealth + toxic_waste + dem_party_control + public_env + interparty_comp  +
            env_groups + corrupt:env_groups, data = woods2)


# Model-level output
glance(lm.4)


# Coefficient-level output
tidy(lm.4)


# Evaluate residuals
residual_plots(lm.4)


