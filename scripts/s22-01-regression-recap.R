##################################################
### Load libraries
##################################################

library(tidyverse)    #Plotting, wrangling, basically everything
library(broom)        #Fitted regression results, creating residuals
library(corrr)        #Correlations
library(educate)      #Normal confidence envelope
library(patchwork)    #For layout with more than one plot
library(modelsummary) #Summary of variables
library(texreg)       #Creating table of regression results



##################################################
### Import data
##################################################

broadband = read_csv("https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/broadband.csv")

# View data
broadband



##################################################
### Explore data
##################################################

datasummary_skim(broadband)



##################################################
### Plotting relationships
##################################################

# Plot 1: Simple relationship?
ggplot(data = broadband, aes(x = pct_poverty, y = fcc_diff)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_light() +
  xlab("Perecntage in Poverty") +
  ylab("Over/Under Estimate of High Speed Broadband Access")


# Plot 2: Interaction b/w poverty and county type?
ggplot(data = broadband, aes(x = pct_poverty, y = fcc_diff)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_light() +
  xlab("Percentage in Poverty") +
  ylab("Over/Under Estimate of High Speed Broadband Access") +
  facet_wrap(~county_type, nrow = 1)


# Plot 3: Interaction between poverty and region?
ggplot(data = broadband, aes(x = pct_poverty, y = fcc_diff)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_light() +
  xlab("Percentage in Poverty") +
  ylab("Over/Under Estimate of High Speed Broadband Access") +
  facet_wrap(~region)


# Plot 4: Interaction between poverty, county type, and region?
ggplot(data = broadband, aes(x = pct_poverty, y = fcc_diff, color = county_type)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_light() +
  scale_color_manual(
    name = "",
    values = c("#2C6DAC", "#C60F7B")
  ) +
  xlab("Percentage in Poverty") +
  ylab("Over/Under Estimate of High Speed Broadband Access") +
  facet_wrap(~region)



##################################################
### Create dummy variables
##################################################

broadband = broadband %>%
  mutate(
    # Create metro dummy
    metro = if_else(county_type == "Metro", 1, 0),
    # Create region dummies
    ne = if_else(region == "New England", 1, 0),
    ma = if_else(region == "Mid-Atlantic", 1, 0),
    enc = if_else(region == "East North Central", 1, 0),
    wnc = if_else(region == "West North Central", 1, 0),
    mtn = if_else(region == "Mountain", 1, 0),
    pac = if_else(region == "Pacific", 1, 0),
    sa = if_else(region == "South Atlantic", 1, 0),
    esc = if_else(region == "East South Central", 1, 0),
    wsc = if_else(region == "West South Central", 1, 0)
  )


# View data
broadband



##################################################
### Model A
##################################################

# Fit model
model_a = lm(fcc_diff ~ 1 + pct_poverty, data = broadband)


# Model-level output
glance(model_a)


# Coefficient-level output
tidy(model_a)



##################################################
### Model B
##################################################

# Fit model
model_b = lm(fcc_diff ~ 1 + pct_poverty + metro + enc + esc + 
               ma + mtn + ne + pac + sa + wnc, data = broadband)


# Model-level output
glance(model_b)


# Coefficient-level output
tidy(model_b)


# Computational convenience
model_b = lm(fcc_diff ~ 1 + pct_poverty + metro + region, data = broadband)
tidy(model_b)


##################################################
### Model C
##################################################

# Fit model
model_c = lm(fcc_diff ~ 1 + pct_poverty + metro + region + 
               pct_poverty:metro, data = broadband)


# Model-level output
glance(model_c)


# Coefficient-level output
tidy(model_c)



##################################################
### Model D
##################################################

# Fit model
model_d = lm(fcc_diff ~ 1 + pct_poverty + metro + region + 
               pct_poverty:region, data = broadband)


# Model-level output
glance(model_d)


# Coefficient-level output
tidy(model_d)



##################################################
### Model E
##################################################

# Fit model
model_e = lm(fcc_diff ~ 1 + pct_poverty + metro + region + 
               pct_poverty:metro + pct_poverty:region + metro:region +
               region:metro:pct_poverty, data = broadband)


# Model-level output
glance(model_e)


# Coefficient-level output
tidy(model_e)



##################################################
### Residual analysis: Density plots
##################################################

# Get the residuals
out_b = augment(model_b)
out_c = augment(model_c)
out_d = augment(model_d)
out_e = augment(model_e)

# Plots
plot_b = ggplot(data = out_b, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  geom_density() +
  theme_light() +
  xlab("Standardized residuals") +
  ylab("Density") +
  ggtitle("Model B")

plot_c = ggplot(data = out_c, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  geom_density() +
  theme_light() +
  xlab("Standardized residuals") +
  ylab("Density") +
  ggtitle("Model C")

plot_d = ggplot(data = out_d, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  geom_density() +
  theme_light() +
  xlab("Standardized residuals") +
  ylab("Density") +
  ggtitle("Model D")

plot_e = ggplot(data = out_e, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  geom_density() +
  theme_light() +
  xlab("Standardized residuals") +
  ylab("Density") +
  ggtitle("Model E")

# Layout the plots using patchwork
(plot_b | plot_c) / (plot_d | plot_e)



##################################################
### Residual analysis: Scatterplot
##################################################

plot_b = ggplot(data = out_b, aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  ggtitle("Model B")

plot_c = ggplot(data = out_c, aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  ggtitle("Model C")

plot_d = ggplot(data = out_d, aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  ggtitle("Model D")

plot_e = ggplot(data = out_e, aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  ggtitle("Model E")

# Layout the plots
(plot_b | plot_c) / (plot_d | plot_e)



##################################################
### Table of regression results
##################################################

htmlreg(
  l = list(model_a, model_b, model_c),
  stars = numeric(0),    #No p-value stars
  digits = 3,
  padding = 20,          #Add space around columns (you may need to adjust this via trial-and-error)
  include.adjrs = FALSE, #Omit Adjusted R^2
  include.nobs = FALSE,  #Omit sample size
  include.rmse = TRUE,   #Include RMSE
  custom.model.names = c("Model A", "Model B", "Model C"), 
  custom.coef.names = c("Intercept", "Percentage in poverty", "Metropolitan county$^\\dagger$",
                        "East South Central", "Mid-Atlantic", "Mountain", "New England",
                        "Pacific", "South Atlantic", "West North Central", "West South Central",
                        "Percentage in poverty x Metropolitan county"),
  reorder.coef = c(2:12, 1), #Put intercept at bottom of table
  caption = "Coefficients (standard errors) for the predictors from a taxonomy of regression models fitted to explain variation in the error in highspeed broadband estimates. Each model was fitted with 3103 cases.",
  caption.above = TRUE, #Move caption above table
  custom.note = "$^\\dagger$Metropolitan county is a dummy-coded indicator (0 = Non-metroplitan; 1 = Metropolitan county). $^\\dagger\\dagger$Region is a set of eight dummy-coded indicators (reference group = East North Central).",
  groups = list("Region$^\\dagger\\dagger$" = 3:10),
  inner.rules = 1, #Include line rule before model-level output
  outer.rules = 1  #Include line rules around table
)



##################################################
### Plot of the Fitted Lines
##################################################

plot_b = ggplot(data = broadband, aes(x = pct_poverty, y = fcc_diff)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 0.441, slope = 0.00358, linetype = "dashed") +  #Non-metro
  geom_abline(intercept = 0.441 - 0.00838, slope = 0.00358, linetype = "solid") +  #Metro
  theme_light() +
  xlab("Percentage in poverty") +
  ylab("Over/under-estimate of highspeed broadband access") +
  ggtitle("Model B")

plot_c = ggplot(data = broadband, aes(x = pct_poverty, y = fcc_diff)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 0.482, slope = 0.000863, linetype = "dashed") +  #Non-metro
  geom_abline(intercept = 0.482 - 0.134, slope = 0.000863 + 0.00934, linetype = "solid") +  #Metro
  theme_light() +
  xlab("Percentage in poverty") +
  ylab("Over/under-estimate of highspeed broadband access") +
  ggtitle("Model C")


# Layout the plots
plot_b | plot_c




