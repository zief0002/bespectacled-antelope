##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(educate)
library(patchwork)
library(tidyverse)




##################################################
### Read in data
##################################################

grad = read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/graduation.csv")
grad



##################################################
### Explore outcome
##################################################

# Compute total number of cases
nrow(grad)


# Compute counts/proportions by outcome level
graduated = grad %>% 
  group_by(degree) %>% 
  summarize(
    Count = n(), 
    Prop = n() / 2344
  )


# View results
graduated



##################################################
### Bar chart (outcome)
##################################################

# New column with text to add to bars
graduated = graduated %>%
  mutate(
    count_prop = paste0(Count, "\n(", round(Prop, 3), ")")
  )

# Barplot
ggplot(data = graduated, aes(x = degree, y = Count)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = count_prop), vjust = 1.1, color = "white") +
  theme_light() +
  xlab("Graduated") +
  ylab("Prop")



##################################################
### Dummy-code the outcome
##################################################

# Create dummy-coded degree variable
grad = grad %>%
  mutate(
    got_degree = if_else(degree == "Yes", 1, 0)
  )

# View data
grad



##################################################
### Explore predictor (ACT)
##################################################

# Density plot
ggplot(data = grad, aes(x = act)) +
  geom_density() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Probability density")


# Summary measures
grad %>%
  summarize(
    M = mean(act),
    SD = sd(act)
  )



##################################################
### Relationship between act and proportion of students who obtain degree
##################################################

# Compute 
prop_grad = grad %>% 
  group_by(act, degree) %>% 
  summarize(
    N = n()  # Compute sample sizes by degree for each ACT score
  ) %>% 
  mutate(
    Prop = N / sum (N) #Compute proportion by degree for each ACT score
  ) %>%
  filter(degree == "Yes") %>% # Only use the "Yes" responses
  ungroup() #Makes the resulting tibble regular

# View data
prop_grad %>%
  print(n = Inf) #Print all the rows


# Scatterplot: GOOD
ggplot(data = prop_grad, aes(x = act, y = Prop)) +
  geom_point(aes(size = N)) +
  geom_smooth(data = grad, aes(y = got_degree), method = "loess", se = FALSE, 
              color = "red", linetype = "dashed") +
  geom_smooth(data = grad, aes(y = got_degree), method = "lm", se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Proportion of students who obtained a degree")


# Scatterplot: NOT GOOD
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Graduated")


# Correlation
grad %>%
  select(degree, act) %>%
  correlate()



##################################################
### Fit linear probability model
##################################################

# Fit the model
lm.1 = lm(got_degree ~ 1 + act, data = grad)


# Model-level- output
glance(lm.1)


# Coefficient-level- output
tidy(lm.1)



##################################################
### Evaluate assumptions
##################################################

out = augment(lm.1)
head(out)


# Examine normality assumption
ggplot(data = out, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  geom_density() +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")


# Examine linearity and homoskedasticity
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



