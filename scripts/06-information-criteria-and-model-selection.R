##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(tidyverse)



##################################################
### Read in and prepare data
##################################################

# Import data
mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/bespectacled-antelope/main/data/mn-schools.csv")


# View data
head(mn)



##################################################
### Fit candidate models
##################################################

lm.1 = lm(grad ~ 1 + sat, data = mn)
lm.2 = lm(grad ~ 1 + sat + public, data = mn)
lm.3 = lm(grad ~ 1 + sat + public + sat:public, data = mn)



##################################################
### Akiake's Information Criteria (AIC)
##################################################

# Compute AIC for model associated with linear hypothesis
# logLik(lm.1)
-2*-113.5472 + 2*3



##################################################
### Use AIC() and glance() functions
##################################################

AIC(lm.1) #Model 1
AIC(lm.2) #Model 2
AIC(lm.3) #Model 3


# AIC available in glance() output
glance(lm.2)



##################################################
### AIC Second-Order Corrected (Corrected AIC)
##################################################

n = 33
k = 3

# Compute AICc for Model 1
-2 * logLik(lm.1)[[1]] + 2 * k * n / (n - k - 1)


# Shortcut with function
AICc(lm.1) #Model 1
AICc(lm.2) #Model 2
AICc(lm.3) #Model 3



##################################################
### Delta-AICc values
##################################################

AICc(lm.1) - AICc(lm.2)  #Model 1
AICc(lm.2) - AICc(lm.2)  #Model 2
AICc(lm.3) - AICc(lm.2)  #Model 3



##################################################
### Relative likelihood
##################################################

exp(-1/2 * 6.900556) #Model 1
exp(-1/2 * 0)        #Model 2
exp(-1/2 * 2.20514)  #Model 3



##################################################
### Evidence ratios
##################################################

1 / 0.3320167  #Model 2 vs Model 3
1 / 0.03173681 #Model 2 vs Model 1



##################################################
### Model probabilities (Akaike Weight)
##################################################

# Compute sum of relative likelihoods
sum_rel = 0.03173681 + 1 + 0.3320167


# Compute model probability for each model
0.03173681  / sum_rel #Model 1
1 / sum_rel           #Model 2
0.3320167 / sum_rel   #Model 3



##################################################
### Table of model evidence
##################################################

#Create table of model evidence
model_evidence = aictab(
  cand.set = list(lm.1, lm.2, lm.3), 
  modnames = c("Model 1", "Model 2", "Model 3")
)


# View output
model_evidence



##################################################
### Pretty printing tables of model evidence
##################################################

# Create data frame to format into table
tab_01 = model_evidence %>%
  data.frame() %>%
  select(-LL, -Cum.Wt)


# View table
tab_01


# Create table with gt()
tab_01 |>
  gt() |>
  cols_align(
    columns = c(Modnames),
    align = "left"
  ) |>
  cols_align(
    columns = c(K, AICc, Delta_AICc, ModelLik, AICcWt),
    align = "center"
  ) |>
  cols_label(
    Modnames = md("*Model*"),
    K = md("*k*"),
    AICc = md("*AICc*"),
    Delta_AICc = html("&#916;AICc"),
    ModelLik = html("Rel(&#8466;)"),
    AICcWt = md("*AICc Wt.*")
  ) |>
  tab_options(
    table.width = pct(50)
  ) |>
  tab_footnote(
    footnote = html("Rel(&#8466;) = Relative likelihood"),
    locations = cells_column_labels(columns = ModelLik)
  )






