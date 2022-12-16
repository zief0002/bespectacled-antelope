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
mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/mn-schools.csv")


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


# Load libraries for formatting
library(knitr)
library(kableExtra)


# Create knitted table
tab_01 %>%
  kable(
    format = "html",
    booktabs = TRUE,
    escape = FALSE,
    col.names = c("Model", "k", "AICc", "$\\Delta$AICc", "Rel($\\mathcal{L}$)", "AICc Weight"),
    caption = "Models rank-ordered by the amount of empirical support as measured by the AICc. The sample size used to fit each model was $n=33$.",
    digits = 1,
    table.attr = "style='width:50%;'"
  ) %>%
  footnote(
    general = "Rel($\\mathcal{L}$) = Relative Likelihood",
    general_title = "Note.",
    footnote_as_chunk = TRUE
  ) %>%
  kable_classic()






