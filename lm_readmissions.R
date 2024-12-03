# Load packages
library(dplyr)
library(readr)
library(ggpubr)
library(viridis)
library(broom)
library(texreg)
library(ggplot2)

# Import data!
measures = read_csv("measures.csv") %>%
  # You need this step
  mutate(state = factor(state))
# Check out some rows of the data!
measures %>% head(3)

# Filter for California and Texas
filtered_measures <- measures %>%
  filter(state %in% c("CALIFORNIA", "TEXAS"))
# Check out some rows of the data!
filtered_measures %>% head(3)

# linear model
model = lm(readmissions ~ state, data = filtered_measures)
#coefficients, using the workshop and a 95% Cf
coefficients_df = broom::tidy(model, conf.int = TRUE, conf.level = 0.95)
# try to see the top 3 effects
top_effects = coefficients_df %>%
  arrange(desc(abs(estimate))) %>%
  #show the top 3 including the intercept
  head(4)
#lets see it
top_effects

# model #1 state effects
model1 =
  lm(readmissions ~ state, data = filtered_measures)
# model #2
model2 =
  lm(readmissions ~ state + medicare_reimbursement, data = filtered_measures)
# model #3
model3 =
  lm(readmissions ~ state + 
       medicare_reimbursement + 
       inpatient_days, data = filtered_measures)
# model #4
model4 =
  lm(readmissions ~ state + 
       medicare_reimbursement + 
       inpatient_days +
       admission_type +
       discharge_destination, data = filtered_measures)

# create a list of the models
models = list(model1, model2, model3, model4)
#our texreg (screenreg) table
screenreg(models)
