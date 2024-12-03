# Load packages
library(dplyr)
library(readr)
library(ggpubr)
library(viridis)
library(broom)
library(texreg)
library(ggplot2)

# Import data!
texas = read_csv("Book(Texas).csv") %>% 
  mutate(State = "Texas")
# Check out some rows of the data!
texas %>% head(3)

# Import data!
california = read_csv("Book(California).csv") %>% 
  mutate(State = "California")
# Check out some rows of the data!
california %>% head(3)

# Combine California and Texas
measures <- bind_rows(texas, california) %>%
  # You need this step.
  mutate(State = factor(State))
# Check out some rows of the data!
measures %>% head(3)

# linear model
model = lm(Readmissions ~ State, data = measures)
#coefficients, using the workshop and a 95% Cf
coefficients_df = broom::tidy(model, conf.int = TRUE, conf.level = 0.95)
# try to see the top 3 effects
top_effects = coefficients_df %>%
  arrange(desc(abs(estimate))) %>%
  #show the top 3 including the intercept
  head(4)
#lets see it
top_effects

# model #1 State effects
model1 =
  lm(Readmissions ~ State, data = measures)
# model #2
model2 =
  lm(Readmissions ~ State + Year, data = measures)
# model #3
model3 =
  lm(Readmissions ~ State + 
       Year + 
       Discharges, data = measures)
# model #4
model4 =
  lm(Readmissions ~ State + 
       Year + 
       Discharges +
       Population +
       Hospitalizations, data = measures)

# create a list of the models
models = list(model1, model2, model3, model4)
states = list("StateCalifornia" = "California",
              "StateTexas" = "Texas")
#our texreg (screenreg) table
# The table below shows California as the baseline compared to Texas
screenreg(models, 
          custom.coef.map = states, 
          include.fstat = TRUE)
