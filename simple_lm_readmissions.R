# Load packages
library(dplyr)
library(readr)
library(ggpubr)
library(viridis)
library(broom)
library(texreg)
library(ggplot2)

# Import Texas data!
texas = read_csv("Book(Texas).csv") %>% 
  mutate(State = "Texas")

# Import California data!
california = read_csv("Book(California).csv") %>% 
  mutate(State = "California")

# Combine California and Texas
measures <- bind_rows(texas, california) %>%
  # You need this step.
  mutate(
    State = factor(State),
    Readmission_Rate = Readmissions/Discharges
    )

# They might optionally run theme_set() to make a really nice chart theme.
# By running theme_set()
theme_set(
  # we tell ggplot to give EVERY plot this theme
  theme_classic(base_size = 14) +
    # With these theme traits, including
    theme(
      # Putting the legend on the bottom, if applicable
      legend.position = "bottom",
      # horizontally justify plot subtitle and caption in center
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      # Getting rid of busy axis ticks
      axis.ticks = element_blank(),
      # Getting rid of busy axis lines
      axis.line = element_blank(),
      # Surrounding the plot in a nice grey border
      panel.border = element_rect(fill = NA, color = "grey"),
      # Remove the right margin, for easy joining of plots
      plot.margin = margin(r = 0)
    )
)

g1 = ggplot() +
  # They need not reorder their neighborhoods by Readmissions, but it makes it look prettier
  geom_jitter(
    data = measures,
    mapping = aes(x = State, y = Readmissions),
    height = 0, shape = 21, color = "white", fill = "darkgrey", alpha = 0.75) +
  geom_boxplot(
    data = measures,
    mapping = aes(x = State, y = Readmissions),
    fill = "steelblue", alpha = 0.5) +
  labs(y = "Readmissions", x = "State") +
  theme(axis.text.x = element_text(hjust = 1, angle = 90))

g1

stat = measures %>%
  group_by(State) %>%
  summarize(
    # 1. Estimate neighborhood means
    xbar = mean(Readmissions),
    # 2. Estimate within-group sample size
    n_w = n(),
    # 3. Estimate within-group standard deviation sigma_short
    s = sd(Readmissions)) %>%
  mutate(
    # 4. Estimate grand mean of Readmissions across all observed states
    xbbar = mean(measures$Readmissions),
    # 5. Estimate each subgroup's standard error, using sigma short
    sigma_s = sqrt(mean(s^2)),
    se = sigma_s / sqrt(n_w)
  )

# Identify the state with the highest mean readmission rate across observed data
stat %>% 
  filter(xbar == max(xbar)) %>% 
  # Calculate how many standard errors greater than the grand mean is it?
  mutate(z = (xbar - xbbar) / se ) %>%
  # z is your multiplier
  glimpse()

# California has the highest readmissions
# Rows: 1
# Columns: 8
# $ State   <fct> California
# $ xbar    <dbl> 593368.9
# $ n_w     <int> 11
# $ s       <dbl> 22827.19
# $ xbbar   <dbl> 535654.3
# $ sigma_s <dbl> 23891.04
# $ se      <dbl> 7203.419
# $ z       <dbl> 8.012111

# Let's write a little tidier function..
tidier = function(model, ci = 0.95, digits = 3){
  model %>% # for a model object
    # get data.frame of coefficients
    # ask for a confidence interval matching the 'ci' above! 
    broom::tidy(conf.int = TRUE, conf.level = ci) %>% 
    # And round and relabel them
    reframe(
      term = term,  
      # Round numbers to a certain number of 'digits'
      estimate = estimate %>% round(digits),
      se = statistic %>% round(digits),
      statistic = statistic %>% round(digits),
      p_value = p.value %>% round(digits),
      # Get stars to show statistical significance
      stars = p.value %>% gtools::stars.pval(),
      # Get better names
      upper = conf.high %>% round(digits),
      lower = conf.low %>% round(digits))
}

m1 = measures %>%
  lm(formula = Readmissions ~ State)

m1 %>% 
  tidier() %>%
  arrange(desc(estimate)) %>%
  slice(1:4)

# A tibble: 2 × 8
#   term        estimate    se statistic p_value stars   upper    lower
#   <chr>          <dbl> <dbl>     <dbl>   <dbl> <chr>   <dbl>    <dbl>
# 1 (Intercept)  593369.  82.4      82.4       0 ***   608395.  578343.
# 2 StateTexas  -115429. -11.3     -11.3       0 ***   -94179. -136679.

# In this model, California serves as the baseline (intercept) for comparison, 
# as it is the first alphabetical state in the factor variable.

# Compared to California, our model projects that Texas sees readmissions **115429** **fewer** 
# than in California, with a 95% confidence interval ranging from -136679 to -94179., an 
# extremely statistically significant association (p < 0.001).

m1 = measures %>% lm(formula = Readmissions ~ State)
m2 = measures %>% lm(formula = Readmissions ~ State + Hospitalizations)
m3 = measures %>% lm(formula = Readmissions ~ State + Hospitalizations + ED_Visits)
m4 = measures %>% lm(formula = Readmissions ~ State + Hospitalizations + ED_Visits +
                       PQI_90_Prevention_Quality_Overall_Composite_Benficiaries)

texreg::screenreg(
  list(m1,m2,m3,m4),
  custom.coef.map = list("StateTexas" = "Texas"), 
  include.fstat = TRUE)

m4 = measures %>% lm(formula = Readmissions ~ State + Hospitalizations + ED_Visits +
                       PQI_90_Prevention_Quality_Overall_Composite_Benficiaries)

m4 %>% 
  tidier() %>%
  arrange(desc(estimate)) %>%
  filter(term %in% c("(Intercept)", "StateTexas"))

# Texas saw readmissions 68241 fewer than California, within a 95% confidence interval 
# of -102505 to -33978 (p < 0.001).


m5 = measures %>% lm(formula = Readmissions ~ State + scale(Hospitalizations) + scale(ED_Visits) +
                       scale(PQI_90_Prevention_Quality_Overall_Composite_Benficiaries))

m5 %>%
  tidier() %>%
  filter(str_starts(term, "scale"))

# The largest effect is from the ED visits.
# We see that for every 1 standard deviation increase in the ED visits in a state,
# readmissions are projected to rise by 26149, with a 95% confidence interval span 
# from 6108 to 46190.
# This effect is marginally significant (p < 0.1), suggesting a potential but less robust relationship.

m1 = measures %>% lm(formula = Readmissions ~ State)
m2 = measures %>% lm(formula = Readmissions ~ State + Hospitalizations)
m3 = measures %>% lm(formula = Readmissions ~ State + Hospitalizations + ED_Visits)
m4 = measures %>% lm(formula = Readmissions ~ State + Hospitalizations + ED_Visits +
                       PQI_90_Prevention_Quality_Overall_Composite_Benficiaries)

bind_rows(
  glance(m1),
  glance(m2),
  glance(m3),
  glance(m4), 
  .id = "model"
)  %>%
  select(model, r.squared) %>%
  mutate(diff = c(0, diff(r.squared)))

# A tibble: 4 × 3
#   model r.squared    diff
#   <chr>     <dbl>   <dbl>
# 1 1         0.865 0      
# 2 2         0.889 0.0243 
# 3 3         0.923 0.0338 
# 4 4         0.928 0.00496


r = bind_rows(
  measures %>% select(Readmissions, variable = State) %>% 
    mutate(type = "State") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = Year) %>% 
    mutate(type = "Year") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = Discharges) %>% 
    mutate(type = "Discharges") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = Population) %>% 
    mutate(type = "Population") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = Total_Beneficaries) %>% 
    mutate(type = "Total_Beneficaries") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = Discharge_Population_Ratio) %>%
    mutate(type = "Discharge_Population_Ratio") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = Hospitalizations) %>% 
    mutate(type = "Hospitalizations")%>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = Inpatient_Days) %>% 
    mutate(type = "Inpatient_Days") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = ED_Visits) %>% 
    mutate(type = "ED_Visits") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = Annual_Wellness_Visit) %>% 
    mutate(type = "Annual_Wellness_Visit") %>%
    lm(formula = Readmissions ~ variable) %>%  glance(),
  measures %>% select(Readmissions, variable = PQI_90_Prevention_Quality_Overall_Composite_Benficiaries) %>% 
    mutate(type = "PQI_90_Prevention_Quality_Overall_Composite_Benficiaries") %>%
    lm(formula = Readmissions ~ variable) %>%  glance()
) %>%
  mutate(variable = c("State", "Year", "Discharges", "Population", "Total_Beneficaries",
                      "Discharge_Population_Ratio", "Hospitalizations", "Inpatient_Days", 
                      "ED_Visits", "Annual_Wellness_Visit", 
                      "PQI_90_Prevention_Quality_Overall_Composite_Benficiaries")) %>%
  select(variable, r.squared) %>%
  # rounding not necessary.
  mutate(r.squared = round(r.squared, 4))
r
# A tibble: 11 × 2
#    variable                                                 r.squared
#    <chr>                                                        <dbl>
#  1 State                                                       0.865 
#  2 Year                                                        0.0041
#  3 Discharges                                                  0.949 
#  4 Population                                                  0.898 
#  5 Total_Beneficaries                                          0.764 
#  6 Discharge_Population_Ratio                                  0.528 
#  7 Hospitalizations                                            0.601 
#  8 Inpatient_Days                                              0     
#  9 ED_Visits                                                   0.850 
# 10 Annual_Wellness_Visit                                       0.676 
# 11 PQI_90_Prevention_Quality_Overall_Composite_Benficiaries    0.185 

measures %>%
  group_by(State) %>%
  reframe(lm(formula = Readmissions ~ Hospitalizations) %>% broom::tidy()) %>%
  filter(term == "Hospitalizations")

# A tibble: 2 × 6
#   State      term             estimate std.error statistic p.value
#   <fct>      <chr>               <dbl>     <dbl>     <dbl>   <dbl>
# 1 California Hospitalizations   0.0658    0.0460      1.43  0.187 
# 2 Texas      Hospitalizations   0.277     0.123       2.26  0.0504

# California (beta = 0.0658, p = 0.187), 
# Texas (beta = 0.277, p = 0.0504), 
# and Mattapan (beta = -0.004, p = 0.113)
# both show positive relationships between Hospitalizations and Readmissions.

measures %>%
  group_by(State) %>%
  reframe(lm(formula = Readmissions ~ Hospitalizations) %>% broom::glance())
# A tibble: 2 × 13
#   State    r.squared adj.r.squared  sigma statistic p.value    df logLik   AIC   BIC deviance df.residual  nobs
#   <fct>        <dbl>         <dbl>  <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
# 1 Califor…     0.185        0.0945 21722.      2.04  0.187      1  -124.  255.  256.   4.25e9           9    11
# 2 Texas        0.361        0.290  20982.      5.09  0.0504     1  -124.  254.  255.   3.96e9           9    11

# Our models for Texas (F = 5.09, p = 0.0504) produce statistically significant fits, 
# fitting better than an intercept model, assuming a 90% confidence interval.

# Our models for California (F = 2.04, p = 0.187) would not be considered having 
# statistically significant fits within a 90% confidence interval.

measuressubset = measures %>% filter(State == "Texas")

m6 = measuressubset %>% lm(formula = Readmissions ~ Hospitalizations)

mysim = measuressubset %>%
  reframe(
    mu = mean(Hospitalizations, na.rm = TRUE),
    sd = sd(Hospitalizations, na.rm = TRUE),
    Hospitalizations = seq(from = mu, to = +3*sd + mu, by = sd),
    yhat = predict(m6, newdata = tibble(Hospitalizations)),
    sigma = m6 %>% glance() %>% with(sigma)) %>%
  group_by(Hospitalizations) %>%
  reframe(ysim = rnorm(1000, mean = yhat, sd = sigma)) %>%
  select(Hospitalizations, ysim) 

# Get simulated confidence intervals
mystat = mysim %>%
  group_by(Hospitalizations) %>%
  summarize(lower = quantile(ysim, probs = 0.025) %>% round(2),
            upper = quantile(ysim, probs = 0.975) %>% round(2))

# If they add the geom_jitter layer, give them 2 bonus points.

ggplot() +
  geom_ribbon(data = mystat,
              mapping = aes(x = Hospitalizations, ymin = lower, ymax = upper), alpha = 0.25, fill = "steelblue") +
  geom_jitter(data = mysim, 
              mapping = aes(x = Hospitalizations, y = ysim), color = "steelblue", alpha = 0.5) +
  theme_classic(base_size = 14) +
  labs(x = "Hospitalizations", y = "Predicted Readmissions")
