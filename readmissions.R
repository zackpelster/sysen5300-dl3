# install.packages("dplyr", "tidyr", "ggpubr", "moments")
library(tidyverse)
library(viridis)
library(ggpubr)
library(moments)
library(dplyr)
library(tidyr)

raw_readmissions = read.csv(
  "FY_2024_Hospital_Readmissions_Reduction_Program_Hospital.csv"
  )

raw_readmissions %>% glimpse()

convert_to_double = function(column){
  as.double(ifelse(grepl("^[0-9]+(\\.[0-9]+)?$", column), column, NA))
}

convert_to_integer = function(column){
  as.integer(ifelse(grepl("^[0-9]+$", column), column, NA))
}

date_format = "%m/%d/%Y"

bronze_readmissions = raw_readmissions %>% mutate(
  Number.of.Discharges = convert_to_integer(Number.of.Discharges),
  Excess.Readmission.Ratio = convert_to_double(Excess.Readmission.Ratio),
  Predicted.Readmission.Rate = convert_to_double(Predicted.Readmission.Rate),
  Expected.Readmission.Rate = convert_to_double(Expected.Readmission.Rate),
  Number.of.Readmissions = convert_to_integer(Number.of.Readmissions),
  Start.Date = as.Date(Start.Date, format = date_format),
  End.Date = as.Date(End.Date, format = date_format),
  )

bronze_readmissions %>% glimpse()

silver_readmissions = bronze_readmissions %>% drop_na(
  Number.of.Discharges,
  Excess.Readmission.Ratio,
  Predicted.Readmission.Rate,
  Expected.Readmission.Rate,
  Number.of.Readmissions
  )

silver_readmissions %>% glimpse()

state_filter = silver_readmissions %>% 
  group_by(State) %>% 
  summarise(
    facilities = n_distinct(Facility.Name)
    ) %>% 
  filter(facilities >= 50)

stat = silver_readmissions %>% 
  filter(State %in% state_filter$State) %>% 
  group_by(State, Facility.Name) %>% 
  summarise(
    Number.of.Discharges = sum(Number.of.Discharges, na.rm = TRUE),
    Number.of.Readmissions = sum(Number.of.Readmissions, na.rm = TRUE),
    rate = Number.of.Readmissions/Number.of.Discharges
)

stat %>% glimpse()

hist(stat$Number.of.Readmissions)
hist(stat$Number.of.Discharges)
hist(stat$rate)

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

describe = function(x){
  # Put our vector x in a tibble
  tibble(x) %>%
    # Calculate summary statistics
    summarize(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      # We'll use the moments package for these two
      skew = skewness(x, na.rm = TRUE),
      kurtosis = kurtosis(x, na.rm = TRUE)) %>%
    # Let's add a caption, that compiles  all these statistics  
    mutate(
      # We'll paste() the following together
      caption = paste(
        # Listing the name of each stat, then reporting its value and rounding it, then separating with " | "
        "Process Mean: ", mean %>% round(2), " | ", 
        "SD: ", sd %>% round(2), " | ",
        "Skewness: ", skew %>% round(2), " | ",
        "Kurtosis: ", kurtosis %>% round(2), 
        # Then make sure no extra spaces separate each item
        sep = "")) %>%
    return()
}
# Run descriptives!
tab = stat$rate %>% describe()
# Check it out!
tab

# Make the initial boxplot...
g1 = stat %>%
  ggplot(mapping = aes(x = State, y = rate, group = State)) +
  # Plot grand mean
  geom_hline(mapping = aes(yintercept = mean(rate)), color = "lightgrey", size = 3) +
  # Plot points and boxplots 
  geom_jitter(height = 0, width = 0.25) +
  geom_boxplot() +
  labs(x = "State (Subgroup)", y = "Rate (Readmissions per Discharge)",
       subtitle = "Process Overview",
       # Add our descriptive stats in the caption!
       caption = tab$caption)
# Part 1 of plot
g1

# Make the histogram, but tilt it on its side
g2 = stat %>%
  ggplot(mapping = aes(x = rate)) +
  geom_histogram(bins = 15, color = "white", fill = "grey") +
  theme_void() +   # Clear the them
  coord_flip()   # tilt on its side
# Part 2 of plot
g2

# Then bind them together into 1 plot, 'h'orizontally aligned.
p1 = ggarrange(g1,g2, widths = c(5,1), align = "h")
# Check it out!
p1

# Calculate short-term statistics within each group
stat_s = stat %>% 
  # For each timestpe
  group_by(State) %>%
  # Calculate these statistics of interest!
  summarize(
    # within-group mean
    xbar = mean(rate),
    # within-group range
    r = max(rate) - min(rate),
    # within-group standard deviation
    sd = sd(rate),
    # within-group sample size
    nw = n(),
    # Degrees of freedom within groups
    df = nw - 1) %>%
  # Last, we'll calculate sigma_short (within-group variance)
  # We're going to calculate the short-term variation parameter sigma_s (sigma_short)
  # by taking the square root of the average of the standard deviation
  # Essentially, we're weakening the impact of any special cause variation
  # so that our sigma is mostly representative of common cause (within-group) variation
  mutate(
    # these are equivalent
    sigma_s = sqrt( sum(df * sd^2) / sum(df) ),
    sigma_s = sqrt(mean(sd^2)), 
    # And get standard error (in a way that retains each subgroup's sample size!)
    se = sigma_s / sqrt(nw),
    # Calculate 6-sigma control limits!
    upper = mean(xbar) + 3*se,
    lower = mean(xbar) - 3*se,
    upper_A_B = mean(xbar) + 2*se,
    lower_A_B = mean(xbar) - 2*se,
    upper_B_C = mean(xbar) + 1*se,
    lower_B_C = mean(xbar) - 1*se)

# Check it!
stat_s %>% head(3)

# To get between-group estimates....
stat_t = stat_s %>%
  summarize(
    xbbar = mean(xbar),
    rbar = mean(r),
    sdbar = mean(sd),
    # We can also recalculate sigma_short here too
    sigma_s = sqrt( mean(sd^2) ),
    # Or we can calculate overall standard deviation 
    sigma_t = sd(stat$rate) )

# Let's extract some labels
labels = stat_s %>%
  summarize(
    State = max(State),
    type = c("xbbar",  "upper", "lower"),
    name = c("mean", "+3 s", "-3 s"),
    # SINCE nw Varies Upper and Lower vary resulting in many unique values ####
    value = c(mean(xbar), first(upper), first(lower)),
    value = round(value, 2),
    text = paste(name, value, sep = " = "))
labels

stat_s %>%
  ggplot(mapping = aes(x = State, y = xbar)) +
  geom_hline(mapping = aes(yintercept = mean(xbar)), color = "lightgrey", size = 3) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  # Upper and lower control limits as lines
  geom_line(mapping = aes(x = State, y = upper, group = 1), color = "red", linetype = "dashed", size = 1) +
  geom_line(mapping = aes(x = State, y = lower, group = 1), color = "red", linetype = "dashed", size = 1) +
  geom_line(mapping = aes(x = State, y = upper_A_B, group = 1), color = "red", linetype = "dashed", size = 2/3) +
  geom_line(mapping = aes(x = State, y = lower_A_B, group = 1), color = "red", linetype = "dashed", size = 2/3) +
  geom_line(mapping = aes(x = State, y = upper_B_C, group = 1), color = "red", linetype = "dashed", size = 1/3) +
  geom_line(mapping = aes(x = State, y = lower_B_C, group = 1), color = "red", linetype = "dashed", size = 1/3) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  # Plot labels
  # geom_label(data = labels, mapping = aes(x = State, y = value, label = text),  hjust = 1)  +
  labs(x = "State (Subgroups)", y = "Average (Readmissions per Discharge)",
       subtitle = "Average Chart")
