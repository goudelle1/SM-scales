# Libraries
library(tidyverse)
library(lubridate)
library(ggthemes)

# set the plots theme
theme_set(theme_bw())

# to not see scientific notations in plots
options(scipen = 999)


# Data loading and preprocessing ====

## Load the data ====
measurements <- read.csv("measurements.csv")
scale_metadata <- read.csv("scale_metadata.csv")
scale_preprocessing <- read.csv("scale_preprocessing.csv")

## Join the measurements data with the scale metadata and preprocessing ====
data_og <- measurements %>%
  left_join(scale_metadata, by = "deviceid") %>%
  left_join(scale_preprocessing, by = "deviceid")

## Reduce dataset ====

# Remove unused columns
df <- data_og[,!names(data_og) %in% c("minwaittime", "delay")]

# Filter data for sensitivity 1,4 and 7 -TO BE JUSTIFIED-
df <- filter(df, sensitivity %in% c(1, 4, 7))

# Missing values in relevant columns!!! 
## Count the number of NA values in each column
na_count <- colSums(is.na(df))
# Print the results
print(na_count)
#->6310 missing item_weight and locationid
## Remove rows with missing values in ITEM_WEIGHT
df <- df %>% filter(!is.na(ITEM_WEIGHT))

# Remove data from irrelevant sections
df <- filter(df, is_relevant_section == "True")

# Keep only the data from sections lasting at least 7 days
## Find the start and end time of each section
df <- df %>%
  group_by(section_id) %>%
  mutate(start_time = min(weighttime), end_time = max(weighttime)) 
## Add the span of each section:
df <- df %>%
  mutate(span = as.numeric(difftime(end_time, start_time, units = "days")))
## Remove observations from sections lasting less than 7 days
df <- df %>% filter(span >= 7)


## Add number of TOO_FEW  ====
#TOO_FEW_per_week: absolute number of TOO_FEW per section and per week
# THIS IS WRONG, THE CALCULATION OF THE WEEK IS NOT CORRECT
df <- df %>%
  mutate(week = week(weighttime)) %>%
  group_by(week, section_id) %>%
  mutate(TOO_FEW_per_week = sum(weightclassification == 'TOO_FEW'))
sum(is.na(df$TOO_FEW_per_week))

## fraction of TOO_MANY / Automatic per section ====
df <- df %>%
  group_by(section_id) %>%
  mutate(Fraction_TOO_MANY = sum(weightclassification == 'TOO_MANY') / sum(initiatortype == 'AUTOMATIC'))
sum(is.na(df$Fraction_TOO_MANY)) # 7485: no weightclassification == TOO_MANY and/or initiatortype == 'AUTOMATIC'
# Prepare a table with the different section_id and their respective fraction of TOO_MANY/AUTOMATIC
# create table with unique section_id values
unique_sections <- distinct(df, section_id)
# join unique_sections with df to get Fraction_TOO_MANY values for each section_id
sections_fract_TOO_MANY <- left_join(unique_sections, df %>% 
                            group_by(section_id) %>% 
                            summarise(Fraction_TOO_MANY = sum(weightclassification == 'TOO_MANY') / sum(initiatortype == 'AUTOMATIC')), 
                          by = 'section_id') %>% 
  mutate(Fraction_TOO_MANY = ifelse(is.na(Fraction_TOO_MANY), 0, Fraction_TOO_MANY)) # replace NAs by 0

# Exploratory Data Analysis ====

## Histogram weight_bins ====
ggplot(df, aes(x = factor(weight_bin,
                          levels = c("0 - 803", "803 - 1877", "1877 - 3859", "3859 - 6673", "6673 - 100000")))) +
  geom_histogram(stat = "count") +
  xlab("Weight Bins") +
  ylab("Number of Observations")

## Histogram sensitivity ====
ggplot(df, aes(x = sensitivity)) +
  geom_histogram(stat = "count") +
  xlab("Sensitivity") +
  ylab("Number of Observations") +
  scale_x_continuous(breaks = c(1, 4, 7)) 


## TOO_FEW_per_week ====
too_few_target = 2

#Count the total number of sections
total_sections <- df %>%
  distinct(section_id) %>%
  nrow()
# Count the total number of sections with 'TOO_FEW_per_week' greater than too_few_target
sections_with_few <- df %>%
  filter(TOO_FEW_per_week > too_few_target) %>%
  distinct(section_id) %>%
  nrow()
# Print the results
cat("Total number of sections: ", total_sections, "\n")
cat("Total number of sections with TOO_FEW_per_week > 2: ", sections_with_few, "\n")
# Plot number of sections and TOO_FEW_per_week:
# Create a dataframe with section IDs and their corresponding number of 'TOO_FEW' measurements per week
too_few_counts <- df %>%
  group_by(section_id) %>%
  summarise(too_few_count = sum(TOO_FEW_per_week))
# Create the histogram plot
ggplot(too_few_counts, aes(x = too_few_count)) +
  geom_histogram(binwidth = 30) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") 

## Fraction TOO_MANY ====
frac_TOO_MANY_target <- 0.5 

ggplot(sections_fract_TOO_MANY, aes(x=Fraction_TOO_MANY)) +
  geom_histogram(binwidth=0.05) +
  geom_vline(xintercept = frac_TOO_MANY_target, color = "red") +
  ggtitle("Distribution of Fraction_TOO_MANY") +
  xlab("Fraction_TOO_MANY") +
  ylab("Number of sections")


#-DOES NOT WORK YET-
#reproduce graphe of a scale as they gave us? (was the wish of the D-one guy...)



# Analysis of the effect of weight ====

## Scale types ====


# Model the influence of SCALE_NAME on TOO_FEW_per_week and Fraction_TOO_MANY for the different values of sensitivity



## Select the observations relevant for weight analysis
df_weight <- filter(df, relevant_for_weight_analysis == "True")


