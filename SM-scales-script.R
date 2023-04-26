# Libraries
library(tidyverse)
library(lubridate)

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
#Remove unused columns
df <- data_og[,!names(data_og) %in% c("minwaittime", "delay")]
#Filter data for sensitivity 1,4 and 7 -TO BE JUSTIFIED-
df <- filter(df, sensitivity %in% c(1, 4, 7))
#Remove data from irrelevant sections
df <- filter(df, is_relevant_section == "True")

#Missing values in relevant columns!!! 
# Count the number of NA values in each column
na_count <- colSums(is.na(df))
# Print the results
print(na_count)
#->6310 missing item_weight and locationid
# Remove rows with missing values in ITEM_WEIGHT
df <- df %>% filter(!is.na(ITEM_WEIGHT))

## Add number of TOO_FEW  ====

#TOO_FEW_per_week: absolute number of TOO_FEW per section and per week
df <- df %>%
  mutate(week = week(weighttime)) %>%
  group_by(week, section_id) %>%
  mutate(TOO_FEW_per_week = sum(weightclassification == 'TOO_FEW'))
sum(is.na(df$TOO_FEW_per_week))

## fraction of TOO_MANY / Automatic per section ====
df <- df %>%
  group_by(section_id) %>%
  mutate(Fraction_TOO_MANY = sum(weightclassification == 'TOO_MANY') / sum(initiatortype == 'AUTOMATIC'))
sum(is.na(df$Fraction_TOO_MANY)) # 7485

# Calculate Fraction_TOO_MANY by section using aggregate
result <- aggregate(Fraction_TOO_MANY ~ section_id, data = df, FUN = mean, na.rm = TRUE)
# Print the resulting table
print(result)


# Exploratory Data Analysis ====


## TOO_FEW_per_week -DOES NOT WORK YET-

too_few_target = 2

#Count the total number of sections
total_sections <- df %>%
  distinct(section_id) %>%
  nrow()
#Count the total number of sections with 'TOO_FEW_per_week' greater than too_few_target
sections_with_few <- df %>%
  filter(TOO_FEW_per_week > too_few_target) %>%
  distinct(section_id) %>%
  nrow()
# Print the results
cat("Total number of sections: ", total_sections, "\n")
cat("Total number of sections with TOO_FEW_per_week > 2: ", sections_with_few, "\n")



#reproduce graphe of a scale as they gave us? Histogram of different weight bins, different sensitivity

# Analysis of the effect of weight ====

## Select the observartions relevant for weight analysis
df_weight <- filter(df, relevant_for_weight_analyis == "True")


