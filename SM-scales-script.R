# Libraries
library(tidyverse)
library(lubridate)
library(ggthemes)
library(forcats)
library(viridis)

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

# Reorder the factor weight_bin
df$weight_bin <- factor(df$weight_bin, levels = c("0 - 803", "803 - 1877", "1877 - 3859", "3859 - 6673", "6673 - 100000"))

## Add number of TOO_FEW  ====
#TOO_FEW_per_week: absolute number of TOO_FEW per section and per week
df <- df %>%
  mutate(week = week(weighttime)) %>%
  group_by(week, section_id) %>%
  mutate(TOO_FEW_per_week = sum(weightclassification == 'TOO_FEW'))
sum(is.na(df$TOO_FEW_per_week)) #0!

## fraction of TOO_MANY / Automatic per section ====
df <- df %>%
  group_by(section_id) %>%
  mutate(Fraction_TOO_MANY = sum(weightclassification == 'TOO_MANY') / sum(initiatortype == 'AUTOMATIC'))
sum(is.na(df$Fraction_TOO_MANY)) # 7485: no weightclassification == TOO_MANY and/or initiatortype == 'AUTOMATIC'
df <- df %>%
  mutate(Fraction_TOO_MANY = ifelse(is.na(Fraction_TOO_MANY), 0, Fraction_TOO_MANY)) # replace NAs by 0

# Prepare a table with the different section_id and their respective fraction of TOO_MANY/AUTOMATIC
# create table with unique section_id values
unique_sections <- distinct(df, section_id)
# join unique_sections with df to get Fraction_TOO_MANY values for each section_id
sections_fract_TOO_MANY <- left_join(unique_sections, df %>% 
                            group_by(section_id) %>% 
                            summarise(Fraction_TOO_MANY = sum(weightclassification == 'TOO_MANY') / sum(initiatortype == 'AUTOMATIC')), 
                          by = 'section_id')

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

## Scales and weights ====
df %>%
  group_by(SCALE_NAME) %>%
  summarise(mean_weight = mean(weight),
            max_weight = max(weight),
            min_weight = min(weight),
            median_weight = median(weight))

# Reorder SCALE_NAME factor levels to display scale 6kg first
df$SCALE_NAME <- fct_relevel(df$SCALE_NAME, "Cloud-V1.00-6kg")
# Plot weight distribution for each scale
ggplot(df, aes(x = weight, fill = factor(SCALE_NAME))) +
  geom_histogram(bins = 30, alpha = 0.5) +
  ggtitle("Weight Distribution by Scale") +
  xlab("Weight (g)") +
  ylab("Density")


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

###  Plot number of sections and TOO_FEW_per_week:  ====
# Create a dataframe with section IDs and their corresponding number of 'TOO_FEW' measurements per week
too_few_counts <- df %>%
  group_by(section_id) %>%
  summarise(too_few_count = sum(TOO_FEW_per_week),sensitivity = first(sensitivity),
            weight = first(weight),
            weight_bin = first(weight_bin),
            LOCATIONID = first(LOCATIONID),
            SCALE_NAME = first(SCALE_NAME))

too_few_counts_weight <- filter(df, relevant_for_weight_analysis == "True") %>%
  group_by(section_id) %>%
  summarise(too_few_count = sum(TOO_FEW_per_week),sensitivity = first(sensitivity),
            weight = first(weight),
            weight_bin = first(weight_bin),
            LOCATIONID = first(LOCATIONID),
            SCALE_NAME = first(SCALE_NAME))

#### Create the general histogram plot  ====

too_few_plot <- ggplot(too_few_counts, aes(x = too_few_count)) +
  geom_histogram(binwidth = 30) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections")

too_few_plot

#### Color it by sensitivity  ====
ggplot(too_few_counts, aes(x = too_few_count, fill = factor(sensitivity))) +
  geom_histogram(binwidth = 25) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity)

# -> High numbers of TOO_FEW come from sensitivity 7. Let's focus on sensitivity 1 and 4:
too_few_counts_low_S <- too_few_counts %>% filter(sensitivity <7)

ggplot(too_few_counts_low_S, aes(x = too_few_count, fill = factor(sensitivity))) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity)

#### Color it by weight ====

# Select the observations relevant for weight analysis

too_few_counts_low_S_weight <- too_few_counts_weight %>% filter(sensitivity <7)

# Separate the data in scales 6kg and scales 20kg
too_few_counts_6 <- too_few_counts_weight %>% filter(SCALE_NAME == "Cloud-V1.00-6kg")
too_few_counts_20 <- too_few_counts_weight %>% filter(SCALE_NAME == "Cloud-V1.00-20kg")
too_few_counts_low_S_6 <- too_few_counts_low_S_weight %>% filter(SCALE_NAME == "Cloud-V1.00-6kg")
too_few_counts_low_S_20 <- too_few_counts_low_S_weight %>% filter(SCALE_NAME == "Cloud-V1.00-20kg")

##### Scale 6kg  ====
ggplot(too_few_counts_6, aes(x = too_few_count, fill = weight, group = weight)) +
  geom_histogram(binwidth = 50) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 6kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity) +
  scale_fill_gradientn(colors = viridis(10))

ggplot(too_few_counts_low_S_6, aes(x = too_few_count, fill = weight, group = weight)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 6kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity) +
  scale_fill_gradientn(colors = viridis(10))

# filter for values of weights above 6000
ggplot(filter(too_few_counts_low_S_6, weight > 6000), aes(x = too_few_count, fill = weight, group = weight)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 6kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity) +
  scale_fill_gradientn(colors = viridis(10))

ggplot(filter(too_few_counts_low_S_6, weight < 6000), aes(x = too_few_count, fill = weight, group = weight)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 6kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity) +
  scale_fill_gradientn(colors = viridis(10))


##### Scale 20kg  ====
ggplot(too_few_counts_20, aes(x = too_few_count, fill = weight, group = weight)) +
  geom_histogram(binwidth = 25) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 20kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity, scale = "free_x") +
  scale_fill_gradientn(colors = viridis(10))

ggplot(too_few_counts_low_S_20, aes(x = too_few_count, fill = weight, group = weight)) +
  geom_histogram(bins = 25) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 20kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity, scale = "free_x") +
  scale_fill_gradientn(colors = viridis(10))

#### Color it by weight_bin ====

##### Scale 6kg  ====
ggplot(too_few_counts_6, aes(x = too_few_count, fill = weight_bin)) +
  geom_histogram(binwidth = 25) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 6kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity, scale = "free_x")

ggplot(too_few_counts_low_S_6, aes(x = too_few_count, fill = weight_bin)) +
  geom_histogram(bins = 25) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 6kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity, scale = "free_x")

##### Scale 20kg  ====
ggplot(too_few_counts_20, aes(x = too_few_count, fill = weight_bin)) +
  geom_histogram(binwidth = 25) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 20kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity, scale = "free_x")

ggplot(too_few_counts_low_S_20, aes(x = too_few_count, fill = weight_bin)) +
  geom_histogram(bins = 25) +
  geom_vline(xintercept = too_few_target, color = "red") +
  ggtitle("Section Counts by 'TOO_FEW' Measurements per Week - Scale 20kg") +
  xlab("Number of 'TOO_FEW' Measurements per week") +
  ylab("Number of Sections") +
  facet_wrap(~ sensitivity, scale = "free_x")


#### Influence of LOCATIONID ====
too_few_counts_mean <- too_few_counts %>%
  group_by(LOCATIONID, SCALE_NAME) %>%
  summarise(mean_too_few_count = mean(too_few_count))

#top 10 locations with highest TOO_FEW means per section:
top_too_few <- too_few_counts_mean %>%
  group_by(SCALE_NAME) %>%
  top_n(10, mean_too_few_count) %>%
  arrange(SCALE_NAME, desc(mean_too_few_count)) 
top_too_few  %>% unique(LOCATIONID)


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

# Dataset for analysis ====

df_ana <- df %>% 
  select(section_id, sensitivity, weight, initiatortype, weightclassification, 
         weight_bin, relevant_for_weight_analysis, SCALE_NAME, BOX_NAME, CUSTOMER_NUMBER, 
         LOCATIONID, SUPPLIER_ITEM_NUMBER, ITEM_WEIGHT, week, TOO_FEW_per_week, 
         Fraction_TOO_MANY, start_time, end_time, span)

na_count_df_ana <- colSums(is.na(df_ana))
# Print the results
print(na_count_df_ana)


# Analysis of the effect of weight ====

## Scale types ====


# Model the influence of SCALE_NAME on TOO_FEW_per_week and Fraction_TOO_MANY for the different values of sensitivity






## Influence on TOO_FEW ====

### Random Forest ====
# Load the randomForest package
library(randomForest)

# Set the seed for reproducibility
set.seed(123)

# Fit a random forest model to predict TOO_FEW_per_week
rf_model_TFW <- randomForest(TOO_FEW_per_week ~ weight + weight_bin + 
                               SCALE_NAME + ITEM_WEIGHT + LOCATIONID + SUPPLIER_ITEM_NUMBER,
                             data = df_weight, importance = TRUE)

# Print the variable importance
print(importance(rf_model_TFW))

# Plot the variable importance
varImpPlot(rf_model_TFW)


### Fit a Poisson GLM (does not work) ====
df_ana$weight_bin <- factor(df_ana$weight_bin)
df_ana$SUPPLIER_ITEM_NUMBER <- factor(df_ana$SUPPLIER_ITEM_NUMBER)
glm_model_TFW <- glm(TOO_FEW_per_week ~ weight + weight_bin + SCALE_NAME +
                       ITEM_WEIGHT + SCALE_NAME + ITEM_WEIGHT + LOCATIONID +
                       SUPPLIER_ITEM_NUMBER, data = df_weight, family = poisson)

# Print model summary
summary(glm_model_TFW)

# Assess variable influence using Wald test
library(car)
Anova(glm_model_TFW, type = "II")


