# Climate Change Survey

## Purpose
This survey aims to gather public opinions on climate change, including awareness, attitudes, and support for various policies.

## Key Sections
1. **Perceptions of Climate Change**
   - Understanding of climate change causes and impacts.
   - Beliefs about the severity and urgency of the issue.

2. **Personal Responsibility**
   - Willingness to adopt sustainable practices.
   - Perceived barriers to sustainable behavior.

3. **Policy Support**
   - Public opinions on carbon pricing, renewable energy subsidies, and emission regulations.

## Data Usage
The data will inform policy recommendations and public engagement strategies aimed at combating climate change.
---
title: "climate change analysis"
author: "tonny kiptoo mutai"
date: "8/14/2024"
output:
---

```{r setup, include=FALSE}
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(mice)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(magrittr)
library(readr)
library(summarytools)
library(glmnet)
library(stringr)
library(corrplot)
library(e1071)
library(data.table)
library(tinytex)
library(randomForest)
library(caret)
opts_chunk$set(echo = TRUE)
```

```{r}
climate <- read.csv(file = "climate_change.csv", header = TRUE, sep = ",")
#cleaning columns name
names(climate)<-make.names(names(climate),unique = TRUE)
```

```{r}
climate %>% select(gender,age_bracket,education_level,regulary_drive,car_mileage_annually,climate_change_awareness,climate_change_concerned,information_origin,Climate_change_frightens,developing_countries_blamed,developed_countries_blamed,climate_change_improves_life_quality,pollution_affecting_climate_change,Climate_change_frightens,Flooding_increasing,closest_statement_to_yours,energy_consumption_increases_climate_change,energy_consumption_increases_climate_change)->climate1
```

```{r}
# checking structure of the data
str(climate1)
```

```{r}
#Check NAs
NA1 = sort(sapply(climate1, function(x) 
  { 
  sum(is.na(x))
  } ))



```


```{r}
# Define a function to replace "24-dec" with a random range
replace_24_Dec <- function(x) {
  # Define the replacement ranges
  ranges <- c("12-24","25-34","35-44","45-54","55-64","65-74","75-84","85 or over")
  
  # Replace "24-Dec" with a random value from the ranges
  if (x == "24-Dec") {
    return(sample(ranges, 1))
  } else {
    return(x)
  }
}

# Apply the replacement function using dplyr
climate3 <- climate1 %>%
  mutate(age_bracket = sapply(climate1$age_bracket, replace_24_Dec))
```

```{r}
# Replace NA values in car_mileage_annually with 0
climate4 <- climate3 %>%
  mutate(car_mileage_annually = replace_na(car_mileage_annually, 0))

# Verify changes
summary(climate4)
```
```{r}
# replacing NAS in categorical variables
fixed_values <- list(
  gender = "Unknown",
  age_bracket = "Not Specified",
  education_level = "Not Specified",
  regulary_drive = "Unknown",
  climate_change_awareness = "Unknown",
  climate_change_concerned = "Unknown",
  information_origin = "Unknown",
  Climate_change_frightens = "Unknown",
  developing_countries_blamed = "Unknown",
  developed_countries_blamed = "Unknown",
  climate_change_improves_life_quality = "Unknown",
  pollution_affecting_climate_change = "Unknown",
  Flooding_increasing = "Unknown",
  closest_statement_to_yours = "Unknown",
  energy_consumption_increases_climate_change = "Unknown")
# Function to replace NA with random choice from available categories
random_impute <- function(v) {
  categories <- unique(na.omit(v))
  if (length(categories) > 0) {
    return(ifelse(is.na(v), sample(categories, size = length(v), replace = TRUE), v))
  }
  return(v)
}

# Apply random imputation
climate4<- climate4 %>%
  mutate(across(all_of(names(fixed_values)), random_impute))
# Check for remaining NAs
a=sum(is.na(climate3))

# Check the distribution of values
print(a)
```


```{r}
# Recode climate_change_awareness correctly
climate4 %>% mutate(climate_change_awareness = case_when(
  climate_change_awareness == "Yes" ~ "YES",
  TRUE ~ as.character(climate_change_awareness)  # Keep NA and any other values unchanged
)) -> climate5

```
```{r}
# Recode reguraly drive correctly
climate4 %>% mutate(regulary_drive = case_when(
  regulary_drive == "Yes" ~ "YES",
  TRUE ~ as.character(climate_change_awareness)  # Keep NA and any other values unchanged
)) -> climate5
```



```{r}
# convert categorical variables into factors
climate$gender = as.factor(climate1$gender)
climate1$age_bracket=as.factor(climate1$age_bracket)
climate1$education_level=as.factor(climate1$education_level)
climate1$regulary_drive=as.factor(climate1$regulary_drive)
climate1$climate_change_awareness=as.factor(climate1$climate_change_awareness)
climate1$climate_change_concerned=as.factor(climate1$climate_change_concerned)
climate1$car_mileage_annually=as.numeric(climate1$car_mileage_annually)
climate1$information_origin=as.factor(climate1$information_origin)
climate1$Climate_change_frightens=as.factor(climate1$Climate_change_frightens)
climate1$developing_countries_blamed=as.factor(climate1$developing_countries_blamed)
climate1$developed_countries_blamed=as.factor(climate1$developed_countries_blamed)
climate1$climate_change_improves_life_quality=as.factor(climate1$climate_change_improves_life_quality)
climate1$pollution_affecting_climate_change=as.factor(climate1$pollution_affecting_climate_change)
climate1$Flooding_increasing=as.factor(climate1$Flooding_increasing)
climate1$closest_statement_to_yours=as.factor(climate1$closest_statement_to_yours)
climate1$energy_consumption_increases_climate_change=as.factor(climate1$energy_consumption_increases_climate_change)
```

```{r}
# Summary of numerical variables
summary(climate5$car_millage_annually)

# Frequency tables for categorical variables
table(climate5$gender)
table(climate5$age_bracket)
table(climate5$education_level)
# Add tables for other categorical variables
```
```{r}
# univariate data visualization
# Histograms for numerical variables
ggplot(climate5, aes(x = car_mileage_annually)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Distribution of Car Mileage Annually", x = "Car Mileage Annually", y = "Frequency")

# Bar plots for categorical variables
ggplot(climate5, aes(x = gender)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count")

ggplot(climate5, aes(x = age_bracket)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Age Bracket", x = "Age Bracket", y = "Count")


```


```{r}

# Create the stacked bar plot
ggplot(climate5, aes(x = information_origin, fill = education_level)) +
  geom_bar(position = "stack") +
  facet_wrap(~ gender) +
  theme_minimal() +
  labs(
    title = "Education Level by Information Origin and Gender",
    x = "Information Origin",
    y = "Count",
    fill = "Education Level"
  ) +
  scale_fill_brewer(palette = "Set3") # Optional: change color palette
```

```{r}
# Boxplot of car mileage by gender

ggplot(climate5, aes(x = gender, y = log(car_mileage_annually))) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Car Mileage Annually by Gender", x = "Gender", y = "Car Mileage Annually")

# Bar plot of climate change awareness by age bracket
# Create the bar plot
ggplot(climate5, aes(x = age_bracket, y = climate_change_awareness, fill = age_bracket)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Climate Change Awareness by Age Bracket",
    x = "Age Bracket",
    y = "Awareness Score"
  ) +
  scale_fill_brewer(palette = "Set3") # Optional: change color palette
```
```{r}
# Combine the data into a long format
data_long <- climate5 %>%
  select(developing_countries_blamed, developed_countries_blamed) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Count")
# Create the bar plot
ggplot(data_long, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Counts of Countries Blamed",
    x = "Category",
    y = "Count",
    fill = "Category"
  ) +
  scale_fill_brewer(palette = "Set2") 
```

```{r}
# Bin the car_mileage into categories
climate5 <- climate5 %>%
  mutate(
    car_mileage_category = cut(
      car_mileage_annually,
      breaks = c(-Inf, 10000, 20000, 30000, 40000, Inf),
      labels = c("0-10k", "10k-20k", "20k-30k", "30k-40k", "40k+")
    )
  )
# Calculate proportions for Climate Change Awareness
climate_awareness_counts <- climate5 %>%
  count(climate_change_awareness) %>%
  mutate(Proportion = n / sum(n))

# Calculate proportions for Car Mileage Categories
car_mileage_counts <- climate5 %>%
  count(car_mileage_category) %>%
  mutate(Proportion = n / sum(n))

ggplot(climate_awareness_counts, aes(x = "", y = Proportion, fill = climate_change_awareness)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Distribution of Climate Change Awareness",
    fill = "Awareness Level"
  ) +
  scale_fill_brewer(palette = "Set3")  # Optional: choose a color palette

# Pie chart for Car Mileage Categories
ggplot(car_mileage_counts, aes(x = "", y = Proportion, fill = car_mileage_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Distribution of Car Mileage",
    fill = "Mileage Range"
  ) +
  scale_fill_brewer(palette = "Set3")  # Optional: choose a color palette

```

```{r}

# Create a contingency table
contingency_table <- table(climate5$climate_change_awareness, climate5$gender)
print(contingency_table)


```
```{r}
# Summarize data by age bracket and education level, with average car mileage
pivot_summary <- climate5 %>%
  group_by(age_bracket, education_level) %>%
  summarize(
    Average_Car_Mileage = mean(car_mileage_annually, na.rm = TRUE),
    Climate_Change_Awareness_Count = n_distinct(climate_change_awareness),
    .groups = 'drop'
  )
# Pivot data to a wider format for better visualization
pivot_wide <- pivot_summary %>%
  pivot_wider(
    names_from = education_level,
    values_from = c(Average_Car_Mileage, Climate_Change_Awareness_Count),
    names_sep = "_"
  )
# Convert data to data.table format
data_dt <- as.data.table(climate5)

# Summarize by age bracket and education level, with average car mileage
pivot_summary_dt <- data_dt[, .(
  Average_Car_Mileage = mean(car_mileage_annually, na.rm = TRUE),
  Climate_Change_Awareness_Count = uniqueN(climate_change_awareness)
), by = .(age_bracket, education_level)]

# Pivot data to wider format
pivot_wide_dt <- dcast(pivot_summary_dt, age_bracket ~ education_level, value.var = c("Average_Car_Mileage", "Climate_Change_Awareness_Count"))
# Print the pivot table
print(pivot_wide)
# or for data.table
```


```{r,message=FALSE}
# Chi-Squared Test for Independence
# Ensure categorical variables are factors
climate5$climate_change_awareness <- as.factor(climate5$climate_change_awareness)
climate5$gender <- as.factor(climate5$gender)

# Chi-squared test for independence
chisq_test_result <- chisq.test(table(climate5$regulary_drive, climate1$gender))

# Print the result of the chi-squared test
print(chisq_test_result)
```



```{r,message=FALSE}
# Fit a logistic regression model if `climate_change_awareness` is binary
model <- glm(climate_change_awareness ~ education_level + age_bracket + 
             information_origin + gender + regulary_drive, 
             data = climate5, family = binomial)

# Summary of the model
summary(model)


```



```{r}
# Predict with the logistic regression model
predictions <- predict(model, newdata = climate5, type = "response")
predicted_class <- ifelse(predictions > 0.5, "YES", "NO")

# Create confusion matrix
confusion_matrix <- table(predicted_class, climate1$climate_change_awareness)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))
```

```{r}
# Save cleaned data to a CSV file
# Save cleaned data to a CSV file on the desktop
desktop_path <-"C:/Users/PROBOOK/Desktop"
write.csv(climate1, file = paste0(desktop_path, "cleaned_climate_data.csv"), row.names = FALSE)
