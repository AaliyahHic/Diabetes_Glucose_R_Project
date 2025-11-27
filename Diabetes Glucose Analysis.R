# Aaliyah Hickson
# Diabetes Glucose Analysis in R
# Dataset: diabetes_data.csv

# install.packages("tidyverse")

install.packages("tidyverse")

# load Tidyverse

library(tidyverse)

# 1. Load data
diabetes <- read_csv("diabetes_data.csv")

# 2. Clean and prepare data 
diabetes <- diabetes %>%
  mutate(
    Date = as.Date(Date),
    TimeOfDay = factor(TimeOfDay, levels = c("Morning", "Afternoon", "Night")),
    HighGlucose = if_else(BloodGlucose >= 180, "High", "In Range")
  )

# Quick look at data
glimpse(diabetes)
summary(diabetes$BloodGlucose)

# 3. Summary statistics 
avg_glucose <- mean(diabetes$BloodGlucose)
min_glucose <- min(diabetes$BloodGlucose)
max_glucose <- max(diabetes$BloodGlucose)

avg_glucose
min_glucose
max_glucose

# Average glucose by time of day
avg_by_time <- diabetes %>%
  group_by(TimeOfDay) %>%
  summarise(
    AvgGlucose = mean(BloodGlucose),
    Readings = n()
  )

avg_by_time

# 4. Plot: Glucose trend over time 
ggplot(diabetes, aes(x = Date, y = BloodGlucose)) +
  geom_line(group = 1) +
  geom_point() +
  labs(
    title = "Blood Glucose Over Time",
    x = "Date",
    y = "Blood Glucose (mg/dL)"
  )

# 5. Plot: Average glucose by time of day 
ggplot(avg_by_time, aes(x = TimeOfDay, y = AvgGlucose)) +
  geom_col() +
  labs(
    title = "Average Blood Glucose by Time of Day",
    x = "Time of Day",
    y = "Average Blood Glucose (mg/dL)"
  )

# 6. Plot: Carbs vs Blood Glucose
ggplot(diabetes, aes(x = Carbs, y = BloodGlucose)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Carbs vs Blood Glucose",
    x = "Carbs (g)",
    y = "Blood Glucose (mg/dL)"
  )

# 7. Simple regression model: BloodGlucose ~ Carbs + InsulinUnits ----
model <- lm(BloodGlucose ~ Carbs + InsulinUnits, data = diabetes)
summary(model)

# 8. Export summary stats to CSV 
summary_table <- tibble(
  Metric = c("Average Glucose", "Minimum Glucose", "Maximum Glucose"),
  Value = c(avg_glucose, min_glucose, max_glucose)
)

write_csv(summary_table, "summary_stats_diabetes.csv")
