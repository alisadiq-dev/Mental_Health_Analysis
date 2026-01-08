# ============================================================================
# Impact of Screen Time on Mental Health - Analysis Script
# Date: January 2026
# ============================================================================

# ---- 1. Setup and Load Libraries ----
# Install packages if not already installed
required_packages <- c("ggplot2", "dplyr", "corrplot", "psych")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")

# Load libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(psych)

# ---- 2. Set Working Directory and Load Data ----
# Set your working directory
setwd("~/Desktop/Mental_Health_Analysis")

# Load the dataset
data <- read.csv("data/digital_diet_mental_health.csv", header = TRUE)

# ---- 3. Data Overview and Summary ----
cat("\n========== DATA OVERVIEW ==========\n")
cat("\nDimensions of dataset:\n")
print(dim(data))

cat("\nStructure of dataset:\n")
str(data)

cat("\nFirst 6 rows:\n")
print(head(data))

cat("\nSummary Statistics:\n")
print(summary(data))

# ---- 4. Location Distribution Table ----
cat("\n========== LOCATION DISTRIBUTION ==========\n")
location_table <- table(data$location)
print(location_table)

# ---- 5. Screen Time Descriptive Statistics ----
cat("\n========== SCREEN TIME STATISTICS ==========\n")
screen_time_table <- data.frame(
  Daily_Screen_Time_Mean = mean(data$daily_screen_time_hours, na.rm = TRUE),
  Daily_Screen_Time_SD   = sd(data$daily_screen_time_hours, na.rm = TRUE),
  
  Phone_Usage_Mean = mean(data$phone_usage_hours, na.rm = TRUE),
  Phone_Usage_SD   = sd(data$phone_usage_hours, na.rm = TRUE),
  
  Social_Media_Mean = mean(data$social_media_hours, na.rm = TRUE),
  Social_Media_SD   = sd(data$social_media_hours, na.rm = TRUE)
)
print(screen_time_table)

# ---- 6. Mental Health Descriptive Statistics ----
cat("\n========== MENTAL HEALTH STATISTICS ==========\n")
mental_health_table <- data.frame(
  Mental_Health_Mean = mean(data$mental_health_score, na.rm = TRUE),
  Mental_Health_SD   = sd(data$mental_health_score, na.rm = TRUE),
  
  Anxiety_Mean = mean(data$weekly_anxiety_score, na.rm = TRUE),
  Anxiety_SD   = sd(data$weekly_anxiety_score, na.rm = TRUE),
  
  Depression_Mean = mean(data$weekly_depression_score, na.rm = TRUE),
  Depression_SD   = sd(data$weekly_depression_score, na.rm = TRUE)
)
print(mental_health_table)

# ---- 7. Lifestyle Descriptive Statistics ----
cat("\n========== LIFESTYLE STATISTICS ==========\n")
lifestyle_table <- data.frame(
  Sleep_Mean = mean(data$sleep_duration_hours, na.rm = TRUE),
  Sleep_SD   = sd(data$sleep_duration_hours, na.rm = TRUE),
  
  Physical_Activity_Mean = mean(data$physical_activity_hours_per_week, na.rm = TRUE),
  Physical_Activity_SD   = sd(data$physical_activity_hours_per_week, na.rm = TRUE),
  
  Mindfulness_Mean = mean(data$mindfulness_minutes_per_day, na.rm = TRUE),
  Mindfulness_SD   = sd(data$mindfulness_minutes_per_day, na.rm = TRUE)
)
print(lifestyle_table)

# ---- 8. Complete Descriptive Statistics Table ----
cat("\n========== COMPLETE DESCRIPTIVE STATISTICS ==========\n")
numeric_vars <- data %>% select(where(is.numeric))
descriptive_stats <- data.frame(
  Variable = names(numeric_vars),
  Mean = sapply(numeric_vars, mean, na.rm = TRUE),
  SD = sapply(numeric_vars, sd, na.rm = TRUE),
  Min = sapply(numeric_vars, min, na.rm = TRUE),
  Max = sapply(numeric_vars, max, na.rm = TRUE),
  Median = sapply(numeric_vars, median, na.rm = TRUE)
)
rownames(descriptive_stats) <- NULL
print(descriptive_stats)

# ---- 9. Correlation Analysis ----
cat("\n========== CORRELATION ANALYSIS ==========\n")

# Correlation between Screen Time and Mental Health Score
cor_screen_mental <- cor(data$daily_screen_time_hours, data$mental_health_score, 
                          use = "complete.obs")
cat("\nCorrelation (Screen Time vs Mental Health):", round(cor_screen_mental, 4), "\n")

# Correlation between Screen Time and Anxiety
cor_screen_anxiety <- cor(data$daily_screen_time_hours, data$weekly_anxiety_score, 
                           use = "complete.obs")
cat("Correlation (Screen Time vs Anxiety):", round(cor_screen_anxiety, 4), "\n")

# Correlation between Screen Time and Depression
cor_screen_depression <- cor(data$daily_screen_time_hours, data$weekly_depression_score, 
                              use = "complete.obs")
cat("Correlation (Screen Time vs Depression):", round(cor_screen_depression, 4), "\n")

# Correlation between Physical Activity and Mental Health
cor_activity_mental <- cor(data$physical_activity_hours_per_week, data$mental_health_score, 
                            use = "complete.obs")
cat("Correlation (Physical Activity vs Mental Health):", round(cor_activity_mental, 4), "\n")

# Full Correlation Matrix
cat("\n--- Full Correlation Matrix ---\n")
cor_vars <- data %>% select(daily_screen_time_hours, phone_usage_hours, social_media_hours,
                            mental_health_score, weekly_anxiety_score, weekly_depression_score,
                            physical_activity_hours_per_week, sleep_duration_hours)
cor_matrix <- cor(cor_vars, use = "complete.obs")
print(round(cor_matrix, 3))

# ---- 10. Normality Tests (Shapiro-Wilk) ----
cat("\n========== NORMALITY TESTS (Shapiro-Wilk) ==========\n")

# Test for Screen Time
shapiro_screen <- shapiro.test(data$daily_screen_time_hours)
cat("\nDaily Screen Time Hours:\n")
cat("  W =", round(shapiro_screen$statistic, 4), ", p-value =", round(shapiro_screen$p.value, 4), "\n")

# Test for Mental Health Score
shapiro_mental <- shapiro.test(data$mental_health_score)
cat("\nMental Health Score:\n")
cat("  W =", round(shapiro_mental$statistic, 4), ", p-value =", round(shapiro_mental$p.value, 4), "\n")

# Test for Anxiety Score
shapiro_anxiety <- shapiro.test(data$weekly_anxiety_score)
cat("\nWeekly Anxiety Score:\n")
cat("  W =", round(shapiro_anxiety$statistic, 4), ", p-value =", round(shapiro_anxiety$p.value, 4), "\n")

# Test for Depression Score
shapiro_depression <- shapiro.test(data$weekly_depression_score)
cat("\nWeekly Depression Score:\n")
cat("  W =", round(shapiro_depression$statistic, 4), ", p-value =", round(shapiro_depression$p.value, 4), "\n")

# Test for Physical Activity
shapiro_activity <- shapiro.test(data$physical_activity_hours_per_week)
cat("\nPhysical Activity Hours:\n")
cat("  W =", round(shapiro_activity$statistic, 4), ", p-value =", round(shapiro_activity$p.value, 4), "\n")

# ---- 11. Regression Analysis ----
cat("\n========== REGRESSION ANALYSIS ==========\n")

# Linear Regression: Screen Time -> Mental Health
model1 <- lm(mental_health_score ~ daily_screen_time_hours, data = data)
cat("\n--- Model 1: Screen Time predicting Mental Health ---\n")
print(summary(model1))

# Linear Regression: Physical Activity -> Mental Health
model2 <- lm(mental_health_score ~ physical_activity_hours_per_week, data = data)
cat("\n--- Model 2: Physical Activity predicting Mental Health ---\n")
print(summary(model2))

# Multiple Regression
model3 <- lm(mental_health_score ~ daily_screen_time_hours + physical_activity_hours_per_week + 
               sleep_duration_hours, data = data)
cat("\n--- Model 3: Multiple Regression ---\n")
print(summary(model3))

# ---- 12. Visualization - Scatter Plots ----
cat("\n========== GENERATING PLOTS ==========\n")

# Plot 1: Screen Time vs Mental Health Score
png("output/scatter_screentime_mentalhealth.png", width = 800, height = 600)
plot(data$daily_screen_time_hours, data$mental_health_score,
     main = "Screen Time vs Mental Health Score",
     xlab = "Daily Screen Time (Hours)",
     ylab = "Mental Health Score",
     pch = 19, col = "steelblue", cex = 1.5)
abline(lm(mental_health_score ~ daily_screen_time_hours, data = data), 
       col = "red", lwd = 2)
legend("topright", legend = paste("r =", round(cor_screen_mental, 3)),
       bty = "n", cex = 1.2)
dev.off()
cat("Saved: output/scatter_screentime_mentalhealth.png\n")

# Plot 2: Physical Activity vs Mental Health Score
png("output/scatter_activity_mentalhealth.png", width = 800, height = 600)
plot(data$physical_activity_hours_per_week, data$mental_health_score,
     main = "Physical Activity vs Mental Health Score",
     xlab = "Physical Activity (Hours/Week)",
     ylab = "Mental Health Score",
     pch = 19, col = "forestgreen", cex = 1.5)
abline(lm(mental_health_score ~ physical_activity_hours_per_week, data = data), 
       col = "red", lwd = 2)
legend("bottomright", legend = paste("r =", round(cor_activity_mental, 3)),
       bty = "n", cex = 1.2)
dev.off()
cat("Saved: output/scatter_activity_mentalhealth.png\n")

# Plot 3: Screen Time vs Anxiety Score
png("output/scatter_screentime_anxiety.png", width = 800, height = 600)
plot(data$daily_screen_time_hours, data$weekly_anxiety_score,
     main = "Screen Time vs Weekly Anxiety Score",
     xlab = "Daily Screen Time (Hours)",
     ylab = "Weekly Anxiety Score",
     pch = 19, col = "darkorange", cex = 1.5)
abline(lm(weekly_anxiety_score ~ daily_screen_time_hours, data = data), 
       col = "red", lwd = 2)
legend("bottomright", legend = paste("r =", round(cor_screen_anxiety, 3)),
       bty = "n", cex = 1.2)
dev.off()
cat("Saved: output/scatter_screentime_anxiety.png\n")

# Plot 4: Correlation Heatmap
png("output/correlation_heatmap.png", width = 900, height = 800)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix Heatmap",
         mar = c(0,0,2,0))
dev.off()
cat("Saved: output/correlation_heatmap.png\n")

# Plot 5: Histograms for Normality Check
png("output/histogram_screentime.png", width = 800, height = 600)
hist(data$daily_screen_time_hours, 
     main = "Distribution of Daily Screen Time",
     xlab = "Daily Screen Time (Hours)",
     col = "lightblue", border = "white",
     breaks = 10)
dev.off()
cat("Saved: output/histogram_screentime.png\n")

png("output/histogram_mentalhealth.png", width = 800, height = 600)
hist(data$mental_health_score, 
     main = "Distribution of Mental Health Score",
     xlab = "Mental Health Score",
     col = "lightgreen", border = "white",
     breaks = 10)
dev.off()
cat("Saved: output/histogram_mentalhealth.png\n")

# ---- 13. Gender-wise Analysis ----
cat("\n========== GENDER-WISE ANALYSIS ==========\n")
gender_summary <- data %>%
  group_by(gender) %>%
  summarise(
    Count = n(),
    Avg_Screen_Time = mean(daily_screen_time_hours, na.rm = TRUE),
    Avg_Mental_Health = mean(mental_health_score, na.rm = TRUE),
    Avg_Anxiety = mean(weekly_anxiety_score, na.rm = TRUE),
    Avg_Depression = mean(weekly_depression_score, na.rm = TRUE)
  )
print(gender_summary)

# ---- 14. Location-wise Analysis ----
cat("\n========== LOCATION-WISE ANALYSIS ==========\n")
location_summary <- data %>%
  group_by(location) %>%
  summarise(
    Count = n(),
    Avg_Screen_Time = mean(daily_screen_time_hours, na.rm = TRUE),
    Avg_Mental_Health = mean(mental_health_score, na.rm = TRUE),
    Avg_Physical_Activity = mean(physical_activity_hours_per_week, na.rm = TRUE)
  )
print(location_summary)

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat("All plots have been saved to the 'output/' folder.\n")
