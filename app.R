# ============================================================================
# Impact of Screen Time on Mental Health - Shiny Dashboard
# Author: [Your Name]
# Date: January 2026
# ============================================================================

# ---- Load Libraries ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(corrplot)

# ---- Load Data ----
data <- read.csv("data/digital_diet_mental_health.csv", header = TRUE)

# Define numeric variables for selection
numeric_vars <- c("daily_screen_time_hours", "phone_usage_hours", "social_media_hours",
                  "sleep_duration_hours", "physical_activity_hours_per_week", 
                  "mindfulness_minutes_per_day", "mental_health_score", 
                  "weekly_anxiety_score", "weekly_depression_score", "age")

# ---- UI Definition ----
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(title = "Mental Health Analysis Dashboard"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Descriptive Statistics", tabName = "descriptive", icon = icon("table")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("chart-line")),
      menuItem("Normality Tests", tabName = "normality", icon = icon("chart-bar")),
      menuItem("Scatter Plots", tabName = "scatter", icon = icon("braille")),
      menuItem("Regression Analysis", tabName = "regression", icon = icon("calculator"))
    ),
    hr(),
    
    # Variable Selection for Normality Test
    h4("Select Variable for Analysis:", style = "padding-left: 15px; color: white;"),
    selectInput("norm_var", "Variable for Normality Test:",
                choices = numeric_vars,
                selected = "daily_screen_time_hours"),
    
    # Variables for Scatter Plot
    selectInput("scatter_x", "Scatter Plot X-Axis:",
                choices = numeric_vars,
                selected = "daily_screen_time_hours"),
    
    selectInput("scatter_y", "Scatter Plot Y-Axis:",
                choices = numeric_vars,
                selected = "mental_health_score")
  ),
  
  # Body
  dashboardBody(
    tabItems(
      
      # ---- Tab 1: Overview ----
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_users", width = 3),
                valueBoxOutput("avg_screen_time", width = 3),
                valueBoxOutput("avg_mental_health", width = 3),
                valueBoxOutput("avg_anxiety", width = 3)
              ),
              fluidRow(
                box(title = "Dataset Preview", status = "primary", solidHeader = TRUE,
                    width = 12, collapsible = TRUE,
                    DTOutput("data_table"))
              ),
              fluidRow(
                box(title = "Data Summary", status = "info", solidHeader = TRUE,
                    width = 12, collapsible = TRUE,
                    verbatimTextOutput("data_summary"))
              )
      ),
      
      # ---- Tab 2: Descriptive Statistics ----
      tabItem(tabName = "descriptive",
              fluidRow(
                box(title = "Screen Time Statistics", status = "primary", solidHeader = TRUE,
                    width = 6, tableOutput("screen_stats")),
                box(title = "Mental Health Statistics", status = "danger", solidHeader = TRUE,
                    width = 6, tableOutput("mental_stats"))
              ),
              fluidRow(
                box(title = "Lifestyle Statistics", status = "success", solidHeader = TRUE,
                    width = 6, tableOutput("lifestyle_stats")),
                box(title = "Complete Descriptive Statistics", status = "warning", solidHeader = TRUE,
                    width = 6, tableOutput("complete_stats"))
              ),
              fluidRow(
                box(title = "Gender-wise Summary", status = "info", solidHeader = TRUE,
                    width = 6, tableOutput("gender_summary")),
                box(title = "Location-wise Summary", status = "info", solidHeader = TRUE,
                    width = 6, tableOutput("location_summary"))
              )
      ),
      
      # ---- Tab 3: Correlation Analysis ----
      tabItem(tabName = "correlation",
              fluidRow(
                box(title = "Correlation Matrix", status = "primary", solidHeader = TRUE,
                    width = 12, tableOutput("cor_matrix"))
              ),
              fluidRow(
                box(title = "Correlation Heatmap", status = "info", solidHeader = TRUE,
                    width = 12, height = "600px", plotOutput("cor_heatmap", height = "500px"))
              ),
              fluidRow(
                box(title = "Key Correlations", status = "warning", solidHeader = TRUE,
                    width = 12, verbatimTextOutput("key_correlations"))
              )
      ),
      
      # ---- Tab 4: Normality Tests ----
      tabItem(tabName = "normality",
              fluidRow(
                box(title = "Shapiro-Wilk Normality Test Results", status = "primary", solidHeader = TRUE,
                    width = 6, verbatimTextOutput("shapiro_result")),
                box(title = "Interpretation", status = "info", solidHeader = TRUE,
                    width = 6, verbatimTextOutput("shapiro_interpretation"))
              ),
              fluidRow(
                box(title = "Histogram", status = "success", solidHeader = TRUE,
                    width = 6, plotOutput("histogram_plot")),
                box(title = "Q-Q Plot", status = "warning", solidHeader = TRUE,
                    width = 6, plotOutput("qq_plot"))
              ),
              fluidRow(
                box(title = "All Variables Normality Test", status = "danger", solidHeader = TRUE,
                    width = 12, tableOutput("all_normality"))
              )
      ),
      
      # ---- Tab 5: Scatter Plots ----
      tabItem(tabName = "scatter",
              fluidRow(
                box(title = "Custom Scatter Plot with Regression Line", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    plotOutput("custom_scatter", height = "500px"))
              ),
              fluidRow(
                box(title = "Screen Time vs Mental Health", status = "danger", solidHeader = TRUE,
                    width = 6, plotOutput("scatter1")),
                box(title = "Physical Activity vs Mental Health", status = "success", solidHeader = TRUE,
                    width = 6, plotOutput("scatter2"))
              ),
              fluidRow(
                box(title = "Screen Time vs Anxiety", status = "warning", solidHeader = TRUE,
                    width = 6, plotOutput("scatter3")),
                box(title = "Screen Time vs Depression", status = "info", solidHeader = TRUE,
                    width = 6, plotOutput("scatter4"))
              )
      ),
      
      # ---- Tab 6: Regression Analysis ----
      tabItem(tabName = "regression",
              fluidRow(
                box(title = "Simple Regression: Screen Time → Mental Health", 
                    status = "primary", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("regression1")),
                box(title = "Simple Regression: Physical Activity → Mental Health", 
                    status = "success", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("regression2"))
              ),
              fluidRow(
                box(title = "Multiple Regression Model", status = "danger", solidHeader = TRUE,
                    width = 12, verbatimTextOutput("regression3"))
              )
      )
    )
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {
  
  # ---- Value Boxes ----
  output$total_users <- renderValueBox({
    valueBox(nrow(data), "Total Users", icon = icon("users"), color = "blue")
  })
  
  output$avg_screen_time <- renderValueBox({
    valueBox(round(mean(data$daily_screen_time_hours, na.rm = TRUE), 2), 
             "Avg Screen Time (hrs)", icon = icon("mobile"), color = "red")
  })
  
  output$avg_mental_health <- renderValueBox({
    valueBox(round(mean(data$mental_health_score, na.rm = TRUE), 2), 
             "Avg Mental Health Score", icon = icon("heart"), color = "green")
  })
  
  output$avg_anxiety <- renderValueBox({
    valueBox(round(mean(data$weekly_anxiety_score, na.rm = TRUE), 2), 
             "Avg Anxiety Score", icon = icon("exclamation-triangle"), color = "yellow")
  })
  
  # ---- Data Table ----
  output$data_table <- renderDT({
    datatable(data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_summary <- renderPrint({
    summary(data)
  })
  
  # ---- Descriptive Statistics Tables ----
  output$screen_stats <- renderTable({
    data.frame(
      Metric = c("Daily Screen Time", "Phone Usage", "Social Media"),
      Mean = c(mean(data$daily_screen_time_hours, na.rm = TRUE),
               mean(data$phone_usage_hours, na.rm = TRUE),
               mean(data$social_media_hours, na.rm = TRUE)),
      SD = c(sd(data$daily_screen_time_hours, na.rm = TRUE),
             sd(data$phone_usage_hours, na.rm = TRUE),
             sd(data$social_media_hours, na.rm = TRUE))
    )
  }, digits = 4)
  
  output$mental_stats <- renderTable({
    data.frame(
      Metric = c("Mental Health Score", "Anxiety Score", "Depression Score"),
      Mean = c(mean(data$mental_health_score, na.rm = TRUE),
               mean(data$weekly_anxiety_score, na.rm = TRUE),
               mean(data$weekly_depression_score, na.rm = TRUE)),
      SD = c(sd(data$mental_health_score, na.rm = TRUE),
             sd(data$weekly_anxiety_score, na.rm = TRUE),
             sd(data$weekly_depression_score, na.rm = TRUE))
    )
  }, digits = 4)
  
  output$lifestyle_stats <- renderTable({
    data.frame(
      Metric = c("Sleep Duration", "Physical Activity", "Mindfulness"),
      Mean = c(mean(data$sleep_duration_hours, na.rm = TRUE),
               mean(data$physical_activity_hours_per_week, na.rm = TRUE),
               mean(data$mindfulness_minutes_per_day, na.rm = TRUE)),
      SD = c(sd(data$sleep_duration_hours, na.rm = TRUE),
             sd(data$physical_activity_hours_per_week, na.rm = TRUE),
             sd(data$mindfulness_minutes_per_day, na.rm = TRUE))
    )
  }, digits = 4)
  
  output$complete_stats <- renderTable({
    numeric_data <- data %>% select(where(is.numeric))
    data.frame(
      Variable = names(numeric_data),
      Mean = sapply(numeric_data, mean, na.rm = TRUE),
      SD = sapply(numeric_data, sd, na.rm = TRUE),
      Min = sapply(numeric_data, min, na.rm = TRUE),
      Max = sapply(numeric_data, max, na.rm = TRUE)
    )
  }, digits = 2)
  
  output$gender_summary <- renderTable({
    data %>%
      group_by(gender) %>%
      summarise(
        Count = n(),
        Avg_Screen_Time = round(mean(daily_screen_time_hours, na.rm = TRUE), 2),
        Avg_Mental_Health = round(mean(mental_health_score, na.rm = TRUE), 2)
      )
  })
  
  output$location_summary <- renderTable({
    data %>%
      group_by(location) %>%
      summarise(
        Count = n(),
        Avg_Screen_Time = round(mean(daily_screen_time_hours, na.rm = TRUE), 2),
        Avg_Mental_Health = round(mean(mental_health_score, na.rm = TRUE), 2)
      )
  })
  
  # ---- Correlation Analysis ----
  output$cor_matrix <- renderTable({
    cor_vars <- data %>% select(daily_screen_time_hours, phone_usage_hours, 
                                 mental_health_score, weekly_anxiety_score, 
                                 weekly_depression_score, physical_activity_hours_per_week)
    round(cor(cor_vars, use = "complete.obs"), 3)
  }, rownames = TRUE)
  
  output$cor_heatmap <- renderPlot({
    cor_vars <- data %>% select(daily_screen_time_hours, phone_usage_hours, social_media_hours,
                                 mental_health_score, weekly_anxiety_score, weekly_depression_score,
                                 physical_activity_hours_per_week, sleep_duration_hours)
    cor_matrix <- cor(cor_vars, use = "complete.obs")
    corrplot(cor_matrix, method = "color", type = "upper",
             addCoef.col = "black", number.cex = 0.8,
             tl.col = "black", tl.srt = 45,
             title = "Correlation Matrix Heatmap",
             mar = c(0,0,2,0))
  })
  
  output$key_correlations <- renderPrint({
    cat("KEY CORRELATIONS:\n")
    cat("================\n\n")
    cat("Screen Time vs Mental Health:", 
        round(cor(data$daily_screen_time_hours, data$mental_health_score, use = "complete.obs"), 4), "\n")
    cat("Screen Time vs Anxiety:", 
        round(cor(data$daily_screen_time_hours, data$weekly_anxiety_score, use = "complete.obs"), 4), "\n")
    cat("Screen Time vs Depression:", 
        round(cor(data$daily_screen_time_hours, data$weekly_depression_score, use = "complete.obs"), 4), "\n")
    cat("Physical Activity vs Mental Health:", 
        round(cor(data$physical_activity_hours_per_week, data$mental_health_score, use = "complete.obs"), 4), "\n")
    cat("Sleep vs Mental Health:", 
        round(cor(data$sleep_duration_hours, data$mental_health_score, use = "complete.obs"), 4), "\n")
  })
  
  # ---- Normality Tests ----
  output$shapiro_result <- renderPrint({
    var_data <- data[[input$norm_var]]
    test <- shapiro.test(var_data)
    cat("Shapiro-Wilk Normality Test\n")
    cat("===========================\n\n")
    cat("Variable:", input$norm_var, "\n\n")
    cat("W statistic:", round(test$statistic, 4), "\n")
    cat("p-value:", round(test$p.value, 4), "\n")
  })
  
  output$shapiro_interpretation <- renderPrint({
    var_data <- data[[input$norm_var]]
    test <- shapiro.test(var_data)
    cat("INTERPRETATION:\n")
    cat("===============\n\n")
    if(test$p.value > 0.05) {
      cat("p-value > 0.05\n\n")
      cat("RESULT: Data is NORMALLY distributed.\n")
      cat("(Fail to reject null hypothesis)\n")
    } else {
      cat("p-value < 0.05\n\n")
      cat("RESULT: Data is NOT normally distributed.\n")
      cat("(Reject null hypothesis)\n")
    }
  })
  
  output$histogram_plot <- renderPlot({
    var_data <- data[[input$norm_var]]
    hist(var_data, 
         main = paste("Histogram of", input$norm_var),
         xlab = input$norm_var,
         col = "lightblue", border = "white",
         breaks = 10)
  })
  
  output$qq_plot <- renderPlot({
    var_data <- data[[input$norm_var]]
    qqnorm(var_data, main = paste("Q-Q Plot of", input$norm_var))
    qqline(var_data, col = "red", lwd = 2)
  })
  
  output$all_normality <- renderTable({
    results <- data.frame(
      Variable = numeric_vars,
      W_Statistic = NA,
      P_Value = NA,
      Normal = NA
    )
    
    for(i in 1:length(numeric_vars)) {
      if(numeric_vars[i] %in% names(data)) {
        test <- shapiro.test(data[[numeric_vars[i]]])
        results$W_Statistic[i] <- round(test$statistic, 4)
        results$P_Value[i] <- round(test$p.value, 4)
        results$Normal[i] <- ifelse(test$p.value > 0.05, "Yes", "No")
      }
    }
    results
  })
  
  # ---- Scatter Plots ----
  output$custom_scatter <- renderPlot({
    x_var <- data[[input$scatter_x]]
    y_var <- data[[input$scatter_y]]
    correlation <- round(cor(x_var, y_var, use = "complete.obs"), 3)
    
    plot(x_var, y_var,
         main = paste(input$scatter_x, "vs", input$scatter_y),
         xlab = input$scatter_x,
         ylab = input$scatter_y,
         pch = 19, col = "steelblue", cex = 1.5)
    abline(lm(y_var ~ x_var), col = "red", lwd = 2)
    legend("topright", legend = paste("r =", correlation), bty = "n", cex = 1.2)
  })
  
  output$scatter1 <- renderPlot({
    plot(data$daily_screen_time_hours, data$mental_health_score,
         main = "Screen Time vs Mental Health",
         xlab = "Daily Screen Time (Hours)", ylab = "Mental Health Score",
         pch = 19, col = "steelblue", cex = 1.2)
    abline(lm(mental_health_score ~ daily_screen_time_hours, data = data), col = "red", lwd = 2)
  })
  
  output$scatter2 <- renderPlot({
    plot(data$physical_activity_hours_per_week, data$mental_health_score,
         main = "Physical Activity vs Mental Health",
         xlab = "Physical Activity (Hours/Week)", ylab = "Mental Health Score",
         pch = 19, col = "forestgreen", cex = 1.2)
    abline(lm(mental_health_score ~ physical_activity_hours_per_week, data = data), col = "red", lwd = 2)
  })
  
  output$scatter3 <- renderPlot({
    plot(data$daily_screen_time_hours, data$weekly_anxiety_score,
         main = "Screen Time vs Anxiety",
         xlab = "Daily Screen Time (Hours)", ylab = "Weekly Anxiety Score",
         pch = 19, col = "darkorange", cex = 1.2)
    abline(lm(weekly_anxiety_score ~ daily_screen_time_hours, data = data), col = "red", lwd = 2)
  })
  
  output$scatter4 <- renderPlot({
    plot(data$daily_screen_time_hours, data$weekly_depression_score,
         main = "Screen Time vs Depression",
         xlab = "Daily Screen Time (Hours)", ylab = "Weekly Depression Score",
         pch = 19, col = "purple", cex = 1.2)
    abline(lm(weekly_depression_score ~ daily_screen_time_hours, data = data), col = "red", lwd = 2)
  })
  
  # ---- Regression Analysis ----
  output$regression1 <- renderPrint({
    model <- lm(mental_health_score ~ daily_screen_time_hours, data = data)
    summary(model)
  })
  
  output$regression2 <- renderPrint({
    model <- lm(mental_health_score ~ physical_activity_hours_per_week, data = data)
    summary(model)
  })
  
  output$regression3 <- renderPrint({
    model <- lm(mental_health_score ~ daily_screen_time_hours + 
                  physical_activity_hours_per_week + sleep_duration_hours, data = data)
    summary(model)
  })
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
