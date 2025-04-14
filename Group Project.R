#read csv data
data <- read.csv('visualizing_global_co2_data.csv')


#remove data from before 1982, as it becomes less robust in accuracy and more of extrapolation
data <- data[data$year > 1981,]

#given that we are going to be using gdp or co2 as a response variable,
#it wouldn't be correct to impute missing gdp/co2 values. Luckily, the amount of removed rows isn't very largt
data <- data[!is.na(data$gdp),]
data <- data[!is.na(data$co2),]
data <- data[!is.na(data$co2_growth_prct),]

#this column is a predefined interaction term. There are 60 missing values, so we chose to get rid of these
data <- data[!is.na(data$co2_per_unit_energy),]

#We also decided to remove aggregate data from the dataset, as we are interested in predicting individual countries
data <- data[!(data$country == 'World'),]

colnames(data)[apply(data,2, anyNA)]


#Below, we begin to remove a number of precalculated action terms. Not only are there a lot of missing values in each of these columns,
#but we can also recalculate them alter if we choose to do so
x <- colnames(data)


#this set of interactions is country's share of the global value as a percentage
#this can easily be added back later if necessary, and we chose to remove them
drop <- subset(x, grepl('share', x))

data <- data[, !colnames(data) %in% drop]


#the next interaction is per capita interactions, which again we can calculate later
drop <- subset(x, grepl('per_capita', x))


data <- data[, !colnames(data) %in% drop]

#this list consists of variables we aren't interest in interpreting. Mainly they are other greenhouse gasses and temperature changes
#The only added value is trade co2. Not only is it 50% missing values, removing it would help remove colinearity from the data
drop <- c(
          "nitrous_oxide", 
          "methane", 
          "other_industry_co2", 
          "consumption_co2_per_gdp", 
          "consumption_co2",
          "total_ghg",
          "total_ghg_excluding_lucf",
          "trade_co2"
          )
data <- data[, !colnames(data) %in% drop]

#this interaction is historical cumulative information. This had a lot of missing values in the dataset, and thus we chose to remove them
x <- colnames(data)

drop <- subset(x, grepl('cumulative', x))
data <- data[, !colnames(data) %in% drop]

#these values are summations of two other values, so we decided to remove them as they are colinear
x <- colnames(data)

#This gets rid of co2 data including
drop <- subset(x, grepl('including', x))
data <- data[, !colnames(data) %in% drop]

#lastly, we removed temperature data, as it didn't have much to do with what we wanted to study
x <- colnames(data)

drop <- subset(x, grepl('temperature', x))
data <- data[, !colnames(data) %in% drop]


colnames(data)[apply(data,2, anyNA)]

colSums(is.na(data))

#the below code adds an income level categorical value to each country, as described by world bank
income_level <- matrix(0, nrow = nrow(data), 1)[,1]
data$income_level <- income_level

data2 <- data

low_income_countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
  "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", 
  "Congo", "Congo", "Djibouti", "Egypt", 
  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", 
  "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Cote d'Ivoire", "Kenya", 
  "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", 
  "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", 
  "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", 
  "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", 
  "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe", " China", "India"
)

middle_income_countries <- c(
  "Saudi Arabia", "Russia", "United States", "Iran", "Iraq", 
  "Kuwait", "Venezuela", "Mexico", "Libya", "Nigeria", 
  "United Arab Emirates", "Indonesia", "China", "Canada", 
  "Algeria", "Egypt", "United Kingdom", "Qatar", "Brazil", "Romania", "Poland", "Ukraine",
  "Russia", "Ukraine", "Belarus", "Uzbekistan", "Kazakhstan", "Georgia", "Azerbaijan", 
  "Lithuania", "Moldova", "Latvia", "Kyrgyzstan", "Tajikistan", "Armenia", "Turkmenistan", "Estonia",
  "Argentina", "Brazil", "Chile", "Colombia", "Mexico", "Peru", "Ecuador", "Paraguay", "Uruguay", "Venezuela",
  
  # Asia
  "Thailand", "Philippines", "Sri Lanka", "India", "Pakistan", "Indonesia", "Iran", "Iraq", "Jordan", 
  
  # Africa
  "Egypt", "Algeria", "Morocco", "Tunisia", "Ivory Coast", "Kenya", "Ghana", "Nigeria", "Zimbabwe",
  
  # Eastern Europe / Soviet Bloc
  "Poland", "Hungary", "Czechoslovakia", "Romania", "Bulgaria", "East Germany", "Ukraine", "Belarus",
  
  # Others
  "Turkey", "Yugoslavia"
)

high_income_countries <- c(
  "United States", "Canada", "United Kingdom", "France", "Germany",
  "Italy", "Belgium", "Netherlands", "Luxembourg", "Denmark", "Norway", 
  "Iceland", "Portugal", "Spain", "Greece", "Turkey", "Finland", "Singapore",
  
  # Other Western-aligned developed nations
  "Australia", "New Zealand", "Japan", "Austria", "Switzerland", "Ireland", 
  "Israel", "South Korea", "Taiwan", "Sweden"
)

data2$income_level[data$country %in% low_income_countries] <- 1
data2$income_level[data$country %in% middle_income_countries] <- 2
data2$income_level[data$country %in% high_income_countries] <- 3
data2$income_level[data2$income_level == 0] <- 1
data2$income_level <- as.factor(data2$income_level)

#the below code adds a continent categorical variable

africa <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
  "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros",
  "Democratic Republic of Congo", "Congo", "Djibouti", "Egypt", "Equatorial Guinea",
  "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea",
  "Guinea-Bissau", "Cote d'Ivoire", "Kenya", "Lesotho", "Liberia", "Libya",
  "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco",
  "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe",
  "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan",
  "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe", "Cape Verde"
)

# Asia
asia <- c(
  "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan",
  "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia",
  "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait",
  "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia",
  "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine",
  "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka",
  "Syria", "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", "Turkey",
  "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen", "Hong Kong"
)

# Europe
europe <- c(
  "Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
  "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland",
  "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy",
  "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta",
  "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway",
  "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia",
  "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Czechia",
  "Vatican City"
)

# North America
north_america <- c(
  "Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Canada", "Costa Rica",
  "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", "Guatemala",
  "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama",
  "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
  "Trinidad and Tobago", "United States"
)

# South America
south_america <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
  "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"
)

# Oceania
oceania <- c(
  "Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia",
  "Nauru", "New Zealand", "Palau", "Papua New Guinea", "Samoa",
  "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu"
)

data2$continent <- matrix(0, nrow(data2), 1)[,1]
data2$continent[data2$country %in% africa] <- "Africa"
data2$continent[data2$country %in% asia] <- "Asia"
data2$continent[data2$country %in% europe] <- "Europe"
data2$continent[data2$country %in% north_america] <- "North America"
data2$continent[data2$country %in% south_america] <- "South America"
data2$continent[data2$country %in% oceania] <- "Oceania"

#we set the added variables as factor, so that the model doesn't mistake income level for a continuous variable
data2$continent <- as.factor(data2$continent)
data2$income_level <- as.factor(data2$income_level)

year.1982 <- data2$year - 1982
data2$year <- year.1982

View(data2[data2$continent == 0,])
country <- data2$country



df <- data2[, sapply(data2, function(x) is.numeric(x) || is.factor(x))]


colSums(is.na(df))

#Lastly, we have two values we imputed using linear regression. Both missing values consisted of less than 5% of the total data lest, thus we believed this was an acceptable course of action

#cement co2 imputation
impute.cement <- lm(cement_co2 ~ ., data = df, na.action = na.exclude)
missing_row <- is.na(df$cement_co2)
df$cement_co2[missing_row] <- predict(impute.cement, newdata = df[missing_row, ])

#land co2 imputation
impute.land <- lm(land_use_change_co2 ~ ., data = df, na.action = na.exclude)
missing_row <- is.na(df$land_use_change_co2)
df$land_use_change_co2[missing_row] <- predict(impute.land, newdata = df[missing_row,])

#scale the population and gdp values to be in line with the rest of the dataset
df$gdp <- df$gdp / 10000000000
df$population <- (df$population) / 1000000


#lastly, we added back country labels for viewing/sumarisation
df.labeled <- df
df.labeled$country <- country
View(df)


#install.packages('segmented')
#install.packages('rpart')
#install.packages('rpart.plot')
library(MASS)
library(nlme)
library(segmented)
library(rpart)
library(rpart.plot)

model <- lm(log(gdp) ~ ., data = df)
model.step <- step(model, direction = 'backward')

cooks_distance <- cooks.distance(model.step)
influential <- which(cooks_distance > 1)

model.tree <- rpart(log(gdp) ~., data = df,  method = 'anova')

summary(model.step)
rpart.plot(model.tree)
summary(model.tree)


# ============================================================
# Research Question:
# "How have the relationships between CO₂ emissions and economic indicators 
# changed over time since 1982, and are there identifiable breakpoints that 
# correspond to notable global economic or policy events?"

# Using Time Series Analysis(ARIMAX) with Structural Break Testing
# Temporal Dynamics and Exogenous Influences:
#   The ARIMAX model extends the traditional ARIMA framework by incorporating external regressors. 
#   This allows it to capture both the inherent time series properties of log-transformed GDP and 
#   the simultaneous impact of CO₂ emissions and related variables (such as cement and land use change CO₂ and population), 
#   providing a comprehensive view of how these factors interact over time.
# 
# Structural Change Detection:
#   Integrating structural break analysis into the workflow facilitates the identification of periods where
#   the underlying relationships between the economic indicators and environmental variables may have shifted. 
#   This is crucial for linking potential breakpoints with notable global economic or policy events, 
#   thus offering deeper insights into regime-dependent dynamics.
# 
# Forecasting and Model Evaluation:
#   By splitting the data into training and testing sets and evaluating forecast accuracy within both the overall and segmented datasets, 
#   the model not only explains historical relationships but also validates its predictive performance. 
#   This is important to confirm that the identified shifts are robust and relevant for forecasting future trends.
# 
# In summary, the ARIMAX with structural break testing is used because it effectively handles the complexities of time series data with exogenous drivers and 
# allows for detecting and quantifying changes in relationships over time—a core requirement for your research question.
# ============================================================

# Load required libraries
library(zoo)
library(strucchange)   # for structural break testing
library(ggplot2)
library(forecast)      # for ARIMA forecasting and accuracy metrics

### ------------------------------
### 1. Data Aggregation and Structural Break Testing
# Assume 'df' is your panel dataset with columns: year, gdp, co2, cement_co2, land_use_change_co2, population.
df_yearly <- aggregate(cbind(gdp, co2, cement_co2, land_use_change_co2, population) ~ year,
                       data = df, FUN = mean)
df_yearly$log_gdp <- log(df_yearly$gdp)

cat("Total number of annual observations:", nrow(df_yearly), "\n")
head(df_yearly)

# Fit the aggregated regression model
model_agg <- lm(log_gdp ~ co2 + cement_co2 + land_use_change_co2 + population, data = df_yearly)
summary(model_agg)

# Test for structural breaks
T_obs <- nrow(df_yearly)
h_new <- 0.25  # Adjusted minimum segment size
bp <- breakpoints(log_gdp ~ co2 + cement_co2 + land_use_change_co2 + population,
                  data = df_yearly, h = h_new)
summary(bp)

# Plot for visual inspection of breakpoints
plot(bp, main = "Structural Break Test: Breakpoints in the Regression Model")
plot(df_yearly$year, resid(model_agg),
     main = "Regression Residuals Over Time",
     xlab = "Years", ylab = "Residuals",
     pch = 19, col = "blue")
abline(v = df_yearly$year[bp$breakpoints], col = "red", lwd = 2)

### ------------------------------
### 2. Converting to Time Series Objects & Preparing External Regressors
# Create time series for log(GDP) and the regressors (annual data starting in 1982)
ts_log_gdp <- ts(df_yearly$log_gdp, start = 1982, frequency = 1)
ts_co2 <- ts(df_yearly$co2, start = 1982, frequency = 1)
ts_cement_co2 <- ts(df_yearly$cement_co2, start = 1982, frequency = 1)
ts_land_use_change_co2 <- ts(df_yearly$land_use_change_co2, start = 1982, frequency = 1)
ts_population <- ts(df_yearly$population, start = 1982, frequency = 1)

external_regressors <- cbind(co2 = ts_co2,
                             cement_co2 = ts_cement_co2,
                             land_use_change_co2 = ts_land_use_change_co2,
                             population = ts_population)

### ------------------------------
### 3. Evaluating the Aggregated ARIMAX Model

# For forecast evaluation, split the time series.
# We use data from 1982 to 2013 as the training set and 2014 to the end as the test set.
train <- window(ts_log_gdp, end = 2013)
test  <- window(ts_log_gdp, start = 2014)

train_regressors <- window(external_regressors, end = 2013)
test_regressors  <- window(external_regressors, start = 2014)

# Fit ARIMAX on the training set
model_arimax_train <- auto.arima(train, xreg = train_regressors)
summary(model_arimax_train)

# Forecast for the test period
h_forecast <- length(test)  # forecast horizon equals the number of test observations
forecast_arimax <- forecast(model_arimax_train, xreg = test_regressors, h = h_forecast)

# Plot the forecast vs. actual values
plot(forecast_arimax, main = "ARIMAX Forecast (Training Data: 1982-2013; Test Data: 2014-?)")
lines(test, col = "purple", lwd = 1)

# Evaluate forecast performance using common metrics: RMSE, MAE, MAPE, etc.
accuracy_metrics <- accuracy(forecast_arimax, test)
print("Aggregated ARIMAX Model Forecast Accuracy:")
print(accuracy_metrics)
# This output provides RMSE, MAE, MAPE, and other metrics.

### ------------------------------
### 4. Evaluating Segmented ARIMAX Models (Within Regime)
# Using breakpoints, we segment the data. Here we demonstrate evaluation if each segment is long enough to have its own hold-out sample.
break_indices <- bp$breakpoints
# Convert indices to years
break_years <- df_yearly$year[break_indices]
cat("Break years:", break_years, "\n")

# Define segment boundaries: from 1982 to last observation
segments <- c(1982, break_years, max(df_yearly$year))
cat("Segment boundaries (years):", segments, "\n")

# Lists to store segmentation results
models_segment <- list()
forecasts_segment <- list()
accuracy_segment <- list()

for (i in 1:(length(segments)-1)) {
  seg_start <- segments[i]
  seg_end   <- segments[i+1]
  
  # Subset the data for the current segment
  seg_data <- subset(df_yearly, year >= seg_start & year <= seg_end)
  
  # Check if the segment has at least 8-10 observations to allow split of training and test.
  if(nrow(seg_data) < 8){
    cat("Segment", i, "(", seg_start, "to", seg_end, ") is too short for split evaluation. Skipping forecast evaluation for this segment.\n")
    next
  }
  
  # Split this segment into training (e.g., first 70%) and test (remaining 30%)
  n_seg <- nrow(seg_data)
  train_seg_idx <- 1:floor(0.7 * n_seg)
  test_seg_idx <- (floor(0.7 * n_seg) + 1):n_seg
  
  seg_train <- seg_data[train_seg_idx, ]
  seg_test  <- seg_data[test_seg_idx, ]
  
  # Create time series objects for the segment
  ts_seg_log_gdp <- ts(seg_train$log_gdp, start = seg_train$year[1], frequency = 1)
  ts_seg_co2 <- ts(seg_train$co2, start = seg_train$year[1], frequency = 1)
  ts_seg_cement_co2 <- ts(seg_train$cement_co2, start = seg_train$year[1], frequency = 1)
  ts_seg_land_use_change_co2 <- ts(seg_train$land_use_change_co2, start = seg_train$year[1], frequency = 1)
  ts_seg_population <- ts(seg_train$population, start = seg_train$year[1], frequency = 1)
  
  seg_regressors_train <- cbind(co2 = ts_seg_co2,
                                cement_co2 = ts_seg_cement_co2,
                                land_use_change_co2 = ts_seg_land_use_change_co2,
                                population = ts_seg_population)
  
  # Similarly, create test regressors
  ts_seg_test_log_gdp <- ts(seg_test$log_gdp, start = seg_test$year[1], frequency = 1)
  ts_seg_test_co2 <- ts(seg_test$co2, start = seg_test$year[1], frequency = 1)
  ts_seg_test_cement_co2 <- ts(seg_test$cement_co2, start = seg_test$year[1], frequency = 1)
  ts_seg_test_land_use_change_co2 <- ts(seg_test$land_use_change_co2, start = seg_test$year[1], frequency = 1)
  ts_seg_test_population <- ts(seg_test$population, start = seg_test$year[1], frequency = 1)
  
  seg_regressors_test <- cbind(co2 = ts_seg_test_co2,
                               cement_co2 = ts_seg_test_cement_co2,
                               land_use_change_co2 = ts_seg_test_land_use_change_co2,
                               population = ts_seg_test_population)
  
  # Fit the ARIMAX model for the segment training data
  model_seg <- auto.arima(ts_seg_log_gdp, xreg = seg_regressors_train)
  models_segment[[i]] <- model_seg
  cat("Segment", i, "(", seg_start, "to", seg_end, ") model summary:\n")
  print(summary(model_seg))
  
  # Forecast for the length of the test set in this segment
  h_seg <- nrow(seg_test)
  fc_seg <- forecast(model_seg, xreg = seg_regressors_test, h = h_seg)
  forecasts_segment[[i]] <- fc_seg
  
  # Evaluate forecast performance in this segment
  acc_seg <- accuracy(fc_seg, ts_seg_test_log_gdp)
  accuracy_segment[[i]] <- acc_seg
  
  cat("Segment", i, "(", seg_start, "to", seg_end, ") forecast accuracy:\n")
  print(acc_seg)
  
  # Optional: Plot forecast vs. actual for this segment
  plot(fc_seg, main = paste("ARIMAX Forecast for Segment", i, "(", seg_start, "to", seg_end, ")"))
  lines(ts_seg_test_log_gdp, col = "blue", lwd = 2)
}

# Brief Analysis of the Output:
#
# - Data consists of 37 annual observations. The aggregated regression has an excellent fit (R² ≈ 0.996),
#   with significant positive effects from CO₂ and population.
#
# - Structural break tests detect breakpoints at observations 9, 18, and 27, suggesting shifts in the underlying relationships.
#
# - The aggregated ARIMAX model (an ARIMA(1,0,0) with exogenous predictors) shows strong persistence and good forecast accuracy.
#
# - Segmented ARIMAX models indicate that the effect of CO₂ varies across different regimes,
#   pointing to evolving dynamics over time that may reflect changes in economic or policy contexts.
