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

# Justification for Using Time Series Analysis and Structural Break Testing
# 1. Temporal Data Structure:
#    - The dataset comprises annual observations (one per year from 1982 onward), 
#      aggregated from a panel of countries. This creates a clear, time-ordered 
#      series of global averages (e.g., for GDP, CO₂, etc.).
#    - Aggregating the data by year yields a univariate time series suitable for 
#      standard time series analysis, providing a natural ordering that is essential 
#      for exploring how relationships evolve over time.

# 2. Changing Relationships Over Time:
#    - The research question focuses on whether the relationships between key 
#      economic indicators (e.g., GDP, population) and environmental measures (e.g., 
#      CO₂ emissions) have shifted over the past decades.
#    - Time series analysis with structural break testing (using methods like those 
#      implemented in the 'strucchange' package) is well-suited to detect points in time 
#      where the underlying regression relationships change.

# 3. Identification of Regime Shifts:
#    - Structural break tests help identify breakpoints—specific times when the 
#      parameters of the model (e.g., the coefficients linking CO₂ and GDP) shift. 
#    - Detecting such breakpoints is crucial for connecting changes in the statistical 
#      relationships to notable global events, such as the end of the Cold War, the 
#      dot-com bubble, or the global financial crisis.

# 4. Robustness and Interpretability:
#    - The method allows us to quantify the magnitude and timing of changes in 
#      relationships. Once breakpoints are identified, separate models can be estimated 
#      for each time segment to assess how predictor effects vary over different regimes.
#    - This step-by-step approach increases both the robustness of the results and 
#      the clarity with which these changes can be linked to historical economic and 
#      policy events.

# 5. Advantages Given the Dataset:
#    - With 37 annual observations, the aggregated data are sufficiently large to allow 
#      for segmentation without overfitting, while still capturing global trends.
#    - Compared to other methods, time series structural break analysis provides a direct 
#      statistical framework to detect when and how these global relationships have changed.

# In summary, using Time Series Analysis combined with Structural Break Testing is justified 
# for this project because it directly addresses the research question by analyzing the 
# evolution and stability of key economic-environmental relationships over time, and by 
# pinpointing the timing of potential regime shifts that may correspond to major global 
# events. This method provides both robust statistical evidence and a clear narrative 
# framework linking changes in data trends to real-world economic and policy changes.

# Matches the time-ordered nature of your data.
# Directly addresses the issue of changing relationships over time.
# Provides a clear method for identifying shifts in these relationships that may coincide with significant global events.
# Is suitable for the aggregated, global-level trends in your dataset.

# ============================================================

library(zoo)
library(strucchange)  # for breakpoints testing
library(ggplot2)

# STEP 1: Aggregate the data by year
# Since the dataset is a panel of countries, we compute the average for each year.
df_yearly <- aggregate(cbind(gdp, co2, cement_co2, land_use_change_co2, population) ~ year,
                       data = df, FUN = mean)

# For a more linear interpretation of GDP we take its logarithm.
df_yearly$log_gdp <- log(df_yearly$gdp)
head(df_yearly)

# Check number of observations
T_obs <- nrow(df_yearly)
cat("Total number of annual observations:", T_obs, "\n")

# ------------------------------
# STEP 2: Fit the regression model on the aggregated data
# Here we model log(gdp) as a function of CO₂ emissions and other related predictors.
model_agg <- lm(log_gdp ~ co2 + cement_co2 + land_use_change_co2 + population,
                data = df_yearly)
summary(model_agg)

# STEP 3: Determine an appropriate minimum segment size
# The default h is 0.15, but if floor(0.15 * T_obs) is not greater than the number of predictors (4),
# we need to set a higher h.
default_min_seg <- floor(0.15 * T_obs)
cat("Default minimum segment size (floor(0.15 * T_obs)):", default_min_seg, "\n")

h_new <- 0.25
cat("Using a new h value of:", h_new, "\n")
min_seg_new <- floor(h_new * T_obs)
cat("New minimum segment size (floor(h_new*T_obs)):", min_seg_new, "\n")

# ------------------------------
# STEP 4: Perform the structural break test with the adjusted h parameter
bp <- breakpoints(log_gdp ~ co2 + cement_co2 + land_use_change_co2 + population,
                  data = df_yearly, h = h_new)
summary(bp)

# ------------------------------
# STEP 5: Plot the breakpoints
plot(bp, main = "Structural Break Test: Breakpoints in the Regression Model")
# The plot shows the RSS (Residual Sum of Squares) for partitions of the time series along with vertical lines
# indicating where the breaks are estimated.

#Visualize the residuals over time and overlay the estimated breakpoints:
plot(df_yearly$year, resid(model_agg),
     main = "Regression Residuals Over Time",
     xlab = "Years since 1982", ylab = "Residuals",
     pch = 19, col = "blue")
abline(v = df_yearly$year[bp$breakpoints], col = "red", lwd = 2)

# ------------------------------
# STEP 6: Evaluate the Performance of the Aggregated Model

# Predicted values from the aggregated model
predictions <- predict(model_agg, newdata = df_yearly)

# Actual values of log_gdp
actual <- df_yearly$log_gdp

# Calculate RMSE and MAE
rmse <- sqrt(mean((actual - predictions)^2))
mae  <- mean(abs(actual - predictions))

cat("Aggregated Model Performance:\n")
cat("  RMSE:", rmse, "\n")
cat("  MAE :", mae, "\n")

# R-squared and Adjusted R-squared from the model summary:
model_summary <- summary(model_agg)
cat("R-squared:", model_summary$r.squared, "\n")
cat("Adjusted R-squared:", model_summary$adj.r.squared, "\n")

# =============================================
# Structural Break Analysis Results and Implications
# =============================================

# The model was estimated on aggregated annual data (1982–2018) with 37 observations.
# The regression model:
#   log(gdp) = β0 + β1 * co2 + β2 * cement_co2 + β3 * land_use_change_co2 + β4 * population + ε
# 
# Model performance:
#   - R-squared: 0.9956, Adjusted R-squared: 0.9951
#   - RMSE: 0.0236, MAE: 0.0171
#
# These metrics indicate an excellent fit with extremely low residual errors, 
# implying that the selected predictors (especially co2 and population) explain almost all of the variation in log(gdp)
# at the aggregated (global) level.
#
# Structural break analysis was performed using the 'breakpoints' function with an adjusted minimum segment 
# parameter (h = 0.25, yielding 9 observations per segment) to ensure sufficient data in each segment.
#
# The analysis identified three significant breakpoints at the following observation indices (converted to normalized breakdates):
#   - Breakpoint 1: Observation 10 (Normalized ~0.27)
#   - Breakpoint 2: Observation 19 (Normalized ~0.51)
#   - Breakpoint 3: Observation 28 (Normalized ~0.76)
#
# Mapping these indices to calendar years (with year 0 corresponding to 1982):
#   - Observation 10 corresponds roughly to year 9, i.e., 1982 + 9 = ~1991.
#   - Observation 19 corresponds roughly to year 18, i.e., 1982 + 18 = ~2000.
#   - Observation 28 corresponds roughly to year 27, i.e., 1982 + 27 = ~2009.
#
# Therefore, the sample is segmented into four distinct regimes:
#   1. Period 1 (1982–1991)
#   2. Period 2 (1992–2000)
#   3. Period 3 (2001–2009)
#   4. Period 4 (2010–2018)
#
# Implications:
#   - The first breakpoint (~1991) may reflect global changes related to the end of the Cold War
#     and early globalization trends.
#   - The second breakpoint (~2000) could be associated with the tech-boom/dot-com bubble period and the
#     onset of increased global awareness of environmental issues.
#   - The third breakpoint (~2009) likely corresponds to the impact of the global financial crisis,
#     which significantly altered economic dynamics and energy consumption patterns.
#
# These structural breaks indicate that the relationship between global CO2 emissions, population,
# and economic output (log(GDP)) has not been stable over time. Instead, it exhibits shifts
# corresponding to major economic and policy events. 
#
# The aggregated model is highly robust at the global level; however, these regime shifts highlight the importance
# of accounting for time-specific changes in the economic-environmental nexus when making policy decisions or forecasting future trends.
