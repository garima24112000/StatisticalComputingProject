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




# ------------------------------------------
# STRUCTURAL BREAK ANALYSIS USING RESAMPLING
# ------------------------------------------

library(boot)

# Use the cleaned data 'df' already processed above
# Fit initial linear model of CO2 over year
co2_model <- lm(co2 ~ year, data = df)

# Estimate segmented model with 1 breakpoint
seg_co2 <- segmented(co2_model, seg.Z = ~ year, npsi = 1, psi = list(year = 18))
summary(seg_co2)

# Plot CO2 emissions with segmented break
plot(df$year, df$co2, main = "CO₂ Emissions with Structural Break", xlab = "Year", ylab = "CO₂ Emissions", pch = 20, col = "gray")
plot(seg_co2, add = TRUE, col = "blue", lwd = 2)

# Extract estimated break year
break_year <- seg_co2$psi[2]

# Restrict data to avoid year edges (e.g., 1987–2017)
df_boot <- df[df$year >= 5 & df$year <= 35, ]

# Bootstrap function to validate break year
boot_break_fn <- function(data, indices) {
  d <- data[indices, ]
  
  mod <- try(lm(co2 ~ year, data = d), silent = TRUE)
  if (inherits(mod, "try-error")) return(NA)
  
  seg <- try(segmented(mod, seg.Z = ~ year, npsi = 1, psi = list(year = 20)), silent = TRUE)
  if (inherits(seg, "try-error")) return(NA)
  
  bp <- try(seg$psi[2], silent = TRUE)
  if (inherits(bp, "try-error") || is.null(bp) || is.na(bp)) return(NA)
  
  return(as.numeric(bp))
}

# Run bootstrap
set.seed(123)
boot_result <- boot(df_boot, statistic = boot_break_fn, R = 1000)

# Filter valid breakpoints (e.g., 6 to 34)
valid_breaks <- boot_result$t[boot_result$t >= 6 & boot_result$t <= 34]

# Histogram of break years
hist(boot_result$t, breaks = 20, main = "Bootstrap Distribution of Break Year", xlab = "Estimated Break Year")
abline(v = break_year, col = "red", lwd = 2)

hist(valid_breaks, breaks = 30,
     main = "Bootstrap Distribution of CO₂ Structural Break Year",
     xlab = "Break Year (relative to 1982)", col = "lightblue", border = "white")
abline(v = mean(valid_breaks, na.rm = TRUE), col = "red", lwd = 2)

# ------------------------------------------
# EVALUATE MODEL PERFORMANCE BEFORE & AFTER
# ------------------------------------------

# Use the filtered average break year from the bootstrap output
final_break_year <- round(mean(valid_breaks, na.rm = TRUE))

# Split data using this break year
before_break <- df[df$year < final_break_year, ]
after_break <- df[df$year >= final_break_year, ]

# Fit separate linear models before and after the break
mod_before <- lm(co2 ~ year, data = before_break)
mod_after <- lm(co2 ~ year, data = after_break)

# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Compute RMSEs
rmse_before <- rmse(before_break$co2, predict(mod_before, before_break))
rmse_after <- rmse(after_break$co2, predict(mod_after, after_break))

summary(mod_before)$r.squared
summary(mod_after)$adj.r.squared

# Report results
cat("Estimated Break Year:", 1982 + final_break_year, "\\n")  # Convert back to actual year
cat("RMSE Before Break:", round(rmse_before, 3), "\\n")
cat("RMSE After Break:", round(rmse_after, 3), "\\n")

# R-squared values
r2_before <- summary(mod_before)$r.squared
r2_after <- summary(mod_after)$r.squared

# Adjusted R-squared values (optional, especially useful if predictors increase)
adj_r2_before <- summary(mod_before)$adj.r.squared
adj_r2_after <- summary(mod_after)$adj.r.squared

# Report R²
cat("R² Before Break:", round(r2_before, 3), "\\n")
cat("R² After Break:", round(r2_after, 3), "\\n")

# Optional: Adjusted R²
cat("Adjusted R² Before Break:", round(adj_r2_before, 3), "\\n")
cat("Adjusted R² After Break:", round(adj_r2_after, 3), "\\n")

# While we identified a statistically significant structural break in CO₂ emissions around 2006 using segmented regression and bootstrapping, 
# the predictive performance of linear models before and after the break was very low (R² ≈ 0). 
# This indicates that year alone is not a sufficient predictor of CO₂ emissions, 
# and the relationship is likely influenced by multiple other socioeconomic and energy-related factors.
