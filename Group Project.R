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

#scale the population and gdp values to be in line with the rest of the dataset
df$gdp <- df$gdp / 10000000000


df <- data2[, sapply(data2, function(x) is.numeric(x) || is.factor(x))]


colSums(is.na(df))

#Lastly, we have two values we imputed using linear regression. Both missing values consisted of less than 5% of the total data lest, thus we believed this was an acceptable course of action

#cement co2 imputatoin
impute.cement <- lm(cement_co2 ~ ., data = df, na.action = na.exclude)
missing_row <- is.na(df$cement_co2)
df$cement_co2[missing_row] <- predict(impute.cement, newdata = df[missing_row, ])

#land co2 imputation
impute.land <- lm(land_use_change_co2 ~ ., data = df, na.action = na.exclude)
missing_row <- is.na(df$land_use_change_co2)
df$land_use_change_co2[missing_row] <- predoct(impute.land, newdata = df[missing_row,])


#lastly, we added back country labels for viewing/summarization
df.labaled <- df
df.labeled$country <- country

VIF(df)
View(df)
