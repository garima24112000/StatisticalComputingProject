install.packages('mice')
library(mice)


data <- read.csv('visualizing_global_co2_data.csv')



data <- data[data$year > 1981,]

data <- data[!is.na(data$gdp),]
data <- data[!is.na(data$co2_per_unit_energy),]
data <- data[!(data$country == 'World'),]

colnames(data)[apply(data,2, anyNA)]

x <- colnames(data)

drop <- subset(x, grepl('share', x))

data <- data[, !colnames(data) %in% drop]
data <- data[!is.na(data$co2),]
data <- data[!is.na(data$co2_growth_prct),]

drop <- subset(x, grepl('per_capita', x))


data <- data[, !colnames(data) %in% drop]


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

x <- colnames(data)

drop <- subset(x, grepl('cumulative', x))
data <- data[, !colnames(data) %in% drop]

x <- colnames(data)

drop <- subset(x, grepl('including', x))
data <- data[, !colnames(data) %in% drop]

x <- colnames(data)

drop <- subset(x, grepl('temperature', x))
data <- data[, !colnames(data) %in% drop]


colnames(data)[apply(data,2, anyNA)]

colSums(is.na(data))


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

data2$continent <- as.factor(data2$continent)
data2$income_level <- as.factor(data2$income_level)

year.1982 <- data2$year - 1982
data2$year <- year.1982

View(data2[data2$continent == 0,])
country <- data2$country

df <- data2[, sapply(data2, function(x) is.numeric(x) || is.factor(x))]


colSums(is.na(df))

impute.cement <- lm(cement_co2 ~ ., data = df, na.action = na.exclude)
missing_row <- is.na(df$cement_co2)
df$cement_co2[missing_row] <- predict(impute.cement, newdata = df[missing_row, ])

impute.land <- lm(land_use_change_co2 ~ ., data = df, na.action = na.exclude)
missing_row <- is.na(df$land_use_change_co2)
df$land_use_change_co2[missing_row] <- predoct(impute.land, newdata = df[missing_row,])

df$country <- country

View(df)
