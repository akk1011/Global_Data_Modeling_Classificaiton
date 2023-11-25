# Load the necessary libraries
library(tidyverse)
library(caret)
library(dplyr)

# Read the dataset
worlddata <- read.csv('/Users/USER/Downloads/archive-3/world_development_data_imputed.csv')


#1 - Creating a Regression Model to how check Life Expectancy at Birth is dependent on GDP, Fertility Rate, Mortality Rate and Population Density
df <- worlddata
colnames(df)
summary(df)

# Checking for NA Values and handling them
df %>%
  summarise_all(~sum(is.na(.)))

#There are no null values - since the data we have is an imputed one from Kaggle

# Select relevant columns
relevant_columns <- c("LifeExpBirth", "GDP", "AdolFertRate", "PopDens", "MortRateU5","FertRate")
df <- df[, relevant_columns]

# Basic EDA
summary(df)
pairsviz <- pairs(~LifeExpBirth + GDP + AdolFertRate + PopDens + MortRateU5, data = df)

#Creating a Regression Model to see how Life Expectancy at Birth is dependent on GDP, Fertility Rate, Mortality Rate and Population Density

# Split the data
set.seed(123)  # for reproducibility
training_indices <- createDataPartition(df$LifeExpBirth, p = 0.8, list = FALSE)
train_data <- df[training_indices, ]
test_data <- df[-training_indices, ]

# Fit a linear regression model
model <- lm(LifeExpBirth ~ ., data = train_data)

# Check the summary
summary(model)

# Make predictions
predictions <- predict(model, newdata = test_data)

# Evaluate the model
postResample(pred = predictions, obs = test_data$LifeExpBirth)

#With a R Squared of 0.8032075, RMSE of 3.925 and MAE of 2.88 - the model does a fairly good job of predicting life expectancy at birth

#CLassfiying Urban Growth - Classification Problem
classifydf <- worlddata
glimpse(classifydf)

#Creating a threshold to define urban population growth
# Ensure you've created classifydf and set the thresholds
high_growth_threshold <- 75  # example threshold
low_growth_threshold <- 25   # example threshold

# Categorize UrbanPopGrowth%
classifydf$UrbanGrowthCategory <- cut(classifydf$UrbanPopGrowth, 
                                      breaks = c(-Inf, 
                                                 quantile(classifydf$UrbanPopGrowth, low_growth_threshold/100), 
                                                 quantile(classifydf$UrbanPopGrowth, high_growth_threshold/100), 
                                                 Inf),
                                      labels = c("Low Urban Growth", "Moderate Urban Growth", "High Urban Growth"),
                                      include.lowest = TRUE)


#Split data into training and testing for calculation
set.seed(123)  # for reproducibility
split_index <- createDataPartition(classifydf$UrbanGrowthCategory, p = 0.8, list = FALSE)
train_data <- classifydf[split_index, ]
test_data <- classifydf[-split_index, ]



# Assuming you have selected relevant features in your train_data
classmodel <- train(UrbanGrowthCategory ~SurfAreaSqKm + PopTotal + PopDens + 
                      PopGrowth. + GDP + GDPGrowth. + NetMigr + MobileSubs.100 +
                      FDINetBoP + GNI.CapAtlas + GNIAtlas + InflConsPric. + UrbanPopGrowth.,
                      data = train_data, method = "rpart")
predictions <- predict(classmodel, test_data)
confusionMatrix(predictions, test_data$UrbanGrowthCategory)

#3 - Classification for GDP Growth
GDP_data <- worlddata
colnames(worlddata)
relevant_columns1 <- c("GDPGrowth.", "GDP", "GNI.CapAtlas", "Imports.GDP", "Exports.GDP", "IndValAdd.GDP","FDINetBoP","AgriValAdd.GDP","GNIAtlas","InflConsPric.")
relevant_columns1
GDP_data <- GDP_data[, relevant_columns1]
# Basic EDA
pairs(~GDPGrowth. + GDP + GNI.CapAtlas + Imports.GDP + Exports.GDP + IndValAdd.GDP, data = GDP_data)

# Define thresholds for categorizing GDPGrowth
negative_growth_threshold <- 0
positive_growth_threshold <- 2  # 

# Create a categorical variable for GDPGrowth%
GDP_data$GrowthCategory <- cut(GDP_data$GDPGrowth., 
                           breaks = c(-Inf, negative_growth_threshold, positive_growth_threshold, Inf),
                           labels = c("Negative Growth", "Stable", "Positive Growth"),
                           include.lowest = TRUE)

#Split data into training and testing data
set.seed(123)  # for reproducibility
split_index <- createDataPartition(GDP_data$GrowthCategory, p = 0.8, list = FALSE)
train_data <- GDP_data[split_index, ]
test_data <- GDP_data[-split_index, ]



# Assuming you have selected relevant features in your train_data
model3 <- train(GrowthCategory ~ ., data = train_data, method = "rpart")
# Make predictions on the testing data
predictions <- predict(model3, newdata = test_data)

# Confusion Matrix and Classification Metrics
confusionMatrix(predictions, test_data$GrowthCategory)



4) #Forecasting India's Population Growth
# Load necessary libraries
library(tidyverse)
library(forecast)
library(tsibble) # for handling time series data
library(ggplot2)
# Forecasting
forecast <- forecast(model, h = 5)  # h is the number of periods to forecast
autoplot(forecast)

# Plotting with ggplot2
ggplot(india_ts, aes(x = Year, y = PopGrowth.)) +
  geom_line() +
  labs(title = "Population Growth in India", x = "Year", y = "Population Growth %")

# Convert tsibble to ts object
india_ts_obj <- ts(india_ts$PopGrowth., start = min(india_ts$Year), end = max(india_ts$Year), frequency = 1)

# Plotting with autoplot from forecast package
autoplot(india_ts_obj)

# Choosing a Model (Example: ARIMA)
model <- auto.arima(india_ts_obj)

# Model Summary
summary(model)

# Forecasting
forecast <- forecast(model, h = 5)  # h is the number of periods to forecast
autoplot(forecast)


         


