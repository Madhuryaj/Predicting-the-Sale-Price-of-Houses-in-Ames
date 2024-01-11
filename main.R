# Predicting house prices using regression on AMES IOWA Dataset analysis using R studio #

getwd()
setwd("/Users/madhuryaj/Desktop/STATS_ASSIGNMENT/")
getwd()

install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(dplyr)
library(ggplot2)
library(caret)

## Import ames data into R studio
ames_house_data <- read_excel("ames.xlsx")

###### Produce appropriate descriptive statistics for the dataset. ######
# Print the number of rows and columns
dimensions <- dim(ames_house_data)
print(paste("Number of rows:", dimensions[1]))
print(paste("Number of columns:", dimensions[2]))

# Check for duplicates based on all columns
duplicate_rows <- ames_house_data[duplicated(ames_house_data),]
# Count the number of duplicate rows
num_duplicates <- sum(duplicated(ames_house_data))
# Print duplicate rows and number of duplicates
print(duplicate_rows)
print(num_duplicates)


#descriptive stats of the structure of dataset
str(ames_house_data)

# Summary statistics for numerical variables
summary(ames_house_data)
# 'sale_price' is the target variable, so lets find the Central Tendenc like Mean, median,
# and standard deviation it
mean(ames_house_data$sale_price)
median(ames_house_data$sale_price)

# Minimum and maximum for 'sale_price'
min(ames_house_data$sale_price)
max(ames_house_data$sale_price)

#Dispersion metric like standard deviation & Quantiles
sd(ames_house_data$sale_price)
quantile(ames_house_data$sale_price)

#### Data Quality Issues - For simplicity the DQ issues are identified and solved for the
#attributes used during this analysis ####

# Outliers - 1
# Create a boxplot to visualize outliers the target field 'sale_price'
boxplot(ames_house_data$sale_price, main = "Sale Prices for Ames Housing Data with
outliers")
#Since we find a few outliers >800000 we can drop them
ames_house_data <-
  ames_house_data[ames_house_data$sale_price <= 800000, ]
boxplot(ames_house_data$sale_price, main = "Sale Prices for Ames Housing Data without outliers")

# Outliers - 2
# Create a boxplot to visualize outliers the input field 'lot_area'
boxplot(ames_house_data$lot_area, main = "Lot area with outlier")
#Since we find a outlier which is less than 1000 sqft - this is dropped
ames_house_data <-
  ames_house_data[ames_house_data$lot_area > 1000, ]
boxplot(ames_house_data$lot_area, main = "Lot area without outlier")

# Missing Values
colSums(is.na(ames_house_data))
# Missing Values - veneer_area - There are 22 missing values
summary(ames_house_data$veneer_area)
# Histogram to understand the distribution of lot areas
hist(ames_house_data$veneer_area,
     breaks = 20,
     main = "Histogram of verneer_area")

# We can substitute the NA with value 0 which means there veneer_area is zero
ames_house_data$veneer_area[is.na(ames_house_data$veneer_area)] <- 0
sum(is.na(ames_house_data$veneer_area))

# Missing Values - 'bsmt_area'
# Drop rows with NA values in 'bsmt_area' - 1 Row dropped
ames_house_data <-
  ames_house_data[complete.cases(ames_house_data$bsmt_area), ]

#check missing values for hypothesis variables and few others that will be used in the model analysis
sum(is.na(ames_house_data$total_sf_living))
sum(is.na(ames_house_data$lot_area))
sum(is.na(ames_house_data$house_condition))
sum(is.na(ames_house_data$year_built))
sum(is.na(ames_house_data$bsmt_area))
sum(is.na(ames_house_data$kitchen))
sum(is.na(ames_house_data$bedroom))
#house_quality = 11 in one row, which is updated to 10
ames_house_data$house_quality[ames_house_data$house_quality == 11] <-
  

### Data Transformations ###
# Change the catogorical attributes to factors for analysis
ames_house_data$house_quality <-
  as.factor(ames_house_data$house_quality)
ames_house_data$house_condition <-
  as.factor(ames_house_data$house_condition)
# Feature Engineering to form a new column from existing column
ames_house_data$total_sf_living <-
  ames_house_data$floor1_sf + ames_house_data$floor2_sf + ames_house_data$low_qual_sf


### Data Visualizations for the hypothesis variables using ggplot2 package ###

# Hypothesis 1 - Lot Area vs Sale Price using ggplot
ggplot(ames_house_data, aes(x = lot_area, y = sale_price)) +
  geom_point(color = "pink") +
  labs(
    title = "Lot Area vs Sale Price",
    x = "Lot Area (sq_ft)",
    y = "Sale Price ($)",
    caption = "Data Source: Ames Iowa Dataset"
  ) +
  
  theme_minimal()

# Hypothesis 2 - Sale Price vs Year Built
ggplot(ames_house_data, aes(x = year_built, y = sale_price)) +
  geom_line(color = "blue") +
  labs(
    title = "Sale Price over Years",
    x = "Year Built",
    y = "Sale Price ($)",
    caption = "Data Source: Ames Iowa Dataset"
  ) +
  theme_minimal()

# Hypothesis 3 - Veneer Area
ggplot(ames_house_data, aes(x = veneer_area, y = sale_price)) +
  geom_point(color = "skyblue") +
  labs(
    title = "Distribution of Veneer Area",
    x = "Veneer Area (sq_ft)",
    y = "Sale Price ($)",
    caption = "Data Source: Ames Iowa Dataset"
  ) +
  theme_minimal()

# Hypothesis 4 - Basement Area
ggplot(ames_house_data, aes(x = bsmt_area, y = sale_price)) +
  geom_point(color = "orange") +
  labs(title = "Basement Area vs Sale Price",
       x = "Basement Area",
       y = "Sale Price") +
  theme_minimal()

# Hypothesis 5 - House Quality
ggplot(ames_house_data, aes(x = house_quality, y = sale_price)) +
  geom_boxplot(color = "darkgreen") +
  labs(
    title = "Sale Price by House Quality",
    x = "House Quality",
    y = "Sale Price ($)",
    caption = "Data Source: Ames House Data"
  ) +
  theme_minimal()


### Measures of association ### 

#Hypothesis 1
cor(ames_house_data$lot_area,
    ames_house_data$sale_price,
    method = "pearson")

#Hypothesis 2
cor(ames_house_data$year_built,
    ames_house_data$sale_price,
    method = "pearson")

#Hypothesis 3
cor(ames_house_data$veneer_area,
    ames_house_data$sale_price,
    method = "pearson")

#Hypothesis 4
cor(ames_house_data$bsmt_area,
    ames_house_data$sale_price,
    method = "pearson")

#Hypothesis 5
cor(as.numeric(ames_house_data$house_quality),
    ames_house_data$sale_price,
    method = "spearman")


#Split the data into a train and test set. You must use your student number to set the

# Student number is used as the seed
set.seed(XXX)

# Create a 80-20 train-test split
indices <-
  createDataPartition(ames_house_data$sale_price, p = 0.8, list = FALSE)
train_data <- ames_house_data[indices, ]
test_data <- ames_house_data[-indices, ]


#Multiple Linear Regression - model_1
multiple_linear_regression_model_1 <-
  lm(sale_price ~ lot_area + year_built + veneer_area + bsmt_area + house_quality,
     data = train_data)
summary(multiple_linear_regression_model_1)
multiple_linear_regression_model_1_prediction <-
  predict(multiple_linear_regression_model_1, newdata = test_data)
postResample(multiple_linear_regression_model_1_prediction,
             test_data$sale_price)

#Multiple Linear Regression - model_2
multiple_linear_regression_model_2 <-
  lm(
    sale_price ~ lot_area + year_built + veneer_area + bsmt_area +
      house_quality + total_sf_living + bedroom + kitchen + house_condition,
    data = train_data
  )
summary(multiple_linear_regression_model_2)
multiple_linear_regression_model_2_prediction <-
  predict(multiple_linear_regression_model_2, newdata = test_data)
postResample(multiple_linear_regression_model_2_prediction,
             test_data$sale_price)

#Multiple Linear Regression - model_3
multiple_linear_regression_model_3 <-
  lm(
    sale_price ~ lot_area + total_sf_living + year_built + veneer_area + bsmt_area +
      house_quality + kitchen + building + house_condition + garage_cars +
      garage_area + garage_qual + bedroom,
    data = train_data
  )
summary(multiple_linear_regression_model_3)
multiple_linear_regression_model_3_prediction <-
  predict(multiple_linear_regression_model_3, newdata = test_data)
postResample(multiple_linear_regression_model_3_prediction,
             test_data$sale_price)

#Multiple Linear Regression- model_4
multiple_linear_regression_model_4 <-
  lm(
    sale_price ~ lot_area + total_sf_living + year_built + veneer_area + bsmt_area +
      house_quality + bedroom + kitchen + house_condition +
      building + sale_cond + bsmt_unf,
    data = train_data
  )
summary(multiple_linear_regression_model_4)
multiple_linear_regression_model_4_prediction <-
  predict(multiple_linear_regression_model_4, newdata = test_data)
postResample(multiple_linear_regression_model_4_prediction,
             test_data$sale_price)


#for model 4 which is best assumption checks are made - multiple_linear_regression_model_4

#the following code demonstrates how to check these assumptions

# Checking for multicollinearity - check if any VIF > 3
library(car)
vif(multiple_linear_regression_model_4)


# Homoscedasticity - check if the residual plot looks like a random spread of points
plot(multiple_linear_regression_model_4)


# Normally distributed residuals - check Q-Q residual plot - if normal the points should be on
#the dotted line
plot(multiple_linear_regression_model_4) # the second plot shows residuals against the normal
distribution

# Independent residuals - run the durban watson test: values between 1.5 and 2.5 is ok
library(lmtest)
dwtest(multiple_linear_regression_model_4)

# Influential cases - check cooks distance: >1 indicates a point is exerting too much influence on
#the regression line

# Calculate Cook's distance
cooks_dist <- cooks.distance(multiple_linear_regression_model_4)

# Check for observations with Cook's distance greater than 1
sum(cooks_dist > 1)
