# install necessary packages  
install.packages(c("tidyverse", "ggplot2", "corrplot", "Hmisc", "Metrics", "MLmetrics", "lmtest", "car","caret"))
library(tidyverse)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(Metrics)
library(lmtest)
library(car)
library(caret)
library(gridExtra)

# Set working directory
setwd("C:/Users/Admin/Desktop/stat_projects")

# Load dataset
housing_dat <- read.csv("housing_dat.csv", header = TRUE, na.string = c(""), stringsAsFactors = TRUE)

#view the data frame
str(housing_dat)

# look at data summary
summary(housing_dat)

# Create a list of variables you want to plot
numeric_var_plot <- c(
  "Sale_Price", "Lot_Frontage", "Lot_Area", "Year_Built", 
  "Total_Bsmt_SF", "First_Flr_SF", "Second_Flr_SF", 
  "Full_Bath", "Half_Bath", "Bedroom_AbvGr", "Kitchen_AbvGr", 
  "Fireplaces", "Longitude", "Latitude"
)

# Create a list to store the plots
plots <- list()

# Loop through the variables and create plots
for (variable in numeric_var_plot) {
  plot <- ggplot(housing_dat, aes(x = .data[[variable]])) +
    geom_histogram(fill = "blue", bins = 30, color = "black") +
    labs(title = paste("Distribution of", variable), x = variable, y = "Frequency")
  
  plots[[variable]] <- plot
}

# Arrange and plot the plots in a grid with a 3x5 layout
grid.arrange(grobs = plots, ncol = 3, nrow = 5)

# plot for categorical variables(factor variables)
plot1 <- ggplot(housing_dat, aes(x = as.factor(Bldg_Type ))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Bldg_Type", x = "Bldg_Type", y = "Frequency")
plot2 <- ggplot(housing_dat, aes(x = as.factor(House_Style))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of House_Style", x = "House_Style", y = "Frequency")
plot3 <- ggplot(housing_dat, aes(x = as.factor(Overall_Cond ))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Overall_Cond", x = "Overall_Cond", y = "Frequency")
plot4 <- ggplot(housing_dat, aes(x = as.factor(Exter_Cond))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Exter_Cond", x = "Exter_Cond", y = "Frequency")

# Arrange plots in a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Data exploration

#let look at the dimension of dataset.
dim(housing_dat)

# list the types for every columns
sapply(housing_dat, class)


# Check for missing values and display the result
col_missing_counts <- colSums(is.na(housing_dat))
col_missing_counts

# Encode categorical variables
housing_dat <- cbind(housing_dat, model.matrix(~ Bldg_Type - 1, data = housing_dat))
housing_dat <- cbind(housing_dat, model.matrix(~ House_Style - 1, data = housing_dat))
housing_dat <- cbind(housing_dat, model.matrix(~ Overall_Cond - 1, data = housing_dat))
housing_dat <- cbind(housing_dat, model.matrix(~ Exter_Cond - 1, data = housing_dat))

# Remove original categorical columns not needed since it has been encoded
cols_to_remove <- c("Bldg_Type", "House_Style", "Overall_Cond", "Exter_Cond")
housing_dat <- housing_dat[, !colnames(housing_dat) %in% cols_to_remove]

# Check summary after encoding and  categorical columns
summary(housing_dat)

# Check for missing values and display the result agian
col_missing_counts <- colSums(is.na(housing_dat))
col_missing_counts

# check for outliers using boxplot for visualizing the variables
variables <- c("Lot_Frontage", "Lot_Area", "Year_Built", "Total_Bsmt_SF",
               "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath",
               "Bedroom_AbvGr", "Kitchen_AbvGr", "Fireplaces", "Longitude",
               "Latitude", "Sale_Price", "Bldg_TypeOneFam",
               "House_StyleOne_Story", "House_StyleTwo_Story",
               "Overall_CondAbove_Average", "Overall_CondAverage",
               "Exter_CondTypical")

# Set up the layout for the plots
par(mfrow = c(4, 5))

# Create boxplots for each variable
for (variable in variables) {
  boxplot(housing_dat[[variable]], main = paste("Box Plot of", variable))
}

# Reset the layout to the default
par(mfrow = c(1, 1))


# correct outliers 
cols <- c("Lot_Frontage", "Lot_Area", "Year_Built", "Total_Bsmt_SF", "First_Flr_SF", "Second_Flr_SF",
          "Full_Bath", "Half_Bath", "Bedroom_AbvGr", "Kitchen_AbvGr", "Fireplaces", "Longitude", "Latitude",
          "Bldg_TypeDuplex", "Bldg_TypeOneFam", "Bldg_TypeTwnhs", "Bldg_TypeTwnhsE", "Bldg_TypeTwoFmCon",
          "Overall_CondAbove_Average", "Overall_CondAverage", "Overall_CondBelow_Average", "Overall_CondExcellent",
          "Overall_CondFair", "Overall_CondGood", "Overall_CondPoor", "Overall_CondVery_Good",
          "Overall_CondVery_Poor", "Exter_CondExcellent", "Exter_CondFair", "Exter_CondGood", "Exter_CondPoor",
          "Exter_CondTypical")
for (col in cols) {
  Q1 <- quantile(housing_dat[[col]], 0.25)
  Q3 <- quantile(housing_dat[[col]], 0.75)
  IQR <- Q3 - Q1
  k <- 1.5 # IQR multiplier 
  lower_bound <- Q1 - k * IQR
  upper_bound <- Q3 + k * IQR
  housing_dat <- housing_dat[housing_dat[[col]] >= lower_bound & housing_dat[[col]] <= upper_bound, ]
}
summary(housing_dat)

# Remove unnecessary columns (columns without range)  
cols_to_remove <- c("House_StyleOne_and_Half_Fin", "House_StyleOne_and_Half_Unf", "House_StyleSFoyer",
                    "House_StyleSLvl", "House_StyleTwo_and_Half_Fin", "House_StyleTwo_and_Half_Unf",
                    "Overall_CondBelow_Average", "Overall_CondAverage", "Overall_CondExcellent", "Overall_CondFair",
                    "Overall_CondGood", "Overall_CondPoor", "Overall_CondVery_Good", "Overall_CondVery_Poor",
                    "Exter_CondExcellent", "Exter_CondFair", "Exter_CondGood", "Exter_CondPoor", "Exter_CondTypical",
                    "Kitchen_AbvGr", "Bldg_TypeDuplex", "Bldg_TypeOneFam", "Bldg_TypeTwnhs", "Bldg_TypeTwnhsE",
                    "Bldg_TypeTwoFmCon", "Overall_CondAbove_Average", "Exter_CondTypical", "Bldg_TypeDuplex")
housing_dat <- housing_dat[, !colnames(housing_dat) %in% cols_to_remove]

summary(housing_dat)


#  after correcting outliers visualizing the variables
variables <- c("Lot_Frontage", "Lot_Area", "Year_Built", "Total_Bsmt_SF", "First_Flr_SF", "Second_Flr_SF",
               "Full_Bath", "Half_Bath", "Bedroom_AbvGr", "Fireplaces", "Longitude", "Latitude",
               "House_StyleOne_Story", "House_StyleTwo_Story")

# Set up the layout for the plots
par(mfrow = c(4, 5))

# Create boxplots for each variable
for (variable in variables) {
  boxplot(housing_dat[[variable]], main = paste("Box Plot of", variable))
}

# Reset the layout to the default
par(mfrow = c(1, 1))

#Linearity check
plot(housing_dat, col = "lightblue")

# Standardize the selected features( Sale_Price is excluded because it the predicted variable)
features <- c("Lot_Frontage", "Lot_Area", "Year_Built", "Total_Bsmt_SF", "First_Flr_SF", 
              "Second_Flr_SF", "Full_Bath", "Half_Bath","Bedroom_AbvGr", "Fireplaces", "Longitude", "Latitude",
              "House_StyleOne_Story", "House_StyleTwo_Story")

# Min-max normalization function
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Apply min-max scaling function to relevant columns
housing_dat[features] <- lapply(housing_dat[features], min_max_transform)
summary(housing_dat)

# Split data into training and testing sets
set.seed(23138548)  # Set seed for reproducibility
split_data <- sample(nrow(housing_dat), nrow(housing_dat) * 0.8)
train_data <- housing_dat[split_data, ] # training data
test_data <- housing_dat[-split_data, ] # testing data

# fit the multiple linear regression model
model1 <- lm(Sale_Price ~ Lot_Frontage + Lot_Area + Year_Built + Total_Bsmt_SF + First_Flr_SF +
              Second_Flr_SF + Full_Bath + Half_Bath+ Bedroom_AbvGr + Fireplaces +
              Longitude + Latitude + House_StyleOne_Story + House_StyleTwo_Story, train_data)
# Print the summary of the model
summary(model1 )
#Residual standard error: 24820 on 653 degrees of freedom
#Multiple R-squared:  0.8593,	Adjusted R-squared:  0.8563 
#F-statistic: 284.8 on 14 and 653 DF,  p-value: < 2.2e-16

#to see the the relationship b/w variables
par(mfrow=c(2,2))
plot(model1, col = "lightblue")
#
vif(model1) #to check for multicollinearity

#The ncvTest to check for constaint variance
ncvTest(model1)#Non-constant Variance Score Test Variance formula: ~ fitted.values Chisquare = 108.7061, Df = 1, p = < 2.22e-16

#to check for aoutcorrolation dubinwatsonTest (model)
durbinWatsonTest(model1)# lag Autocorrelation D-W Statistic p-value
#                         1      0.01024435      1.975368   0.768
# Alternative hypothesis: rho != 0

# Create a histogram of residuals
par(mfrow=c(1,1))
hist(residuals(model1))



#fit the multiple linear regression model
# drop variables with multicolinearity above 5 Total_Bsmt_SF,First_Flr_SF, Second_Flr_SF, House_StyleTwo_Story
model2 <- lm(Sale_Price ~ Lot_Frontage + Lot_Area + Year_Built + 
               Full_Bath + Half_Bath + Bedroom_AbvGr + Fireplaces +
               Longitude + Latitude + House_StyleOne_Story, train_data) 

#Print the summary of the model2
summary(model2)
#Residual standard error: 39740 on 657 degrees of freedom
#Multiple R-squared:  0.6372,	Adjusted R-squared:  0.6316 
#F-statistic: 115.4 on 10 and 657 DF,  p-value: < 2.2e-16

#to see the the relationship b/w variables
par(mfrow=c(2,2))
plot(model2, col = "lightblue")

vif(model2) #to check for multicollinearity, 

#The ncvTest to check for constaint variance
ncvTest(model2)# Non-constant Variance Score Test Variance formula: ~ fitted.values Chisquare = 115.7961, Df = 1, p = < 2.22e-16

#to check for aoutcorrolation dubinwatsonTest (model2)
durbinWatsonTest(model2)# lag Autocorrelation D-W Statistic p-value
#  lag Autocorrelation D-W Statistic p-value
#    1     -0.03033418      2.057995    0.44
# Alternative hypothesis: rho != 0

# Create a histogram of residuals
par(mfrow=c(1,1))
hist(residuals(model2))


# fit the multiple linear regression model log transform
final_model <- lm(log(Sale_Price) ~ Lot_Frontage + Lot_Area + Year_Built + 
               Full_Bath + Half_Bath + Bedroom_AbvGr + Fireplaces +
               Longitude + Latitude + House_StyleOne_Story, train_data)



#Print the summary of the model3
summary(final_model)
#Residual standard error: 0.1686 on 657 degrees of freedom
#Multiple R-squared:  0.7458,	Adjusted R-squared:  0.742 
#F-statistic: 192.8 on 10 and 657 DF,  p-value: < 2.2e-16 

#to see the the relationship b/w variables
par(mfrow=c(2,2))
plot(final_model, col = "lightblue")

vif(final_model) #to check for multicollinearity

#The ncvTest to check for constaint variance
ncvTest(final_model)#Non-constant Variance Score Test Variance formula: ~ fitted.values Chisquare = 9.451764, Df = 1, p = 0.0021095

#to check for aoutcorrolation dubinwatsonTest (model3)
durbinWatsonTest(final_model)# lag Autocorrelation D-W Statistic p-value
#  lag Autocorrelation D-W Statistic p-value
#   1      -02299099     2.043147     0.574
# Alternative hypothesis: rho != 0

# Create a histogram of residuals
par(mfrow=c(1,1))
hist(residuals(final_model))

test_data$Sale_Price <- log(test_data$Sale_Price)
test_predictions <- predict(final_model, newdata = test_data)

#Assess the R-squared value
rsquared <- summary(final_model)$adj.r.squared  # Use $adj.r.squared for adjusted R-squared

#Print or use the R-squared value as needed
print(rsquared) #0.741954

#Cross-validation
#Define your control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

#Define your model
cvmodel_results <- train(log(Sale_Price) ~ ., data = train_data, method = "lm", trControl = ctrl)

#Print the results
print(cvmodel_results)
#RMSE       Rsquared  MAE       
# 0.1011789  0.9092361  0.0797689


residual_final_model <- residuals(final_model)
ggplot(train_data, aes(x = residual_final_model)) +
  geom_density(fill = "#3498db", color = "#e74c3c", alpha = 0.7) +
  labs(title = "Kernel Density Plot for final_model", x = "Residuals", y = "Density") +
  theme_minimal()

