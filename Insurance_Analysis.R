# Creating a regression model to determine effect of different variables 
# on the total cost of medical insurance bill 

getwd() # Checking working directory 

# Loading packages 
library(dplyr)
library(ggplot2)
library(effects)


# header = TRUE reads first row as header 
df <-  read.csv('Raw-data/insurance.csv', header = TRUE) 

# summarising data 

mean_charges <- mean(df$charges) # 13270.42 
mean_age <- mean()

summ <- df %>% 
  summarise(mean_age = mean(age),
            mean_child = mean(children),
            mean_charge = mean(charges),
            mean_bmi = mean(bmi),
            std_charge = sd(charges),
            n = length(charges),
            se = std_charge/sqrt(n))


# using unlist() to find all numeric columns 
num_cols <- unlist(lapply(df, is.numeric))

# plotting all numeric columns, takes all rows 
plot(df[,num_cols])

# Making Histograms for each variable  

# Charges 
ggplot(df, aes(x = charges)) +
  geom_histogram(binwidth = 1000, 
                 fill = "skyblue", color = "black") +
  labs(title = "Distribution of Insurance Costs", 
       x = "Insurance Cost")

# Age 
hist_age <- ggplot(df, aes( x = age)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue", color = "black") +
  labs(x = "Age",
       y = "Counts")

# BMI 
hist_bmi <- ggplot(df, aes( x = bmi)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue", color = "black") +
  labs(x = "BMI",
       y = "Counts")

# Children 
hist_child <- ggplot(df, aes( x = children)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue", color = "black") +
  labs(x = "Children",
       y = "Counts")

# Checking correlation b/w different variables 
cor <- round(cor(df[,num_cols]),2) # round() used to specify decimal 

# Converting non numeric variables to factors so I can plot them 
smoker = as.factor(df$smoker)
sex = as.factor(df$sex)
region = as.factor(df$region)

# plotting each factor 
boxplot(df$charges ~ smoker, main = 'smoker') # main = sets title of boxplot
boxplot(df$charges ~ sex, main = 'sex')
boxplot(df$charges ~ region, main = 'region')

# Making a regression model 
model <- lm(charges~., data = df)

summary(model)

# Making table of values from regression model 
coefficients <- coef(summary(model))

# Conclusions 
# R^2 = 0.751 --> 75.1% of variance can explained be by this model 
# Age, BMI, and whether or not one smokes all have a strong impact on 
# insurance costs 

# Using predict() to predict values based on model
predicted <- predict(model)

# Checking correlation of model 
cor.test(df$charges, predicted)

# Plot of observed vs predicted values 
model_plot <- plot(df$charges, predicted,
     xlab = "Observed charges", 
     ylab = "Predicted charges") + 
  abline(a = 0, b = 1, col = "red")

# r2 for predicted and observed values is 0.867 indicating a strong relationship 
# b/w predicted and observed values 



# Using a different approach to make a Linear Regression Model 

# ** Preparing and splitting data **

n_train <- round(0.8 * nrow(df)) # Calculates # of rows for training data set (80%)

# Creating var to generate random sample of rows representing training data 
train_indices <- sample(1:nrow(df), n_train)

# sample() randomly selects n_train row numbers
# selected rows will be used for training dataset 

# To create training data set by selecting rows from randomly generate train indices
Data_train <- df[train_indices, ]

# Create test data using all data excluded from train indices 
Data_test <- df[-train_indices, ]

# In summary, this line of code splits the original dataset, 'df' into two subsets 
# Data_train contains 80% while Data_test contains 20% of the original data not included in the training dataset 
# The training subset is how the model will be formed 
# The test subset will be used to test how well the model performs on unseen data 

# ** Making formula **
# as.formula() to convert string to formula 
form1 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")

# ** Training and testing the model **
model1 <- lm(form1, data = Data_train) # Using training data and formula1 to make initial model 

model1_summ <- summary(model1)

# r-squared of 0.757 
# Saving r-squared
r_squared1 <- model1_summ$adj.r.squared

# Model suggests that BMI, children, and smoking status all influence insurance charges 
# while sex and region have no sig impact 

# Children in other model had no significant impact on insurance charges. Why? 
# Will investigate this more later 

# Testing data 
prediction1 <- predict(model1, newdata = Data_test)

# calculating residuals (diff b/w actual and predicted)
residuals1 <- Data_test$charges - prediction1


# Calculating RMSE to measure how well the model predictions matches actual values 
rmse1 <- sqrt(mean(residuals1^2))


# RMSE = 5965.24 -- lower than std (12110) 

# **Making new model ** 

# Removing sex and region as they have no significant impact on insurance charges 
form2 <- as.formula("charges ~ age + bmi + children + smoker + region")

model2 <- lm(form2, data = Data_train)

model2_summ <- summary(model2)

# Testing R-squared -- 0.757, same as previous model 
r_squared2 <- model2_summ$adj.r.squared 

prediction2 <- predict(model2, newdata = Data_test)

residuals2 <- Data_test$charges - prediction2

rmse2 <- sqrt(mean(residuals2^2)) # Higher RMSE compared to original model 


# **Comparing the models** 

# R-squared: Model 1 - 0.757 | Model 2 - 0.757 
# RMSE: Model 1 - 5965.24 | Model 2 - 6003.91 

# Both models are very similar so I will continue w/ the second one as it is more simple 

# ** Testing Model 2 ** 

# Testing model against existing data - Data_test$prediction creates new column
Data_test$prediction <- predict(model2, newdata = Data_test)

ggplot(Data_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red", linetype = "dashed") +
  ggtitle("Prediction vs. Real values") + 
  theme(panel.background = element_blank()) + 
  geom_vline(xintercept = -4000, color = "black") +
  geom_hline(yintercept = -2000, color = "black") + 
  labs(x = "Predicted values",
       y = "Actual values")

# Testing correlation b/w predicted and actual charges 
cor.test(Data_test$charges, Data_test$prediction)

# R-squared = 0.84, p < 0.005 -- highly significant 


# Testing residuals

Data_test$residuals <- Data_test$charges - Data_test$prediction

# Making point range plot
ggplot(data = Data_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")

# Making histogram
ggplot(Data_test, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "brown", 
                 color = "black") +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0))
  ggtitle("Histogram of residuals")

# Making gain curve plot 
  
install.packages("pROC")
library(prOC)


# Testing model on random made up patients 

# 1. **Jenny**: 19 years old, BMI 27.9, has no children, smokes, from northwest region.

# 2. **Kevin**: 40 years old, BMI 45, 4 children, doesn't smoke, from southeast region.

# 3. **John**: 25 years old. BMI 33.4, no children, doesn't smoke, from northeast region.

Jenny <- data.frame(age = 19,
                  bmi = 27.9,
                  children = 0,
                  smoker = "yes",
                  region = "northwest")

# predicted cost is 25732.56
print(paste0("Health care charges for Jenny: ", round(predict(model2, Jenny), 2)))

Kevin <- data.frame(age = 40,
                    bmi = 45,
                    children = 4,
                    smoker = "no",
                    region = "southeast")
# predicted cost is 14474.33
print(paste0("Health care charges for Kevin: ", round(predict(model2, Kevin), 2)))


John <- data.frame(age = 25,
                    bmi = 33.4,
                    children = 0,
                    smoker = "no",
                    region = "northeast")
# predicted cost is 5645.83
print(paste0("Health care charges for John: ", round(predict(model2, John), 2)))


Poopies <- data.frame(age = 34,
                      bmi = 0.2, 
                      children = 5.5,
                      smoker = "yes",
                      region = "southwest")
print(paste0("Health care charges for Poopies: ", round(predict(model2, Poopies), 2)))

