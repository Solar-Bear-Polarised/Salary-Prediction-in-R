library(tidyr)
library(dplyr)
library(ggplot2)
library(corrgram)
library(gridExtra)
library(caret)

 Dataset
View(Dataset) 
# converting string variables to Binary
# Although it will make no sense to change these String variables to Numeric form as Regression
# won't work with it, we have to perform Annova, just doing to find if there are any relation between them.

Dataset$Sex <- Dataset$Sex <- ifelse(Dataset$Sex=="Male",0,1)
View(Dataset) 

Dataset$Employer<- recode(Dataset$Employer,"State-gov"=1,"Private"=2,"Self-emp-not-inc"=3,"Federal-gov"=4,"Local-gov"=5,)
View(Dataset)

lapply(Dataset, summary)

# Statistical values of Age, Education Years, Hours per week are factors which we will predict the salary upon using Regression function


# finding correalation between factors
cor1<-cor(Dataset$Salary, Dataset$Age)
cor1
cor2<-cor(Dataset$Salary, Dataset$EducationYrs)
cor2
cor3<-cor(Dataset$Salary, Dataset$HoursPerWeek)
cor3


Dataset$Sex <- Dataset$Sex <- ifelse(Dataset$Sex==1,"Male","Female")
View(Dataset)

# Multiple Linear Regression Model

Multiple_Regression <- lm(Salary~Age+EducationYrs+HoursPerWeek, data=Dataset)
summary(Multiple_Regression)
plot(Multiple_Regression)

# using log to make inference more practical

Multiple_RegressionL <- lm(log(Salary)~log(Age)+log(EducationYrs)+log(HoursPerWeek), data=Dataset)
summary(Multiple_RegressionL)
plot(Multiple_RegressionL)
# taking log did not provide concrete differences between values achieved earlier so proceeding without log


library(dplyr)
library(ggplot2)
library(lmtest)

# Load the necessary library
library(corrplot)

# Select the columns for the independent variables
ind_vars <- Dataset[, c("Age", "EducationYrs", "HoursPerWeek")]

# Calculate the correlation matrix
corr_matrix <- cor(ind_vars)

# Visualize the correlation matrix
corrplot(corr_matrix, type = "upper", method = "circle", diag = FALSE)

#Variance inflation factor (VIF) is used to detect the severity of multicollinearity in the ordinary least square (OLS) regression analysis.

vif(Multiple_Regression)
vif
# As all values are below 5 ( sometimes 4), There is no Multicollinearity problem in this regression
residuals <- residuals(Multiple_Regression)

# Plot the residuals versus the fitted values
ggplot(data = data.frame(residuals, fitted(Multiple_Regression)), aes(x = fitted(Multiple_Regression), y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Residuals vs Fitted Values")


plot(Multiple_Regression)

#Heteroskedasticity Check

bp_test<- bptest(Multiple_Regression)
bp_test
# P value is less than 0.05, reject null hypothesis, Heteroskedasticity available

bp_testL<- bptest(Multiple_RegressionL)
bp_testL
# again Heteroskedasticity present

# Visualize the residuals
ggplot(Dataset, aes(x = fitted(Multiple_Regression), y = residuals(Multiple_Regression))) +
  geom_point() +
  labs(x = "Fitted values", y = "Residuals")

# Predict salaries for new data without Heteroscedasticity removal

pred <- predict(Multiple_Regression, data = Dataset)

# Print the predicted salaries
pred
# Correct for heteroskedasticity using HC standard errors
library(sandwich)
library(lmtest)
# Correct for heteroskedasticity using HC standard errors
model_HC <- coeftest(Multiple_Regression, vcov = sandwich)

# Print the corrected coefficients and standard errors
summary(model_HC)

# Autocorrelation

# Load the necessary libraries
library(car)
library(lmtest)



# Check for autocorrelation using the Durbin-Watson test
dw_test <- durbinWatsonTest(Multiple_Regression)


# Print the test result
dw_test

# Correct for autocorrelation using Cochrane-Orcutt regression
co_model <- cochrane.orcutt(Multiple_Regression)

# Print the corrected coefficients and standard errors
summary(co_model)

# As there was no significant autocorrelation, the earlier model is being used

# Model Misspecification

# Check for model misspecification using the influence.measures() function
influence <- influence.measures(Multiple_Regression)


# Print the influence measures
influence

# Correct for model misspecification by adding a squared term for Age
model_corrected <- lm(Salary ~ Age + I(Age^2) + HoursPerWeek + EducationYrs, data = Dataset)

# Print the corrected coefficients and standard errors
summary(model_corrected)

# Final Prediction

Finalpred <- predict(model_corrected, data = Dataset)
Finalpred


