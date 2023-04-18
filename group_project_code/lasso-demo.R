library(glmnet)
library(dplyr)

# Load the airquality dataset
attach(airquality)
airquality <- airquality %>% tidyr::drop_na()

m1 <- lm(Ozone ~ Solar.R + Wind + Temp + Month + Day, data = airquality)
summary(m1)

# independent & dependent variable
y <- airquality$Ozone

x <- data.matrix(airquality[, c('Solar.R', 'Wind', 'Temp', 'Month', 'Day')])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

############ WHO data
life_exp <- readr::read_csv("group_project_code/Life Expectancy Data.csv") %>% 
  janitor::clean_names() %>% 
  tidyr::drop_na()

m2 <- lm(life_expectancy ~ adult_mortality + infant_deaths + alcohol + hepatitis_b +
           measles + bmi + under_five_deaths + polio + total_expenditure + hiv_aids +
           gdp + population + schooling, data = life_exp)
summary(m2)

# independent & dependent variable
y <- life_exp$life_expectancy

x <- data.matrix(life_exp[, c('adult_mortality','infant_deaths','alcohol','hepatitis_b', 
                              'measles', 'bmi', 'under_five_deaths' , 'polio' , 'total_expenditure' , 
                              'hiv_aids' , 'gdp' , 'population' , 'schooling')])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


