####### CCAM R WORKSHOP - OCTOBER 5, 2018 #######
################ R CODE - PART 2 ################

#### IMPORTANT NOTE: YOU NEED THE VARIABLES CREATED FROM PART 1 IN YOUR ENVIRONMENT
#### TO RUN THIS AFTER THE WORKSHOP, LOAD THE ENVIRONMENT YOU SAVED AFTER PART 1
#### TO LOAD, CLICK THE ENVIRONMENT TAB, THEN THE OPEN BUTTON

# Slide 2 - creating a simple linear regression model
lm_sqft <- lm(PRICE ~ SQUARE_FEET, data = housing.training)

# Slide 3 - creating a multiple linear regression model
lm_sqft_lotsize <- lm(PRICE ~ SQUARE_FEET + LOT_SIZE, data = housing.training)

# Slide 4 - evaluating the fit of a linear model
broom::glance(lm_sqft_lotsize) # to directly call a function in a package we haven't loaded, the syntax is package_name::function_name


# Slide 5 - comparing the fit of multiple linear models

linear_models <- list(lm1 = lm_sqft, lm2 = lm_sqft_lotsize)  # a list of our models... 
model_names <- tibble(model = c("lm_sqft", "lm_sqft_lotsize"))  # ...and a tibble storing their names

# apply the glance function to every model in our list
# technically purrr is loaded when we load the tidyverse, so we don't need the purrr:: part
fits <- map_df(linear_models, broom::glance)

cbind(model_names, fits) # combine the two and print output to Console

# Slide 7 - validation set
# library (caret) # run this line only if you closed out of R and need to reload the caret package
set.seed(1005)
rows.training2 <- createDataPartition(
  y = housing.training$PROPERTY_TYPE,  #categorical variable we want to split by
  p = .8,  # 80% of each property type in the training set, 20% in validation
  list = FALSE
)

housing.training2 <- housing.training[rows.training2,]

housing.validation <- housing.training[-rows.training2,]


# Slides 8-9 - fitting and predicting with a validation set
lm_sqft2 <- lm(PRICE ~ SQUARE_FEET, data = housing.training2)

# library(tidyverse) # run this line only if you closed out of R and need to reload the tidyverse packages
predicted_lm_sqft2 <- housing.validation %>% modelr::add_predictions(lm_sqft2)
head(predicted_lm_sqft2[6:12])  # only columns 6-12, to save space in the output

# Slides 10-12 - evaluating a linear model with a validation set
squared_residuals_lm_sqft2 <- (predicted_lm_sqft2$PRICE - predicted_lm_sqft2$pred)^2
rmse_lm_sqft2 <- sqrt(mean(squared_residuals_lm_sqft2))  # RMSE
print(rmse_lm_sqft2)

abs_residuals_lm_sqft2 <- abs(predicted_lm_sqft2$PRICE - predicted_lm_sqft2$pred)
mae_lm_sqft2 <- mean(abs_residuals_lm_sqft2)  # MAE
print(mae_lm_sqft2)

# rmse in modelr package computes RMSE
modelr::rmse(lm_sqft2, data = housing.validation)
# mae in modelr package computes MAE
modelr::mae(lm_sqft2, data = housing.validation)


# Slides 14-15 - comparing predictive accuracy of multiple linear regression models
lm_sqft_lotsize2 <- lm(PRICE ~ SQUARE_FEET + LOT_SIZE, data = housing.training2)

linear_models_validation <- list(lm1 = lm_sqft2, lm2 = lm_sqft_lotsize2)
model_names_validation <- c("lm_sqft", "lm_sqft_lotsize")

lm_rmse <- purrr::map_dbl(linear_models_validation, modelr::rmse, data = housing.validation)
lm_mae <- purrr::map_dbl(linear_models_validation, modelr::mae, data = housing.validation)

tibble(model = model_names_validation, rmse = lm_rmse, mae = lm_mae) # lower prediction errors are better

# Slide 17 - preparing data for a binary classification model
glm_training_tbl <- housing.training %>% 
  mutate(TYPE = ifelse(PROPERTY_TYPE == "Single Family Residential", 1, 0)) %>% 
  mutate(TYPE = as.factor(TYPE))

# Slide 18 - logistic regression models
glm_sqft <- glm(TYPE ~ SQUARE_FEET, data = glm_training_tbl, family = "binomial")

# Slide 19 - errors in logistic regression models
glm_lot_size <- glm(TYPE ~ LOT_SIZE, data = glm_training_tbl, family = "binomial")


# Slide 23 - evaluating fit of logistic regression models
broom::glance(glm_sqft)

# Slides 24-28 - evaluating predictive accuracy of logistic regression models
# first - create training and validation sets for glm out of our original training and validation sets
glm_training2_tbl <- housing.training2 %>% 
  mutate(TYPE = ifelse(PROPERTY_TYPE == "Single Family Residential", 1, 0)) %>% 
  mutate(TYPE = as.factor(TYPE))

glm_validation_tbl <- housing.validation %>% 
  mutate(TYPE = ifelse(PROPERTY_TYPE == "Single Family Residential", 1, 0)) %>% 
  mutate(TYPE = as.factor(TYPE))

# then - fit the model on the new training set
glm_sqft2 <- glm(TYPE ~ SQUARE_FEET, data = glm_training2_tbl, family = "binomial")

# try adding the predictions directly, it won't work the way you think it will
predicted_glm_sqft2 <- glm_validation_tbl %>% modelr::add_predictions(glm_sqft2)
head(predicted_glm_sqft2$pred)

# type = "response" tells R that we want the probabilities of being in class 1
predicted_probs <- predict(glm_sqft2, newdata = glm_validation_tbl, type = "response")

# now we can add predicted probabilities instead
predicted_glm_sqft2 <- glm_validation_tbl %>% mutate(pred = predicted_probs)
head(predicted_glm_sqft2$pred)

# Slides 29-30 - misclassification rate
p.cutoff <- 0.5
predicted_class <- factor(ifelse(predicted_glm_sqft2$pred > p.cutoff, 1, 0))  # do the actual classification
predicted_glm_sqft2 <- predicted_glm_sqft2 %>% mutate(class = predicted_class)  # not strictly necessary, but makes printing nicer

original_class <- glm_validation_tbl$TYPE

# Misclassification rate
mean(original_class != predicted_class)

# Slides 31-34 - additional evaluation metrics for classifiers
caret::confusionMatrix(data = predicted_class, reference = original_class, positive = "1")

glm_evaluation <- caret::confusionMatrix(data = predicted_class, reference = original_class, positive = "1")
glm_evaluation$byClass[1:7]  # seven different useful evaluation metrics

p.cutoff2 <- 0.75 # change the cutoff value
predicted_class2 <- factor(ifelse(predicted_glm_sqft2$pred > p.cutoff2, 1, 0))
glm_evaluation2 <- caret::confusionMatrix(data = predicted_class2, reference = original_class, positive = "1")
glm_evaluation2$table  # just the confusion matrix 
glm_evaluation2$byClass[1:7]  # sensitivity, etc.

# Slide 38 - creating folds for cross-validation
set.seed(1005)
five.folds <- caret::createFolds(
  y = housing.training$PROPERTY_TYPE,  # categorical variable we want to split by
  k = 5  # five folds
)

# Slides 39-42 - cross-validation
## cross-validation example for linear model using RMSE as our metric
rmse_cv <- numeric(5)  # five 0's
for (i in 1:5){
  validation.rows <- five.folds[[i]]
  train.data <- housing.training[-validation.rows, ]
  validation.data <- housing.training[validation.rows, ]
  lm.cv <- lm(PRICE ~ SQUARE_FEET, data = train.data)
  rmse_cv[i] <- modelr::rmse(lm.cv, data = validation.data)
}
rmse_cv

overall_rmse_sqft <- sqrt(mean(rmse_cv^2))  # estimate of overall RMSE
overall_rmse_sqft

## cross-validation example for logistic regression model using misclassification rate as our metric
misclassification_cv_glm_sqft <- numeric(5)
for (i in 1:5){
  validation.rows <- five.folds[[i]]
  train.data <- glm_training_tbl[-validation.rows, ]
  validation.data <- glm_training_tbl[validation.rows, ]
  original.class.cv <- validation.data$TYPE
  glm.cv <- glm(TYPE ~ SQUARE_FEET, data = train.data, family = "binomial")
  glm.cv.predicted <- predict(glm.cv, newdata = validation.data, type = "response")
  predicted.class.cv <- factor(ifelse(glm.cv.predicted > 0.5, 1, 0))
  misclassification_cv_glm_sqft[i] <- mean(original.class.cv != predicted.class.cv)
}

misclassification_cv_glm_sqft

mean(misclassification_cv_glm_sqft)  # estimate of overall misclassification rate

misclassification_cv_glm_sqft <- numeric(5)
for (i in 1:5){
  validation.rows <- five.folds[[i]]
  train.data <- glm_training_tbl[-validation.rows, ]
  validation.data <- glm_training_tbl[validation.rows, ]
  original.class.cv <- validation.data$TYPE
  glm.cv <- glm(TYPE ~ SQUARE_FEET+ BEDS, data = train.data, family = "binomial")
  glm.cv.predicted <- predict(glm.cv, newdata = validation.data, type = "response")
  predicted.class.cv <- factor(ifelse(glm.cv.predicted > 0.5, 1, 0))
  misclassification_cv_glm_sqft[i] <- mean(original.class.cv != predicted.class.cv)
}

misclassification_cv_glm_sqft

mean(misclassification_cv_glm_sqft)  # estimate of overall misclassification rate



##### my experiment
# linear models
lm_beds <- lm(PRICE ~ BEDS, data = housing.training)
lm_beds_baths <- lm(PRICE ~ BEDS + BATHS, data = housing.training)
broom::glance(lm_beds_baths)
linear_models2 <- list(lm1 = lm_beds, lm2 = lm_beds_baths)  # a list of our models... 
model_names2 <- tibble(model = c("lm_beds", "lm_beds_baths"))
fits2 <- map_df(linear_models2, broom::glance)
cbind(model_names2, fits2)

lm_beds_baths_sqft <- lm(PRICE ~ BEDS + BATHS + SQUARE_FEET, data = housing.training)
broom::glance(lm_beds_baths_sqft)
linear_models3 <- list(lm1 = lm_beds, lm2 = lm_beds_baths, lm3=lm_sqft, lm4=lm_beds_baths_sqft)  # a list of our models... 
model_names3 <- tibble(model = c("lm_beds", "lm_beds_baths", "lm_sqft", "lm_beds_baths_sqft"))
fits3 <- map_df(linear_models3, broom::glance)
cbind(model_names3, fits3)

