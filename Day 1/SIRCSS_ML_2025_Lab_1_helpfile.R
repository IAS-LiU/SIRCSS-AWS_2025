# --------------------------------------------------------
# R Helpfile
# Lab 1, SIRCSS AWS 2025, 2025-12-01
# --------------------------------------------------------

# --------------------------------------------------------
# Load packages
# --------------------------------------------------------

# For any package that you may not already have installed,
# use the function install.packages("package-name") to install it.
library(data.table)
library(glmnet)
library(ggplot2)
library(mlbench)
library(caret)
library(randomForest)
library(rpart.plot)
library(pdp)
# --------------------------------------------------------

# --------------------------------------------------------
# High-dimensional data example
# --------------------------------------------------------

# Objective: predict sentiment based on language used in IMDB reviews

# Import (pre-processed) IMDB data
imdb <- fread(file = 'imdb_data.csv')

# Inspect
imdb[1:10, 1:10]
dim(imdb) # High-dimensional? Yes.

# Fit ridge regression
# Assess a range of lambda values through (10-fold) cross-validation
X <- imdb[, -c('id', 'sentiment'), with = F] # Exclude response and id columns.
X <- as.matrix(X) # Make input data into a matrix
X <- scale(X) # Standardize data
cvglmnet <- cv.glmnet(
  x = X,
  y = imdb$sentiment,
  nfolds = 5, # Number CV-folds
  family = 'binomial', # Outcome binary --> logit/binomial
  alpha = 0, # alpha=0 --> ridge
  type.measure = 'class'
) # measure performance in terms of accuracy

# Which lambda gave the best results?
cvglmnet # Accuracy ~ 80%
plot(cvglmnet, sign.lambda = 1)

# From plot:
# i) As we increase the penalty, performance worsens. Why? Increase bias.
# ii) Note log-scale of lambda

# Why does it not "go up" to the left?
# cv.glmnet uses an efficient heuristic for selecting grid to search over
# let's consider a very small lambda
cvglmnet$lambda # This is the grid we considered
mylambda_grid <- c(10^-10, cvglmnet$lambda.min)
cvglmnet_extra <- cv.glmnet(
  x = X,
  y = imdb$sentiment,
  nfolds = 5,
  family = 'binomial',
  alpha = 0,
  type.measure = 'class',
  lambda = mylambda_grid
)
plot(cvglmnet_extra, sign.lambda = 1)
# Here we can see that, indeed, as we make the penalty "too small" and performance worsens
# This time, because of variance.

# Let's now extract the coefficients that correspond to the lambda which gave the lowest error
best_coefs <- coef(cvglmnet, s = "lambda.min")
best_coefs_dt <- data.table(word = rownames(best_coefs), coef = best_coefs[, 1])

# If we didnt standardize X before running cv.glmnet(); instead used standardize=TRUE in cv.glmnet()
# X_sd <- apply(X, 2, sd)
# X_sd <- data.table(var=names(X_sd),sd=X_sd)
# best_coefs_dt <- merge(x = best_coefs_dt,
#                        y = X_sd,
#                        by.x = 'word',
#                        by.y = 'var')
# best_coefs_dt[,coef := coef*sd]

best_coefs_dt[order(coef, decreasing = T)]

# The words that are most predictive of positive sentiment are words like:
# "great", "best", "excellent"

# For negative, on the other hand, we find: "bad", "worst", and "boring".

# --------------------------------------------------------
# Non-linear example
# --------------------------------------------------------

data("Ozone")
Ozone_dt <- as.data.table(Ozone)
names(Ozone_dt) <- c(
  "Month",
  "Date",
  "Day",
  "Ozone",
  "Press_height",
  "Wind",
  "Humid",
  "Temp_Sand",
  "Temp_Monte",
  "Inv_height",
  "Press_grad",
  "Inv_temp",
  "Visib"
)
Ozone_dt <- Ozone_dt[complete.cases(Ozone_dt)]
Ozone_dt <- Ozone_dt[, -c('Month', 'Date', 'Day'), with = F] # Exclude date variables

# Objective: predict air pollution ("Ozone") based on measurements like
#            temperature, wind, etc. (LA, 1976 data)

# Data contains non-linearities:
# > plotting pollution ("Ozone") against temperature measured at El Monte ("Temp_Monte")
ggplot(Ozone_dt, aes(x = Temp_Monte, y = Ozone)) + geom_point() # Linear/non-linear?

# Two key functions we will make use of from the "caret" package to
# > estimate generic supervised learning models: train()
# > do cross-validation: trainControl()

# Use package "caret" to do k-fold cross-validation
tc <- caret::trainControl(method = 'cv', number = 10)

# --------------------------------------------------------
# Let's start with applying decision trees to the problem
# --------------------------------------------------------

# To fit a decision tree with the caret package, we specify methods argument
# equal to "rpart" (which is an R-package for estimating decision trees)
# > We use the tuneGrid argument to specify hyperparameter-combinations we want to estimate different models for
# > The rpart package takes as input a "cp" parameter. This is the penalty (ISL: alpha)
set.seed(1234567)
rpart_model <- caret::train(
  Ozone ~ .,
  data = Ozone_dt,
  tuneGrid = expand.grid(cp = c(0, 0.001, 0.01, 0.05, 0.1)),
  method = "rpart",
  trControl = tc
)

# We inspect the results by printing the object
rpart_model # In this case, pruning (that is cp > 0, did not help improve performance much)

# We can extract the results table (to e.g., enable plotting) as follows
rpart_model$results
ggplot(rpart_model$results, aes(x = cp, y = RMSE)) + geom_point()
ggplot(rpart_model$results, aes(x = cp, y = Rsquared)) + geom_point()

# We can extract the "best model" using $finalModel (the cp parameter having the lowest error)
best_decision_tree <- rpart_model$finalModel

# And we can plot its tree structure using rpart.plot
rpart.plot(best_decision_tree)

# To calculate variable importance, we may use the command:
caret::varImp(best_decision_tree)

# To plot it, we can do as follows:
varimp_object <- caret::varImp(best_decision_tree)
varimp_dt <- data.table(
  var = rownames(varimp_object),
  imp = varimp_object$Overall
)
varimp_dt <- varimp_dt[order(imp, decreasing = T)]
varimp_dt[, imp := imp / max(imp)] # Making variable importance into a relative measure
ggplot(varimp_dt, aes(y = reorder(var, imp), x = imp)) + geom_point(size = 3)

# Note: if we allow our tree to grow deeper (by lowering the minimum number of obs in a leaf,
# then pruning helps more!)
set.seed(1234567)
rpart_model2 <- caret::train(
  Ozone ~ .,
  data = Ozone_dt,
  tuneGrid = expand.grid(cp = c(0, 0.001, 0.01, 0.05, 0.1)),
  control = rpart.control(minbucket = 2),
  method = "rpart",
  trControl = tc
)
rpart_model2
ggplot(rpart_model2$results, aes(x = cp, y = Rsquared)) + geom_point()


# --------------------------------------------------------
# Let's now try a random forest on the same data
# --------------------------------------------------------

# To fit a random forest using the caret package, we simply set method="rf"
# > Note that now we now have a parameter "mtry" instead of "cp".
# > mtry = number of variables to evaluate for each split
mtry_values <- seq(1, ncol(Ozone_dt), 2)
set.seed(1234567)
rf_model <- caret::train(
  Ozone ~ .,
  data = Ozone_dt,
  tuneGrid = expand.grid(mtry = mtry_values),
  method = "rf",
  trControl = tc
)
# Again, inspect
rf_model

# Notes:
# - Substantial improvement compared to standard decision tree (Rsquared: 0.77 > 0.67)
# - Does "decorrelating" the trees help? Yes! RMSE(mtry=1) < RMSE(mtry=9).

# Again, we can extract best model
best_rf_model <- rf_model$finalModel # As we see here, we used 500 trees. We can change the number of trees with the "ntrees" argument

# And again, we can extract variable importance:
caret::varImp(best_rf_model)
varimp_rf_object <- caret::varImp(best_rf_model)
varimp_rf_dt <- data.table(
  var = rownames(varimp_rf_object),
  imp = varimp_rf_object$Overall
)
varimp_rf_dt <- varimp_rf_dt[order(imp, decreasing = T)]
varimp_rf_dt[, imp := imp / max(imp)]
varimp_rf_dt

# Compare variable importance
# -- Plot them together (rpart, rf)
varimp_dt$model <- 'rpart'
varimp_rf_dt$model <- 'rf'
varimp_combined <- rbind(varimp_dt, varimp_rf_dt)
order_by_rf <- varimp_rf_dt$var
varimp_combined$var <- factor(varimp_combined$var, levels = c(order_by_rf))
ggplot(varimp_combined, aes(y = var, imp), x = imp) +
  geom_point(size = 3) +
  facet_wrap(~model)

# Example of a meaningful difference: "Press_height" much more important for the random forest

# Let's produce a partial depenency plot for this variable, using the pdp package
partial(
  object = rf_model,
  pred.var = 'Press_height',
  train = Ozone_dt,
  grid.resolution = 20,
  plot = TRUE
)

# Very non-linear pattern indeed.

# --------------------------------------------------------
# Let's contrast: what pattern do we see in a basic linear model?
# --------------------------------------------------------
set.seed(1234567)
lm_model <- caret::train(
  Ozone ~ .,
  data = Ozone_dt,
  method = "lm",
  trControl = tc
)
partial(
  object = lm_model,
  pred.var = 'Press_height',
  train = Ozone_dt,
  grid.resolution = 20,
  plot = TRUE
)

# Quite different indeed.

# Let's also plot for the most important variable
partial(
  object = rf_model,
  pred.var = 'Temp_Monte',
  train = Ozone_dt,
  grid.resolution = 20,
  plot = TRUE
)


# --------------------------------------------------------
# What about KNN? Again, for the same data, here is how you
# would fit a kNN model, using cross-validation to select
# the appropriate k
# --------------------------------------------------------
set.seed(12345)
knn_model <- caret::train(
  Ozone ~ .,
  data = Ozone_dt,
  tuneGrid = expand.grid(k = 1:50),
  method = "knn",
  trControl = tc
)
knn_model
ggplot(knn_model$results, aes(x = k, y = RMSE)) + geom_line()
ggplot(knn_model$results, aes(x = k, y = Rsquared)) + geom_line()


# --------------------------------------------------------
# Small data? repeatedcv --> more robustness
# > It repeats cross-validation a certain number of times
# --------------------------------------------------------
tc_repeat <- caret::trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 20
)

set.seed(12345)
knn_model2 <- caret::train(
  Ozone ~ .,
  data = Ozone_dt,
  tuneGrid = expand.grid(k = 1:50),
  method = "knn",
  trControl = tc_repeat
)
knn_model2
ggplot(knn_model2$results, aes(x = k, y = RMSE)) + geom_line()
ggplot(knn_model2$results, aes(x = k, y = Rsquared)) + geom_line()
# --------------------------------------------------------
