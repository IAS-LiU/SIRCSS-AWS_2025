# -------------------------------------------------------------------
# Lab 3 — R Helpfile
# -------------------------------------------------------------------

set.seed(77142)

# ----------------------------
# Load needed packages
# ----------------------------
library(data.table)
library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)
library(htetree)
library(grf)
library(ggplot2)

# ----------------------------
# (0) Utilities
# ----------------------------
clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))
make_rank_deciles <- function(v) {
  r <- rank(v, ties.method="average", na.last="keep")
  d <- as.integer(ceiling(10 * r / max(r, na.rm=TRUE)))
  d[d < 1] <- 1; d[d > 10] <- 10
  factor(d, levels = 1:10, labels = paste0("D", 1:10))
}

# ----------------------------
# (1) Import data
# ----------------------------

# Data: simulated data containing information about cultural products and sales.
# Objective: Estimate the causal effect of advertisement on sales, and understand
#            how the the effect of advertisement is moderated. Note: all variables
#            are measured before the advertisement was implemented (except the outcome).

path_to_ads_dt <- '/Users/marar08/Documents/Teaching/MLSS_HT2025/Labs/W6/ads_dt.rds'
ads <- readRDS(file = path_to_ads_dt)
head(ads)

# 
ads[,season:=factor(season)]

# Baseline difference, without any controls
print(ads[, .(N=.N, mean_sales=mean(sales)), by=ad])

# Sales are on average higher for cultural items that are advertised

# ----------------------------
# (2) OLS
# ----------------------------
ols <- lm(sales ~ .,data = ads)
print(summary(ols)$coef["ad", , drop = FALSE])

# ----------------------------
# (3) Orthogonal learner with RF: 
# > Without cross-fitting
# ----------------------------
set.seed(1)
rf_m_in <- randomForest(x=as.data.frame(ads[, -c('ad','sales'),with=F]), y=as.factor(ads$ad), ntree=250, mtry=2)
rf_g_in <- randomForest(x=as.data.frame(ads[, -c('ad','sales'),with=F]), y=ads$sales, ntree=250, mtry=2)
mhat_in <- predict(rf_m_in, newdata=as.data.frame(ads[, -c('ad','sales'),with=F]), type="prob")[,"1"]
ghat_in <- predict(rf_g_in, newdata=as.data.frame(ads[, -c('ad','sales'),with=F]))
ads[, `:=`(X_tilde_in = ad - mhat_in, 
           Y_tilde_in = sales - ghat_in)]
orth_rf <- lm(Y_tilde_in ~ X_tilde_in, data=ads)
summary(orth_rf)

# Remove residuals
ads[,c('X_tilde_in','Y_tilde_in') := NULL]

# ----------------------------
# (4) Orthogonal learner with RF:
# > With 5-fold cross-fitting
# ----------------------------
set.seed(2)
folds <- caret::createFolds(ads$ad, k=5, returnTrain=FALSE)
xhat_oof <- yhat_oof <- rep(NA_real_, nrow(ads))
for (i in seq_along(folds)) {
  te <- folds[[i]]; tr <- setdiff(seq_len(nrow(ads)), te)
  rf_m <- randomForest(x=as.data.frame(ads[tr, -c('ad','sales'),with=F]), y=as.factor(ads$ad[tr]), ntree=250, mtry=2)
  rf_g <- randomForest(x=as.data.frame(ads[tr, -c('ad','sales'),with=F]), y=ads$sales[tr], ntree=250, mtry=2)
  xhat_oof[te] <- predict(rf_m, newdata=as.data.frame(ads[te, -c('ad','sales'),with=F]), type="prob")[,"1"]
  yhat_oof[te] <- predict(rf_g, newdata=as.data.frame(ads[te, -c('ad','sales'),with=F]))
}
ads[, `:=`(X_tilde = ad - xhat_oof, Y_tilde = sales - yhat_oof)]
orth_rf2 <- lm(Y_tilde ~ X_tilde, data=ads)
print(summary(orth_rf2)$coef["X_tilde", , drop=FALSE])

# ----------------------------
# (5) Causal tree
# ----------------------------
ct <- causalTree(
  sales ~ price + discount + comp_price + rating + traffic + 
                  endcap + weekend + factor(season) + category,
  data      = as.data.frame(ads),
  treatment = ads$ad, 
  split.Rule   = "CT",    # treatment-effect splits
  split.Honest = TRUE,    # honest splitting/estimation (default: 50/50)
  cv.option    = "CT",    # cross-validate complexity
  cv.Honest    = TRUE,    # CV honors honesty too
  split.Bucket = TRUE,    # candidate split bucketing (instead of looking for all possible splits --> more efficient)
  bucketNum    = 40,      # number of observations per bucket
  minsize      = 50,      # minimum number of obs in a node to allow split
  xval         = 5        # number of CV folds
)
# If CV picks root-only, try a slightly different grid
n_pre_prune <- length(unique(ct$where))
opcp <- ct$cptable[which.min(ct$cptable[,"rel error"]), "CP"]
if(opcp>0){
  ct_p <- prune(ct, cp = opcp)
}else{
  ct_p <- ct
}
n_post_prune <- length(unique(ct_p$where))
cat(sprintf("\nCausal tree nodes: pre-prune leaves=%d; post-prune leaves=%d\n", n_pre_prune, n_post_prune))
 
# If we only get root node, encourage to grow larger 
if (n_post_prune <= 1) {
  # Retry with smaller minsize and more buckets to encourage a first split
  set.seed(4)
  ct <- causalTree(
    sales ~ price + discount + comp_price + rating + traffic + endcap +
            weekend + factor(season) + category,
    data      = as.data.frame(ads),
    treatment = ads$ad,
    split.Rule   = "CT",
    split.Honest = TRUE,
    cv.option    = "CT",
    cv.Honest    = TRUE,
    split.Bucket = TRUE,
    bucketNum    = 60,
    minsize      = 35,
    xval         = 5
  )
  opcp <- ct$cptable[which.min(ct$cptable[,"rel error"]), "CP"]
  ct_p <- prune(ct, cp = opcp)
  n_post_prune <- length(unique(ct_p$where))
  cat(sprintf("Retry tree: post-prune leaves=%d\n", n_post_prune))
}
  
# Visualize
rpart.plot::rpart.plot(ct_p, type=2, extra=101, under=TRUE, fallen.leaves=TRUE,
                       tweak=1.0, main="Causal Tree (pruned)")

  
# Calculate leaf estimates manually (no weighting)
leaves <- factor(ct_p$where)
leaf_tab <- copy(ads)[, leaf := leaves][,
  .(n=.N, 
    eff = mean(sales[ad==1]) - mean(sales[ad==0])),
  by = leaf][order(-eff)]
cat("\nTop leaves (naive diff-in-means):\n"); print(head(leaf_tab, 8))


# ----------------------------
# (6) Causal forest
# ----------------------------
ads[,season := as.integer(as.character(season))]
X <- as.matrix(ads[, .(price, discount, comp_price, rating, traffic,
                       endcap, weekend, season,
                       catA = as.integer(category=="A"),
                       catB = as.integer(category=="B"))])
Y <- ads$sales
W <- ads$ad
set.seed(5)
cf <- causal_forest(X, Y, W, num.trees = 2000, honesty = TRUE)

# ATE
cat("\nGRF average treatment effect:\n")
print(average_treatment_effect(cf))

# ATE on overlap population
e_hat <- cf$W.hat
ate_overlap <- average_treatment_effect(cf, subset = (e_hat >= .05 & e_hat <= .95))
# ATC as a robustness estimand
atc <- average_treatment_effect(cf, target.sample = "control")
list(ATE_overlap = ate_overlap, ATC = atc)

# Variable importance
vi <- variable_importance(cf)
cat("\nTop GRF variable importance:\n")
var_imp_dt <- data.table(var=colnames(X), 
                         importance=as.numeric(vi))
print(head(var_imp_dt[order(-importance)], 8))

# CATE by deciles of rating
dec_rating <- make_rank_deciles(ads$rating)
unq_ratings <- sort(unique(dec_rating))
ate_ratings_dt <- list()
for(i in 1:length(unq_ratings)){
  current_rating_idx <- dec_rating==unq_ratings[i]
  current_rating_ate <- average_treatment_effect(cf, subset = current_rating_idx)
  ate_ratings_dt <- rbindlist(list(ate_ratings_dt,
                                   as.data.table(matrix(current_rating_ate,ncol = 2))),
                              use.names = T, fill = T)
  ate_ratings_dt[i,rating := unq_ratings[i]]
}
# Fix names
setnames(ate_ratings_dt,c('V1','V2'),c('cate','se'))
# Plot
ggplot(ate_ratings_dt,aes(x=rating,y=cate)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=cate-(2*se),ymax=cate+(2*se)),width=0.4)

