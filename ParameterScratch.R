xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 10000, nfold = 10, showsd = T,
                 stratified = T, print_every_n = 10, early_stopping_round = 50,
                 eval_metric = "error", maximize = F)

xgb1 <- xgb.train (params = params, data = dtrain, nrounds =43, watchlist = 
                     list(val=dtest,train=dtrain), print_ever_n = 10, early_stopping_round = 50,
                   maximize = F , eval_metric = "error",  weight=weights)

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
head(xgbpred)
mean(ctest$Diabetes.Yes)
mean(xgbpred)
mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 
auc(ctest$Diabetes.Yes, xgbpred)


params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3,
               gamma=0, max_depth=20, min_child_weight=1, subsample=1, colsample_bytree=1)
#AUC = 0.5397952, 0.6020942 non-standardized

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1,
               gamma=0, max_depth=5, min_child_weight=1, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.562, 0.595174 non-standardized

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1,
               gamma=0, max_depth=20, min_child_weight=1, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.5367756, 0.5767418 non-standardized

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1,
               gamma=0, max_depth=20, min_child_weight=1, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.536353, 0.5799093 non-standardized

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1,
               gamma=0, max_depth=20, min_child_weight=5, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.5521743

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1,
               gamma=0, max_depth=20, min_child_weight=10, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.5641307, 0.5908624 non-standardized

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.01,
               gamma=0, max_depth=5, min_child_weight=1, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.5303748

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.01,
               gamma=0, max_depth=10, min_child_weight=1, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.5307974

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.01,
               gamma=0, max_depth=20, min_child_weight=10, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.552597

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1,
               gamma=1, max_depth=20, min_child_weight=10, subsample=0.8, 
               colsample_bytree=0.8, scale_pos_weight = 1)
#AUC = 0.551329

paramDF <- expand.grid(
  max_depth = seq(2, 20, by = 2),
  min_child_weight = seq(2, 20, by = 2),
  eta = 0.1)
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)

bestResults <- tibble()
pb <- txtProgressBar(style = 3)
for(i in seq(length(paramList))) {
  rwCV <- xgb.cv(params = paramList[[i]],
                 data = dtrain, 
                 nrounds = 500, 
                 nfold = 10,
                 early_stopping_rounds = 10,
                 verbose = FALSE,
                 eval_metric = "error",
                 objective = "binary:logistic")
  bestResults <- bestResults %>% 
    bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
  gc() 
  setTxtProgressBar(pb, i/length(paramList))
}
close(pb)

depth_leaves <- bind_cols(paramDF, bestResults)
View(depth_leaves)

diabetestested <- xgb.train(data = dtrain, verbose = 0,
                            watchlist = list(train = dtrain, test = dtest), 
                            nrounds = 10000,
                            early_stopping_rounds = 50,
                            max_depth = 22,
                            min_child_weight = 0,
                            gamma = 0.5,
                            subsample = 0.8,
                            colsample_bytree = 0.7,
                            eta = 0.05,
                            eval_metric = "error",
                            objective = "binary:logistic")
diabetestested
diabetestested$evaluation_log %>% 
  pivot_longer(cols = c(train_error, test_error), names_to = "Error") %>% 
  ggplot(aes(x = iter, y = value, color = Error)) + geom_line()
print(diabetestested)

xgbpred1 <- predict (diabetestested,dtest)
xgbpred1 <- ifelse (xgbpred1 > 0.5,1,0)

mean(test2$Diabetes.Yes)
mean(xgbpred1)
auc(ctest$Diabetes.Yes, xgbpred1)
#AUC = 0.5414859, 0.5471012

paramDF <- expand.grid(
  max_depth = seq(10, 30, by = 2),
  min_child_weight = seq(0, 5, by = 1),
  eta = 0.1)
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
#AUC = 0.548732, 0.530068

paramDF <- expand.grid(
  gamma = seq(0, 1, by = 0.1),
  max_depth= 22,
  min_child_weight=0,
  eta = 0.1)
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
#AUC = 0.5329107, non-standardized AUC = 0.5632216
#auc= 0.5483607, 0.5536186

paramDF <- expand.grid(
  gamma = 0.5,
  max_depth= 22,
  min_child_weight=0,
  eta = 0.1,
  subsample = seq(0,1, by = 0.1),
  colsample_bytree = seq(0,1, by = 0.1))
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)


#Standardization of values, run after line 563 of NewNhanes, then run 577-640
df <- as.data.frame(dummy)
df <- na.exclude(df)
df$Age <-  ((df$Age-mean(df$Age))/sd(df$Age))
print(df$Age)
df$Smoking <-  ((df$Smoking-mean(df$Smoking))/sd(df$Smoking))
df$Weight <-  ((df$Weight-mean(df$Weight))/sd(df$Weight))
df$Height <-  ((df$Height-mean(df$Height))/sd(df$Height))
df$Waist <-  ((df$Waist-mean(df$Waist))/sd(df$Waist))
df$Leg <-  ((df$Leg-mean(df$Leg))/sd(df$Leg))
df$BMI <-  ((df$BMI-mean(df$BMI))/sd(df$BMI))
df$Arm <-  ((df$Arm-mean(df$Arm))/sd(df$Arm))
df$ArmCircumference <-  ((df$ArmCircumference-mean(df$ArmCircumference))/sd(df$ArmCircumference))
df$Cholesterol <-  ((df$Cholesterol-mean(df$Cholesterol))/sd(df$Cholesterol))
df$KcalIntake <-  ((df$KcalIntake-mean(df$KcalIntake))/sd(df$KcalIntake))
df$Sodium <-  ((df$Sodium-mean(df$Sodium))/sd(df$Sodium))
df$Carb <-  ((df$Carb-mean(df$Carb))/sd(df$Carb))
df$Fiber <-  ((df$Fiber-mean(df$Fiber))/sd(df$Fiber))
df$Calcium <-  ((df$Calcium-mean(df$Calcium))/sd(df$Calcium))
df$Caffeine <-  ((df$Caffeine-mean(df$Caffeine))/sd(df$Caffeine))
df$Pulse <-  ((df$Pulse-mean(df$Pulse))/sd(df$Pulse))
df$Systolic <-  ((df$Systolic-mean(df$Systolic))/sd(df$Systolic))
df$Diastolic <-  ((df$Diastolic-mean(df$Diastolic))/sd(df$Diastolic))
head(df)

NHANES_dmywhite <- df[df[, "Race.Non.Hispanic.White"] == 1,]
head(NHANES_dmywhite)
NHANES_dmyblack <- df[df[, "Race.Non.Hispanic.Black"] == 1,]
head(NHANES_dmyblack)
#removing other racial categories from black subset
NHANES_dmyblack= NHANES_dmyblack[, -3:-7]
head(NHANES_dmyblack)
#removing other racial categories from white subset
NHANES_dmywhite= NHANES_dmywhite[, -3:-7]
head(NHANES_dmywhite)
ncol(NHANES_dmywhite)
ncol(NHANES_dmyblack)
