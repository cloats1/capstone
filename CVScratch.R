paramDF <- expand.grid(
  max_depth = seq(1, 21, by = 2),
  min_child_weight = seq(1, 21, by = 2),
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
                            max_depth = 21,
                            min_child_weight = 0.01,
                            gamma = 0.1,
                            subsample = 1.0,
                            colsample_bytree = 1.0,
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
#true mean= 0.09665007, pred mean= 0.03847895, AUC= 0.5953639


paramDF <- expand.grid(
  max_depth = seq(15, 31, by = 2),
  min_child_weight = seq(0, 10, by = 1),
  eta = 0.1)
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
#AUC= 0.57579

paramDF <- expand.grid(
  max_depth = seq(18, 22, by = 1),
  min_child_weight = seq(0, 1, by = 0.1),
  eta = 0.1)
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
#AUC= 0.5794281

paramDF <- expand.grid(
  max_depth = 21,
  min_child_weight = seq(0, 0.1, by = 0.01),
  eta = 0.1)
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
#AUC= 0.5824399

paramDF <- expand.grid(
  max_depth = 21,
  min_child_weight = 0.01,
  eta = 0.1,
  gamma = seq(0, 1, by =0.1))
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
#AUC= 0.5854082



set.seed(13785)
bsta <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 20, 
                gamma=0.2, eta = 0.1, nthread = 4, nrounds = 100, subsample=0.9, 
                colsample_bytree= 1.0, lambda=0.4, objective = "binary:logistic")

preda <- predict(bsta, test3)
print(head(preda))
predictiona <- as.numeric(preda > 0.5)
print(head(predictiona))
mean(test2$Diabetes)
mean(predictiona)
auc(test2$Diabetes, predictiona)
#AUC= 0.708642

set.seed(13785)
bstb <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 20, 
                gamma=0.2, eta = 0.1, nthread = 4, nrounds = 1000, subsample=0.9, 
                colsample_bytree= 1.0, lambda=0.4, objective = "binary:logistic")

predb <- predict(bstb, test3)
print(head(predb))
predictionb <- as.numeric(predb > 0.5)
print(head(predictionb))
mean(test2$Diabetes)
mean(predictionb)
auc(test2$Diabetes, predictionb)
#AUC= 0.7137989

set.seed(13785)
bstc <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 20, 
                gamma=0.2, eta = 0.1, nthread = 4, nrounds = 1500, subsample=0.9, 
                colsample_bytree= 1.0, lambda=0.4, objective = "binary:logistic")

predc <- predict(bstc, test3)
print(head(predc))
predictionc <- as.numeric(predc > 0.5)
print(head(predictionc))
mean(test2$Diabetes)
mean(predictionc)
auc(test2$Diabetes, predictionc)
#AUC= 0.7151749

set.seed(13785)
bstd <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 20, 
                gamma=0.2, eta = 0.1, nthread = 4, nrounds = 1750, subsample=0.9, 
                colsample_bytree= 1.0, lambda=0.4, objective = "binary:logistic")

predd <- predict(bstd, test3)
print(head(predd))
predictiond <- as.numeric(predd > 0.5)
print(head(predictiond))
mean(test2$Diabetes)
mean(predictiond)
auc(test2$Diabetes, predictiond)
#AUC= 0.7155221

set.seed(13785)
bste <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 20, 
                gamma=0.2, eta = 0.1, nthread = 4, nrounds = 1750, subsample=0.9, 
                colsample_bytree= 1.0, lambda=0.3, objective = "binary:logistic")

prede <- predict(bste, test3)
print(head(prede))
predictione <- as.numeric(prede > 0.5)
print(head(predictione))
mean(test2$Diabetes)
mean(predictione)
auc(test2$Diabetes, predictione)
#AUC= 0.719303

set.seed(13785)
bstf <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 20, 
                gamma=0.2, eta = 0.1, nthread = 4, nrounds = 1750, subsample=0.9, 
                colsample_bytree= 1.0, lambda=0.3, objective = "binary:logistic")

predf <- predict(bstf, test3)
print(head(predf))
predictionf <- as.numeric(predf > 0.5)
print(head(predictionf))
mean(test2$Diabetes)
mean(predictionf)
auc(test2$Diabetes, predictionf)
#AUC= 