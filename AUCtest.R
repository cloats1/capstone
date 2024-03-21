NHANESauc= NHANES_full[, -1:-2]
head(NHANESauc)
NHANESauc= NHANESauc[,-3:-4]
head(NHANESauc)
NHANESauc= NHANESauc[,-4:-5]
head(NHANESauc)
NHANESauc= NHANESauc[,-7]
head(NHANESauc)
NHANESauc= NHANESauc[,-9]
head(NHANESauc)
NHANESauc= NHANESauc[,-9]
head(NHANESauc)
NHANESauc= NHANESauc[,-10]
head(NHANESauc)
NHANESauc= NHANESauc[,-13]
head(NHANESauc)
NHANESauc= NHANESauc[,-15]
head(NHANESauc)
NHANESauc= NHANESauc[,-3]
head(NHANESauc)
NHANESauc= NHANESauc[,-15:-16]
head(NHANESauc)
ga <- data.table(NHANESauc, keep.rownames = FALSE)
head(ga)
gamy <- dummyVars(" ~ .", data = ga)
NHANES_dummya <- data.frame(predict(gamy, newdata = ga))
dummya <- NHANES_dummya  %>% drop_na(Diabetes.Yes)

head(dummya)
ncol(dummya)

dummya= dummya[, -8:-11]
head(dummya)

aucwhite <- dummya[dummya[, "Race.Non.Hispanic.White"] == 1,]
aucblack <- dummya[dummya[, "Race.Non.Hispanic.Black"] == 1,]
#removing other racial categories from black subset
aucblack= aucblack[, -2:-6]
head(aucblack)
#removing other racial categories from white subset
aucwhite= aucwhite[, -2:-6]
head(aucwhite)
ncol(aucwhite)
ncol(aucblack)

trainwhitea <- createDataPartition(aucwhite$Diabetes.Yes, p=.8, list=FALSE, times=1)
traina <- aucwhite[trainwhitea,]
testa <- aucwhite[-trainwhitea,]
testa2 <- aucwhite[-trainwhitea,]
output_vectora <- traina[ 'Diabetes.Yes'] == 1 
weightsa <- traina['surveyweight']
weightsa <- as.double(unlist(weightsa))
weightsa2 <- testa['surveyweight']
weightsa2 <- as.double(unlist(weightsa2))

traina = traina[, -14]
#removing Y column
traina2= traina[, -2]
traina2 <- as.matrix(traina2)
bsthope <- xgboost(data = traina2, label = output_vectora, weight = weightsa, max_depth = 4,
                eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")
importance <- xgb.importance(feature_names = colnames(traina2), model = bsthope)
head(importance)

testa2 = testa2[, -14]
#removing Y column from test set
testa3= testa2[, -2]
testa3 <- as.matrix(testa3)
predhope <- predict(bsthope, testa3)
print(head(predhope))
predictionH <- as.numeric(predhope > 0.5)
print(head(predictionH))
mean(testa2$Diabetes.Yes)
mean(predictionH)

bst11 <- xgboost(data = traina2, label = output_vectora, weight = weightsa, max_depth = 25, 
                gamma= 0, eta = 0.7, nthread = 4, nrounds = 100, colsample_bytree = 1.0,
                subsample = 1.0, min_child_weight = 1, lambda = 1.0,
                objective = "binary:logistic")

pred11 <- predict(bst11, testa3)
print(head(pred11))
prediction11 <- as.numeric(pred11 > 0.5)
print(head(prediction11))
mean(testa2$Diabetes.Yes)
mean(prediction11)
auc(testa2$Diabetes.Yes, prediction11)

bstauca <- xgboost(data = traina2, label = output_vectora, weight = weightsa, max_depth = 100, 
                  max_leaves = 10000, eta = 0.001, nthread = 4, nrounds = 3000, 
                  objective = "binary:logistic")

predauca <- predict(bstauca, traina2)
print(head(predauca))
predictionauca <- as.numeric(predauca > 0.5)
print(head(predictionauca))
mean(traina$Diabetes.Yes)
mean(predictionauca)
auc(traina$Diabetes.Yes, predictionauca)
#AUC = 0.9748434

bstaucb <- xgboost(data = traina2, label = output_vectora, weight = weightsa, max_depth = 100, 
                  max_leaves = 10000, eta = 0.001, nthread = 4, nrounds = 3000, 
                  objective = "binary:logistic")

predaucb <- predict(bstaucb, testa3)
print(head(predaucb))
predictionaucb <- as.numeric(predaucb > 0.5)
print(head(predictionaucb))
mean(testa2$Diabetes.Yes)
mean(predictionaucb)
auc(testa2$Diabetes.Yes, predictionaucb)

trainscale <- scale(traina2)
trainscale <- as.matrix(trainscale)



bstscale <- xgboost(data = trainscale, label = output_vectora, weight = weightsa, max_depth = 100, 
                   max_leaves = 10000, eta = 0.001, nthread = 4, nrounds = 3000, 
                   objective = "binary:logistic")

testscale <- scale(testa3)
testscale <- scale(testscale)

predscale <- predict(bstscale, testscale)
print(head(predscale))
predictionscale <- as.numeric(predscale > 0.5)
print(head(predictionscale))
mean(testa2$Diabetes.Yes)
mean(predictionscale)
auc(testa2$Diabetes.Yes, predictionscale)