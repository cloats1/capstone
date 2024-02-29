
#Code to merge all the NHANES years being looked at into a full dataset
C1 <- rbind(NHANES2001, NHANES2003)
C2 <- rbind(C1, NHANES2005)
C3 <- rbind(C2, NHANES2007)
C4 <- rbind(C3, NHANES2009)
#add column of missing data for family history of diabetes
C4$`Family history of Diabetes` <- '.'
C5 <- rbind(C4, NHANES2011)
C6 <- rbind(C5, NHANES2013)
C7 <- rbind(C6, NHANES2015)
#add column of missing data for alcohol question
NHANES2017$`Had at least 12 alcohol drinks/ 1 yr?` <- '.'
NHANES_full <- rbind(C7, NHANES2017)
ls(NHANES_full)
#remove all subjects younger than 20 years of age
NHANES_full = subset(NHANES_full, NHANES_full$`Age in years at screening` > 19)

#Separating the data into a white training, white testing, and other race sets
NHANES_split <- split(NHANES_full, f= NHANES_full$`Race/Hispanic Origin`)
NHANES_split 
NHANES_white <-NHANES_split$`Non-Hispanic White`
NHANES_white <- NHANES_white %>% select(-"Race/Hispanic Origin")
NHANES_black <-NHANES_split$`Non-Hispanic Black`
NHANES_black <- NHANES_black %>% select(-"Race/Hispanic Origin")
NHANES_mexican <-NHANES_split$`Mexican American`
NHANES_mexican <- NHANES_mexican %>% select(-"Race/Hispanic Origin")
NHANES_hispanic <-NHANES_split$`Other Hispanic`
NHANES_hispanic <- NHANES_hispanic  %>% select(-"Race/Hispanic Origin")
NHANES_other <- NHANES_split$`Other Race - Including Multi-Racial`
NHANES_other <- NHANES_other %>% select(-"Race/Hispanic Origin")
is.data.frame(NHANES_white)

#Testing xgboost on just NHANES2013
testNHANES2013 = subset(NHANES2013, NHANES2013$`Age in years at screening` > 19)
NHANES2013 <- testNHANES2013

data(NHANES2013)
df <- data.table(testNHANES2013, keep.rownames = FALSE)
df[, SEQN := NULL]
head(df)
#attempting to separate categorical variables into dummy variables
dmy <- dummyVars(" ~ .", data = df)
trsf <- data.frame(predict(dmy, newdata = df))
print(trsf)
trsf$X.Doctor.told.you.have.diabetes.Yes
train2013 <- createDataPartition(trsf$X.Doctor.told.you.have.diabetes.Yes,
                                 p=.8, list=FALSE, times=1)
sampletrain <- trsf[train2013,]
sampletest <- trsf[-train2013,]
output_vector <- sampletrain[ 'X.Doctor.told.you.have.diabetes.Yes'] == 1 
sampletrain <- as.matrix(sampletrain)

bst <- xgboost(data = sampletrain, label = output_vector, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")

sampletest <- as.matrix(sampletest)
sampletest <- xgb.DMatrix(sampletest)
pred <- predict(bst, sampletest[1:1153 ,'X.Doctor.told.you.have.diabetes.Yes'])
print(head(pred))
prediction <- as.numeric(pred > 0.5)
print(head(prediction))
prediction

#Creating dummy variables for the NHANES white dataset
df <- data.table(NHANES_white, keep.rownames = FALSE)
df[, SEQN := NULL]
head(df)
dmy <- dummyVars(" ~ .", data = df)
trsf <- data.frame(predict(dmy, newdata = df))
print(trsf)
trsf$X.Doctor.told.you.have.diabetes.Yes
trsf <- trsf %>% drop_na(X.Doctor.told.you.have.diabetes.Yes)

#Splitting white dataset into training and testing
trainwhite <- createDataPartition(trsf$X.Doctor.told.you.have.diabetes.Yes,
                                 p=.8, list=FALSE, times=1)
train <- trsf[trainwhite,]
test <- trsf[-trainwhite,]
output_vector <- train[ 'X.Doctor.told.you.have.diabetes.Yes'] == 1 
train <-as.matrix(train)
bst <- xgboost(data = train, label = output_vector, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")
test <- as.matrix(test)
test <- xgb.DMatrix(test)
pred <- predict(bst, test[1:4031 ,'X.Doctor.told.you.have.diabetes.Yes'])
print(head(pred))
prediction <- as.numeric(pred > 0.5)
print(head(prediction))
prediction

#Preparing black dataset for prediction
black <- data.table(NHANES_black, keep.rownames = FALSE)
black[, SEQN := NULL]
head(black)
bdmy <- dummyVars(" ~ .", data = black)
btrsf <- data.frame(predict(bdmy, newdata = black))
print(btrsf)
btrsf$X.Doctor.told.you.have.diabetes.Yes
btrsf <- btrsf %>% drop_na(X.Doctor.told.you.have.diabetes.Yes)
btest <- as.matrix(btrsf)
#Need to add in the following column to match NHANES_white but this causes
#btest to stop being a matrix according to xgb.DMatrix
btest <- cbind(btest, "X.Family.history.of.Diabetes.Do.not.know"='.')
btest <- as.matrix(btest)
btest <- xgb.DMatrix(btest)
nrow(btest)
pred <- predict(bst, btest[1:9308 ,'X.Doctor.told.you.have.diabetes.Yes'])
#Feature names stored in `object` and `newdata` are different!