install.packages("xgboost")
install.packages("dplyr")
install.packages('mefa')
install.packages('Rtsne')

library(dplyr)
library(xgboost)
library(data.table)
library(rpart)
setwd('/Users/jacobyoung/documents/DataScience')

nbadata <- read.csv(file="shot_logs.csv",head=TRUE,sep=",")



#change game_clock
nbadata$GAME_CLOCK <- as.character(nbadata$GAME_CLOCK)
minPerGameLeft <- nbadata$GAME_CLOCK

minPerGameLeft <- sapply(strsplit(minPerGameLeft,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)

nbadata$GAME_CLOCK <- minPerGameLeft


nbadata$SHOT_CLOCK <- as.numeric(nbadata$SHOT_CLOCK)

#predict missing value
predicted_clock <- rpart(nbadata$SHOT_CLOCK ~ nbadata$DRIBBLES + nbadata$GAME_CLOCK+ nbadata$CLOSE_DEF_DIST + nbadata$PERIOD+nbadata$SHOT_DIST,
                         data = nbadata[!is.na(nbadata$SHOT_CLOCK),], method = "anova")
nbadata$SHOT_CLOCK[is.na(nbadata$SHOT_CLOCK)] <- predict(predicted_clock, nbadata[is.na(nbadata$SHOT_CLOCK),])

#nbadata <- nbadata[ which(nbadata$PTS_TYPE==3), ]
#nbadata$GAME_CLOCK <- as.character(nbadata$GAME_CLOCK)
#library(lubridate)

#nbadata$GAME_CLOCK[is.na(nbadata$GAME_CLOCK)] <- '1:00'

#lapply(1:3, function(x) x^2)


#nbadata <- nbadata[ which(nbadata$SHOT_CLOCK<5), ]
#nbadata <- nbadata[which(nbadata$GAME_CLOCK<10)]
nbadata <- nbadata[,c(1,5,6,7,8,9,10,11,12,13,14,16,17,21)] #cut it down

#check missing values
table(is.na(nbadata))
sapply(nbadata, function(x) sum(is.na(x))/length(x))*100

#quick data cleaning
#remove extra character from target variable
library(stringr)

#remove leading whitespaces
char_col <- colnames(nbadata)[ sapply (nbadata,is.character)]
for(i in char_col) set(nbadata,j=i,value = str_trim(nbadata[[i]],side = "left"))

#set all missing value as "Missing"
nbadata[is.na(nbadata)] <- "Missing"



nbadata1 <- nbadata[ which(nbadata$player_id==2544), ] #lebron 
nbadata2 <- nbadata[ which(nbadata$player_id==202681), ] #kyrie
nbadata3 <- nbadata[ which(nbadata$player_id==201567), ] #kevin love
nbadata4 <- nbadata[ which(nbadata$player_id==101112), ] #channing fyre
nbadata5 <- nbadata[ which(nbadata$player_id==201160), ] #jr

############################## setting intervals 
percent_training <- 0.2

n_training_obs1 <- floor(percent_training*nrow(nbadata1))
train_ind1 <- sample(1:nrow(nbadata1), size = n_training_obs1)

n_training_obs2 <- floor(percent_training*nrow(nbadata2))
train_ind2 <- sample(1:nrow(nbadata2), size = n_training_obs2)

n_training_obs3 <- floor(percent_training*nrow(nbadata3))
train_ind3 <- sample(1:nrow(nbadata3), size = n_training_obs3)

n_training_obs4 <- floor(percent_training*nrow(nbadata4))
train_ind4 <- sample(1:nrow(nbadata4), size = n_training_obs4)

n_training_obs5 <- floor(percent_training*nrow(nbadata5))
train_ind5 <- sample(1:nrow(nbadata5), size = n_training_obs5)

###################


#lebron james
nba1.train <- nbadata1[train_ind1,]
nba1.test <- nbadata1[-train_ind1,]
#nba2- Kyrie Irving
nba2.train <- nbadata2[train_ind2,]
nba2.test <- nbadata2[-train_ind2,]
#kevin love
nba3.train <- nbadata3[train_ind3,]
nba3.test <- nbadata3[-train_ind3,]
#channing frye
nba4.train <- nbadata4[train_ind4,]
nba4.test <- nbadata4[-train_ind4,]
#jr smith
nba5.train <- nbadata5[train_ind5,]
nba5.test <- nbadata5[-train_ind5,]

#nbadatax <- nbadata[30:35,]
#convert data frame to data table for all #######
nba1.train <- setDT(nba1.train)
nba1.test <- setDT(nba1.test)

#nbadatax<- setDT(nbadatax)
nba2.train <- setDT(nba2.train)
nba2.test <- setDT(nba2.test)

nba3.train <- setDT(nba3.train)
nba3.test <- setDT(nba3.test)

nba4.train <- setDT(nba4.train)
nba4.test <- setDT(nba4.test)

nba5.train <- setDT(nba5.train)
nba5.test <- setDT(nba5.test)

nbadatax<- nba1.train[0:5]
nbadatax$GAME_ID <- 10
nbadatax$PERIOD <-4
nbadatax$TOUCH_TIME <-4.4
nbadatax$SHOT_CLOCK <-4.1
nbadatax$DRIBBLES <-17
nbadatax$GAME_CLOCK <-.0333

nbadatax$SHOT_DIST <-24
nbadatax$SHOT_NUMBER <- 21
nbadatax$player_id <- 0
nbadatax$FINAL_MARGIN <- 1
nbadatax$CLOSEST_DEFENDER_PLAYER_ID <-0
nbadatax$CLOSE_DEF_DIST <-4




##########################
#labels

labels1 <- nba1.train$SHOT_RESULT
ts_labels1 <- nba1.test$SHOT_RESULT

labels2 <- nba2.train$SHOT_RESULT
ts_labels2 <- nba2.test$SHOT_RESULT

labels3 <- nba3.train$SHOT_RESULT
ts_labels3 <- nba3.test$SHOT_RESULT

labels4 <- nba4.train$SHOT_RESULT
ts_labels4 <- nba4.test$SHOT_RESULT

labels5 <- nba5.train$SHOT_RESULT
ts_labels5 <- nba5.test$SHOT_RESULT

labels <- nbadatax$SHOT_RESULT

#####################
nba_test <- model.matrix(~.+0,data = nbadatax[,-c("SHOT_RESULT"),with=F])

new_train1 <- model.matrix(~.+0,data = nba1.train[,-c("SHOT_RESULT"),with=F])
new_test1 <- model.matrix(~.+0,data = nba1.test[,-c("SHOT_RESULT"),with=F])


new_train2 <- model.matrix(~.+0,data = nba2.train[,-c("SHOT_RESULT"),with=F])
new_test2 <- model.matrix(~.+0,data = nba2.test[,-c("SHOT_RESULT"),with=F])

new_train3 <- model.matrix(~.+0,data = nba3.train[,-c("SHOT_RESULT"),with=F])
new_test3 <- model.matrix(~.+0,data = nba3.test[,-c("SHOT_RESULT"),with=F])

new_train4 <- model.matrix(~.+0,data = nba4.train[,-c("SHOT_RESULT"),with=F])
new_test4 <- model.matrix(~.+0,data = nba4.test[,-c("SHOT_RESULT"),with=F])

new_train5 <- model.matrix(~.+0,data = nba5.train[,-c("SHOT_RESULT"),with=F])
new_test5 <- model.matrix(~.+0,data = nba5.test[,-c("SHOT_RESULT"),with=F])

################


labels1 <- as.numeric(labels1)-1
ts_labels1 <- as.numeric(ts_labels1)-1

labels2 <- as.numeric(labels2)-1
ts_labels2 <- as.numeric(ts_labels2)-1

labels3 <- as.numeric(labels3)-1
ts_labels3 <- as.numeric(ts_labels3)-1

labels4 <- as.numeric(labels4)-1
ts_labels4 <- as.numeric(ts_labels4)-1

labels5 <- as.numeric(labels5)-1
ts_labels5 <- as.numeric(ts_labels5)-1

labelstest <- as.numeric(labels)-1
######################

library(xgboost)
#install.packages("mlr")

library(mlr)
xtest <- xgb.DMatrix(data = nba_test,label = labelstest)


dtrain1 <- xgb.DMatrix(data = new_train1,label = labels1)
dtest1 <- xgb.DMatrix(data = new_test1,label=ts_labels1)

dtrain2 <- xgb.DMatrix(data = new_train2,label = labels2)
dtest2 <- xgb.DMatrix(data = new_test2,label=ts_labels2)

dtrain3 <- xgb.DMatrix(data = new_train3,label = labels3)
dtest3 <- xgb.DMatrix(data = new_test3,label=ts_labels3)

dtrain4 <- xgb.DMatrix(data = new_train4,label = labels4)
dtest4 <- xgb.DMatrix(data = new_test4,label=ts_labels4)

dtrain5 <- xgb.DMatrix(data = new_train5,label = labels5)
dtest5 <- xgb.DMatrix(data = new_test5,label=ts_labels5)



##################


xgbcv1 <- xgboost(data = dtrain1, label = labels1, max_depth = 4,
                 eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

xgbcv2 <- xgboost(data = dtrain2, label = labels2, max_depth = 4,
                 eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

xgbcv3 <- xgboost(data = dtrain3, label = labels3, max_depth = 4,
                 eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

xgbcv4 <- xgboost(data = dtrain4, label = labels4, max_depth = 4,
                 eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")


xgbcv5 <- xgboost(data = dtrain5, label = labels5, max_depth = 4,
                 eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")



########################creating testing dataset

xgbcvtest1 <- xgboost(data = dtest1, label = ts_labels1, max_depth = 4,
                  eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

xgbcvtest2 <- xgboost(data = dtest2, label = ts_labels2, max_depth = 4,
                  eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

xgbcvtest3 <- xgboost(data = dtest3, label = ts_labels3, max_depth = 4,
                  eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

xgbcvtest4 <- xgboost(data = dtest4, label = ts_labels4, max_depth = 4,
                  eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

xgbcvtest5 <- xgboost(data = dtest5, label = ts_labels5, max_depth = 4,
                  eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")





#####################
importance <- xgb.importance(feature_names = colnames(dtrain1), model = xgbcv)
head(importance)

xgbcvtest1 <- xgboost(data = dtest1, label = labels1, max_depth = 6,
                     eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

importanceRaw <- xgb.importance(feature_names = colnames(dtest1), model = xgbcvtest)
# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]


head(importanceClean)


xgb.plot.importance(importance_matrix = importance)



ypred1 <- predict(xgbcvtest1, xtest)
ypred2 <- predict(xgbcvtest2, xtest)
ypred3 <- predict(xgbcvtest3, xtest) #KEVIN LOVE
ypred4 <- predict(xgbcvtest4, xtest)
ypred5 <- predict(xgbcvtest5, xtest)


############visualization
install.packages('corrplot')

library(Rtsne)

tsne = Rtsne(new_train1, check_duplicates=FALSE, pca=TRUE, 
             perplexity=30, theta=0.5, dims=2)
embedding = as.data.frame(tsne$Y)
embedding$Class = labels
g = ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  geom_point(size=1.25) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE 2D Embedding of 'Classe' Outcome") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
print(g)

corrplot.mixed(cor(new_train1), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")
