library(xlsx)
library(plyr)
library(dplyr)
library(psych)
library(rpart)
library(rpart.plot)
library(ROCR)
library(heuristica)
library(caret)
library(e1071)
library(lift)
library(gains)
library(C50)
library(AUC)
library(utils)

data<-read.xlsx("data.xls",sheetIndex = 1)
summary(data)

data_t <- apply(data[,c(-1,-3,-11,-14,-23,-27,-29)],2,as.factor)

# C:  to refer to specific column in the data ----data[, c("RESPONSE")]
summary(as.factor(data[, c("RESPONSE")]))

# calculating missing values
summary(data[,c(23)])

#identifying the count of 0 and 1 associated with missing values
summary(as.factor(data[is.na(data[,c(23)])==1,c(32)]))

#plotting a stacked bar graph by grouping AGE against RESPONSE values

b <- c(-Inf,10,20,30,40,50,60,70,Inf) #creating AGE band with 10 as width

names <- c(10,20,30,40,50,60,70,80) #labelling every band

data_age <- cut(data[,c(23)],b,names) #create a new vector data_age and classifying the age with labels

counts <- table(data$RESPONSE,data_age)
barplot(counts,legend=rownames(counts))

#plotting a stacked bar graph by grouping DURATION against RESPONSE values

b <- c(-Inf,10,20,30,40,50,60,70,Inf) #creating DURATION band with 10 as width
names <- c(10,20,30,40,50,60,70,80) #labelling every band

data_duration <- cut(data[,c(3)],b,names) #create a new vector data_duration and classifying the duration with labels

counts <- table(data$RESPONSE,data_duration)
barplot(counts,legend=rownames(counts))
cor(data$DURATION,data$RESPONSE)

#plotting a stacked bar graph by grouping AMOUNT against RESPONSE values

b <- c(-Inf,2000,4000,6000,8000,10000,12000,14000,16000,18000,Inf) #creating AMOUNT band with 2000 as width
names <- c(2000,4000,6000,8000,10000,12000,14000,16000,18000,20000) #labelling every band

data_amount <- cut(data[,c(11)],b,names) #create a new vector data_amount and classifying the amount with labels

counts <- table(data$RESPONSE,data_amount)
barplot(counts,legend=rownames(counts))
cor(data[,c(11)],data$RESPONSE)

# Mean & Std. deviation of continuous variables
data_num <- describe(data[,c(3,11,14,23,27,29)])

# Frequency of continuous variables
data_factor <-  apply(data[,c(-1,-3,-11,-14,-23,-27,-29)],2,count)

#bar plot for categorical variable
counts <- table(data$RESPONSE,data$CHK_ACCT)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$CHK_ACCT,data$RESPONSE)

counts <- table(data$RESPONSE,data$HISTORY)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$HISTORY,data$RESPONSE)

counts <- table(data$RESPONSE,data$NEW_CAR)
barplot(counts,legend=rownames(counts))
ftable(counts)
#cor(data$NEW_CAR,data$RESPONSE)

counts <- table(data$RESPONSE,data$USED_CAR)
barplot(counts,legend=rownames(counts))
ftable(counts)
#cor(data$USED_CAR,data$RESPONSE)

counts <- table(data$RESPONSE,data$FURNITURE)
barplot(counts,legend=rownames(counts))
ftable(counts)
#cor(data$FURNITURE,data$RESPONSE)

counts <- table(data$RESPONSE,data$RADIO.TV)
barplot(counts,legend=rownames(counts))
ftable(counts)
#cor(data$RADIO.TV,data$RESPONSE)

counts <- table(data$RESPONSE,data$EDUCATION)
barplot(counts,legend=rownames(counts))
ftable(counts)
#cor(data$EDUCATION,data$RESPONSE)

counts <- table(data$RESPONSE,data$RETRAINING)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$RETRAINING,data$RESPONSE)

counts <- table(data$RESPONSE,data$SAV_ACCT)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$SAV_ACCT,data$RESPONSE)

counts <- table(data$RESPONSE,data$EMPLOYMENT)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$EMPLOYMENT,data$RESPONSE)

counts <- table(data$RESPONSE,data$MALE_DIV)
barplot(counts,legend=rownames(counts))
ftable(counts)
#cor(data$MALE_DIV,data$RESPONSE)

counts <- table(data$RESPONSE,data$MALE_SINGLE)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$MALE_SINGLE,data$RESPONSE)

counts <- table(data$RESPONSE,data$MALE_MAR_or_WID)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$MALE_MAR_or_WID,data$RESPONSE)

counts <- table(data$RESPONSE,data$CO.APPLICANT)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$CO.APPLICANT,data$RESPONSE)

counts <- table(data$RESPONSE,data$GUARANTOR)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$GUARANTOR,data$RESPONSE)

counts <- table(data$RESPONSE,data$PRESENT_RESIDENT)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$PRESENT_RESIDENT,data$RESPONSE)

counts <- table(data$RESPONSE,data$REAL_ESTATE)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$REAL_ESTATE,data$RESPONSE)

counts <- table(data$RESPONSE,data$PROP_UNKN_NONE)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$PROP_UNKN_NONE,data$RESPONSE)

counts <- table(data$RESPONSE,data$OTHER_INSTALL)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$OTHER_INSTALL,data$RESPONSE)

counts <- table(data$RESPONSE,data$RENT)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$RENT,data$RESPONSE)

counts <- table(data$RESPONSE,data$OWN_RES)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$OWN_RES,data$RESPONSE)

counts <- table(data$RESPONSE,data$JOB)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$JOB,data$RESPONSE)

counts <- table(data$RESPONSE,data$TELEPHONE)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$TELEPHONE,data$RESPONSE)

counts <- table(data$RESPONSE,data$FOREIGN)
barplot(counts,legend=rownames(counts))
ftable(counts)
cor(data$FOREIGN,data$RESPONSE)

#Decision Tree for full data

data_t2 <- cbind(data_t,data[,c(3,11,14,23,27,29)]) 

dt_1 <- rpart(RESPONSE ~ .,data = data_t2,method = "class", parms = list(split = "gini"),
              control = rpart.control(minsplit = 30, minbucket = round(30/3), cp = 0.005, maxcompete = 4, 
                                      maxsurrogate = 5, usesurrogate = 2, xval = 10, surrogatestyle = 0, maxdepth = 20))

prp(dt_1)
rpart.plot(dt_1)

plotcp(dt_1) #Cross validation error with cp

varImp(dt_1)

predict_0 <- predict(dt_1,data_t2[,c(-25)])

colnames(predict_0) <- c("c_0","c_1")

predict_0_check <- predict_0 %>% 
  as.data.frame() %>% 
  mutate(value = ifelse(c_1 > 0.8,1,0))

temp0 <- prediction(predict_0_check[,c(3)],data_t2[,c(25)])

predict_0_perf1 <- performance(temp0,"tpr","fpr") #ROC curve
plot(predict_0_perf1)

auc_0 <- auc(roc(predict_0_check[,c(3)],data_t2[,c(25)])) #AUC 
print(auc_0)

gain <- performance(temp0,"tpr","rpp") #Gain Chart
plot(gain, main = "gain chart")  

predict_0_check[,c(3)] <- as.factor(predict_0_check[,c(3)])

confusionMatrix(predict_0_check[,c(3)],data_t2[,c(25)]) #Confusion Matrix

plotLift(predict_0_check[,c(3)],data_t2[,c(25)]) #Lift Chart

TopDecileLift(predict_0_check[,c(3)],data_t2[,c(25)]) #Top Decile

#Building & measuring decision tree with 50% data

set.seed(1006)

train_data_t2 <- sample_frac(data_t2, 0.5)
test_data_t2 <- setdiff(data_t2,train_data_t2)

costf <- matrix(c(0,1,5,0),nrow = 2,ncol =2,byrow = TRUE)
  
dt_2 <- rpart(RESPONSE ~ .,data = train_data_t2,method = "class", parms = list(prior = c(0.7,0.3),split = "gini", loss = costf),
              control = rpart.control(minsplit = 24, minbucket = round(24/3), cp = 0.005, maxcompete = 4, 
                                      maxsurrogate = 5, usesurrogate = 2, xval = 8, surrogatestyle = 0, maxdepth = 15))

rpart.plot(dt_2)
varImp(dt_2)

tnodes <- as.numeric(rownames(dt_2$frame)) # Counting  the nodes
dt_1_depth <- max(rpart:::tree.depth(tnodes)) # Measure tree depth

plotcp(dt_2) #Cross validation error with cp

predict_1 <- predict(dt_2,test_data_t2[,c(-25)])

colnames(predict_1) <- c("c_0","c_1")

predict_1_check <- predict_1 %>% 
  as.data.frame() %>% 
  mutate(value = ifelse(c_1 > 0.5,1,0))

predict_1_checktb <- cbind(predict_1_check,test_data_t2[,c(25)])
colnames(predict_1_checktb) <- c('Prob 0','Prob 1','Predicted Class','Actual Class')
#write.xlsx(predict_1_checktb,"c:/Users/Sriram/Documents/R/prediction.xlsx")
  
temp <- prediction(predict_1_check[,c(3)],test_data_t2[,c(25)])

predict_1_perf1 <- performance(temp,"tpr","fpr") #ROC curve
plot(predict_1_perf1)

auc_1 <- auc(roc(predict_1_check[,c(3)],test_data_t2[,c(25)])) #AUC 
print(auc_1)

predict_1_check[,c(3)] <- as.factor(predict_1_check[,c(3)])

confusionMatrix(predict_1_check[,c(3)],test_data_t2[,c(25)]) #Confusion Matrix

plotLift(predict_1_check[,c(3)],test_data_t2[,c(25)]) #Lift Chart

TopDecileLift(predict_1_check[,c(3)],test_data_t2[,c(25)]) #Top Decile

#Plotting DT using C5.0

set.seed(1006)

train_data_t2 <- sample_frac(data_t2, 0.5)
test_data_t2 <- setdiff(data_t2,train_data_t2)

train_data_t21 <- train_data_t2 %>%
  mutate (NEW_AGE = ifelse(is.na(AGE),0, AGE))

train_data_t21$AGE <- NULL

#train_data_t21$NEW_AGE <- as.numeric(train_data_t21$NEW_AGE)

train_data_t21$NEW_CAR <- as.character(train_data_t21$NEW_CAR)
train_data_t21$NEW_CAR [train_data_t21$NEW_CAR == ""] <- "missing"
train_data_t21$NEW_CAR <- as.factor(train_data_t21$NEW_CAR)

train_data_t21$USED_CAR <- as.character(train_data_t21$USED_CAR)
train_data_t21$USED_CAR [train_data_t21$USED_CAR == ""] <- "missing"
train_data_t21$USED_CAR <- as.factor(train_data_t21$USED_CAR)

train_data_t21$FURNITURE <- as.character(train_data_t21$FURNITURE)
train_data_t21$FURNITURE [train_data_t21$FURNITURE == ""] <- "missing"
train_data_t21$FURNITURE <- as.factor(train_data_t21$FURNITURE)

train_data_t21$RADIO.TV <- as.character(train_data_t21$RADIO.TV)
train_data_t21$RADIO.TV [train_data_t21$RADIO.TV == ""] <- "missing"
train_data_t21$RADIO.TV <- as.factor(train_data_t21$RADIO.TV)

train_data_t21$EDUCATION <- as.character(train_data_t21$EDUCATION)
train_data_t21$EDUCATION [train_data_t21$EDUCATION == ""] <- "missing"
train_data_t21$EDUCATION <- as.factor(train_data_t21$EDUCATION)

train_data_t21$RETRAINING <- as.character(train_data_t21$RETRAINING)
train_data_t21$RETRAINING [train_data_t21$RETRAINING == ""] <- "missing"
train_data_t21$RETRAINING <- as.factor(train_data_t21$RETRAINING)

costf <- matrix(c(0,1,5,0),nrow = 2,ncol = 2,byrow = TRUE)

C50_tree <- C5.0(x = train_data_t21[,c(-25)], y = train_data_t2$RESPONSE,rules = FALSE,
                 control = C5.0Control(winnow = TRUE, CF = 0.22), costs = costf)

plot(C50_tree)

C5imp(C50_tree)

test_data_t21 <- test_data_t2 %>%
  mutate (NEW_AGE = ifelse(is.na(AGE),0, AGE))

test_data_t21$AGE <- NULL

#test_data_t21$NEW_AGE <- as.numeric(test_data_t21$NEW_AGE)

test_data_t21$NEW_CAR <- as.character(test_data_t21$NEW_CAR)
test_data_t21$NEW_CAR [test_data_t21$NEW_CAR == ""] <- "missing"
test_data_t21$NEW_CAR <- as.factor(test_data_t21$NEW_CAR)

test_data_t21$USED_CAR <- as.character(test_data_t21$USED_CAR)
test_data_t21$USED_CAR [test_data_t21$USED_CAR == ""] <- "missing"
test_data_t21$USED_CAR <- as.factor(test_data_t21$USED_CAR)

test_data_t21$FURNITURE <- as.character(test_data_t21$FURNITURE)
test_data_t21$FURNITURE [test_data_t21$FURNITURE == ""] <- "missing"
test_data_t21$FURNITURE <- as.factor(test_data_t21$FURNITURE)

test_data_t21$RADIO.TV <- as.character(test_data_t21$RADIO.TV)
test_data_t21$RADIO.TV [test_data_t21$RADIO.TV == ""] <- "missing"
test_data_t21$RADIO.TV <- as.factor(test_data_t21$RADIO.TV)

test_data_t21$EDUCATION <- as.character(test_data_t21$EDUCATION)
test_data_t21$EDUCATION [test_data_t21$EDUCATION == ""] <- "missing"
test_data_t21$EDUCATION <- as.factor(test_data_t21$EDUCATION)

test_data_t21$RETRAINING <- as.character(test_data_t21$RETRAINING)
test_data_t21$RETRAINING [test_data_t21$RETRAINING == ""] <- "missing"
test_data_t21$RETRAINING <- as.factor(test_data_t21$RETRAINING)

predict_2 <- predict(C50_tree,test_data_t21[,c(-25)])

predict_2_tmp <- as.data.frame(predict_2)
predict_2_tmp[,c(1)] <- as.numeric(as.character(predict_2_tmp[,c(1)]))

temp1 <- prediction(predict_2_tmp[,c(1)],test_data_t21[,c(25)])

predict_2_perf1 <- performance(temp1,"tpr","fpr") #ROC curve
plot(predict_2_perf1)

auc_2 <- auc(roc(predict_2_tmp[,c(1)],test_data_t21[,c(25)])) #AUC 
print(auc_2)

predict_2_tmp[,c(1)] <- as.factor(predict_2_tmp[,c(1)])

confusionMatrix(predict_2_tmp[,c(1)],test_data_t21[,c(25)]) #Confusion Matrix

plotLift(predict_2_tmp[,c(1)],test_data_t21[,c(25)]) #Lift Chart

TopDecileLift(predict_2_tmp[,c(1)],test_data_t21[,c(25)]) #Top Decile

#################################################################
