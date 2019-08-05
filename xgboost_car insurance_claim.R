data = read.csv(file.choose())
View(data)
data=data[,-1]
class(data)
data[data == "?"] = NA
View(data)
is.na(data)
any(is.na(data))
sum(is.na(data))
colSums(is.na(data))
str(data)
summary(data)
# ommitin all missing values
#a=na.omit(data)
#a
#dim(a)
#rm(a)
install.packages("DMwR")
# replacing the missing values with mean
library(DMwR)

install.packages("Hmisc")
library(Hmisc)   # replacing the missing values with mode

data$Cat1=impute(data$Cat1 ,mode)
data$Cat2=impute(data$Cat2 ,mode)   
data$Cat3=impute(data$Cat3 ,mode)
data$Cat4=impute(data$Cat4 ,mode)
data$Cat5=impute(data$Cat5 ,mode)
data$Cat6=impute(data$Cat6 ,mode)
data$Cat7=impute(data$Cat7 ,mode)
data$Cat8=impute(data$Cat8 ,mode)
data$Cat10=impute(data$Cat10 ,mode)
data$Cat11=impute(data$Cat11 ,mode)

data$OrdCat=impute(data$OrdCat,mode)
colSums(is.na(data)) 



na.omit(data$Blind_Make)
na.omit(data$Blind_Model)
na.omit(data$Blind_Submodel)

vec=which(complete.cases(data))
vec
vec1=which(!complete.cases(data))
vec1
data=data[-vec1,]
View(data)
summary(data)
str(data)
any(is.na(data))

data$Claim_Amount=ifelse(data$Claim_Amount==0,0,1)
data$Claim_Amount

summary(data)
str(data)

# To check the balance of the dataset 
data$Claim_Amount=as.numeric(data$Claim_Amount)
data$Vehicle=as.factor(data$Vehicle)
data$Calendar_Year=as.factor(data$Calendar_Year)
data$Model_Year=as.factor(data$Model_Year)
data$Blind_Make=as.factor(data$Blind_Make)
data$Blind_Model=as.factor(data$Blind_Model)
data$Blind_Submodel=as.factor(data$Blind_Submodel)
data$Cat1=as.factor(data$Cat1)
data$Cat2=as.factor(data$Cat2)
data$Cat3=as.factor(data$Cat3)
data$Cat4=as.factor(data$Cat4)
data$Cat5=as.factor(data$Cat5)
data$Cat6=as.factor(data$Cat6)
data$Cat7=as.factor(data$Cat7)
data$Cat8=as.factor(data$Cat8)
data$Cat9=as.factor(data$Cat9)
data$Cat10=as.factor(data$Cat10)
data$Cat11=as.factor(data$Cat11)
data$Cat12=as.factor(data$Cat12)
data$OrdCat=as.factor(data$OrdCat)
data$NVCat=as.factor(data$NVCat)
data$Household_ID=as.factor(data$Household_ID)

str(data)


data$C_claim=data$Claim_Amount
View(data)

data$Claim_Amount=NULL
View(data)

table(data$C_claim)
prop.table(table(data$C_claim))

#install.packages("dplyr")
#library(dplyr)
#filtered_data=filter(data,data$C_claim==1)
#filtered_data$Claim_Amount


which(data$C_claim==1) #It prints record numbers are having 1
data$C_claim[which(data$C_claim==1)]  #It prints record are having 1


# Checking outliers
boxplot(data$Var1)     # 1 2 4 5 6 8
summary(data$Var1)
bench1= 0.4429 +1.5*( 0.4429-(-0.6900))
bench1
data$Var1[data$Var1>bench1]=bench1
boxplot(data$Var1) 
summary(data$Var1)


boxplot(data$Var2)     
summary(data$Var2)
bench2= 0.3942+1.5*IQR(data$Var2)
bench2
data$Var2[data$Var2>bench2]=bench2
boxplot(data$Var2) 
summary(data$Var2)

boxplot(data$Var4)     
summary(data$Var4)
bench4= 0.4009+1.5*IQR(data$Var4)
bench4
data$Var4[data$Var4>bench4]=bench4
boxplot(data$Var4) 
summary(data$Var4)

boxplot(data$Var5)    
summary(data$Var5)
bench5= 0.52719+1.5*IQR(data$Var1)
bench5
data$Var5[data$Var5>bench5]=bench5
boxplot(data$Var5)
summary(data$Var5)


boxplot(data$Var6)
summary(data$Var6)
bench6= 0.4715+1.5*IQR(data$Var1)
bench6
data$Var6[data$Var6>bench6]=bench6
boxplot(data$Var1) 
summary(data$Var1)

boxplot(data$Var8)  
summary(data$Var8)
bench8= 0.2953+1.5*IQR(data$Var8)
bench8
data$Var8[data$Var8>bench8]=bench8
boxplot(data$Var8) 
summary(data$Var8)


boxplot(data$NVVar4)
boxplot(data$NVVar4)
summary(data$NVVar4)
bench9= -0.2661+5*IQR(data$NVVar4)
bench9
data$NVVar4[data$NVVar4>bench9]=bench9
boxplot(data$NVVar4) 
summary(data$NVVar4)

boxplot(data$NVVar3)
summary(data$NVVar3)
bench10= -0.2723+5*IQR(data$NVVar4)
bench10
data$NVVar3[data$NVVar3>bench10]=bench10
boxplot(data$NVVar3) 
summary(data$NVVar3)

boxplot(data$NVVar2)
summary(data$NVVar2)
bench11= -0.2661+5*IQR(data$NVVar2)
bench11
data$NVVar2[data$NVVar2>bench11]=bench11
boxplot(data$NVVar2) 
summary(data$NVVar2)

boxplot(data$NVVar1)
summary(data$NVVar1)
bench12= -0.2315+5*IQR(data$NVVar1)
bench12
data$NVVar1[data$NVVar1>bench12]=bench12
boxplot(data$NVVar1) 
summary(data$NVVar1)


str(data)




#Packages
install.packages("xgboost")
install.packages("magrittr")   #for pipe line
install.packages("dplyr")
install.packages("Matrix")
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)


#divide data
set.seed(123)       # Set seed for reproducible results


idx = sample(nrow(data), nrow(data) * 0.7)    #create an index
train = data[ idx,]              #create training set
test = data[ -idx,]              #creating testing set


#create matrix and one-hot encoding(dummy variables creation)
#one hot encoding creates dummy variables for factor variables to convert data into numeric format
trainm=sparse.model.matrix(C_claim~.,data=train)
head(trainm)
train_label=train[,"C_claim"] 
train_label
train_matrix=xgb.DMatrix(data=as.matrix(trainm),
                         label=train_label)
train_matrix

testm=sparse.model.matrix(C_claim~.,data=test)
head(testm)
head(test)
test_label=test[,"C_claim"]
test_label
test_matrix=xgb.DMatrix(data=as.matrix(testm),
                        label=test_label)
test_matrix

#parameters
#numberof classes=nc
nc=length(unique(train_label))
nc
xgb_parameters=list("objective"="multi:softprob",
                    "eval_metric"="mlogloss",
                    "num_class"=nc)
xgb_parameters

watchlist=list(train=train_matrix,test=test_matrix)
watchlist

#extreme gradient boosting model
#nround is number of iterations
#train_matrix is training dataset

model1=xgb.train(params = xgb_parameters,
                 data=train_matrix,
                 nrounds = 100,
                 watchlist = watchlist,eta=0.3)


#it prints error in training and error intesting for 100 iteratios
#eta is learning rate generally b/w 0 to 1 but default is 0.3
#if eta value is high then chance of overfitting
#error will be reduced by 0.1,0.2 like that by reducing over fitting


model1

#Error plot
#train and test error plot
e=data.frame(model1$evaluation_log)
e
plot(e$iter,e$train_mlogloss,col="blue")  #e$train_mlogloss=training related error
lines(e$iter,e$test_mlogloss,col="red")
min(e$test_mlogloss)     #minimum error in test
e[e$test_mlogloss==0.149591,]  #which iteration has that error(min error)


model2=xgb.train(params = xgb_parameters,
                 data=train_matrix,
                 nrounds = 55,
                 watchlist = watchlist,
                 eta=0.1)
# if nrounds is 55 ,model gives min error for test data

#feature importance
imp=xgb.importance(colnames(train_matrix),model2)
imp     #gain improvement in accurancy by a feature to the branches it is on
xgb.plot.importance(imp)

#prediction and confusion matrix test data
predict=predict(model2,newdata = test_matrix)
head(predict)
#in predict 1st 2 values for 1st record
#sum of 1st 2 values is 1 ,sum of 3rd and 4th is 1 ..
head(test$C_claim)
length(predict)
pred=matrix(predict,nrow=nc,
            ncol=length(predict)/nc) %>%
  t() %>% data.frame() %>% 
  mutate(label=test_label,
         max_prob=max.col(.,"last")-1)

head(pred)
pred
#if x1 col has high prob thrn prints 0
#else prints 1

#confusion matrix
install.packages("caret")
library(caret)

confusionMatrix(table(prediction=pred$max_prob,
                      Actual=pred$label))



#max.depth=6 default,but it can take 1 to infinete
# gamma=0 default but from 0 to infinete,
#for larger values of gamma more conservative algorithm
#ie: to avoid over fitting use small values of gamma
#subsample=1(ie :100%) default but from 0 to 1
#and lower value of subsample avoids overfitting
#subsample=0.5(used half of instances to grow the trees)
#is better than 1
#colsample_bytree=1 should be telling each tree to 
#sample from 100% of the columns/features when building a tree.
#to reduce overfitting increase gamma ,
#ie to decrease gap between red line and blue line give small gamma value





