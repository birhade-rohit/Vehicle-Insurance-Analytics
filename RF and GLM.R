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
data$Claim_Amount=as.factor(data$Claim_Amount)
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



# Random Forest Model
library(randomForest)
set.seed(343)


train= sample(1:nrow(data),0.8*nrow(data))
model = randomForest(C_claim~.-Household_ID-Blind_Submodel-Blind_Model-Blind_Make, data=data,subset=train,mtry=6,importance=T)

model.pred= predict(model,newdata=data[-train,],type = "Class")
model.test = data[-train,"C_claim"]
table(model.pred,model.test)

# Logistic Regression model
glm.fit= glm(C_claim ~ .-Household_ID , data=data, subset= train,family = "binomial")
pred= predict(glm.fit, newdata=data[-train,],type="response")
ifelse(pred<=0.5,0,1)
table(pred,model.test)