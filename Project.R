# Data Exploration By Table
install.packages("glmnet")
library(glmnet)
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
heart.data$num[heart.data$num > 0] <- 1
t1 = table(heart.data$num)
barplot(t1)

chclass = c("character","character","character","numeric","numeric","character","character","numeric","character","numeric","character","character","character","numeric")
for (i in 1:14) {
  heart.data[,i] = as(heart.data[,i],Class=chclass[i])
}

for(i in 1:13){
  t<-tapply(heart.data$num, heart.data[,i], FUN = mean)
  if(class(heart.data[,i])=="character"){
    barplot(t,xlab = names(heart.data)[i],ylab = "Prob of Disease")
  }else{
    plot(t,xlab = names(heart.data)[i],ylab = "Prob of Disease")
  }
}

# ---------------------------------------------------------------------------------
# Data Exploration By Clusters
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
heart.data$num[heart.data$num > 0] <- 1

hc.heart.complete = hclust(dist(heart.data), method = "complete")
plot(hc.heart.complete)
hc.out.1 = cutree(hc.heart.complete, 4)

plot(heart.data$age, heart.data$trestbps, col = hc.out.1)
plot(heart.data$age, heart.data$chol, col = hc.out.1)
plot(heart.data$age, heart.data$thalach, col = hc.out.1)
plot(heart.data$age, heart.data$oldpeak, col = hc.out.1)

plot(heart.data$trestbps, heart.data$chol, col = hc.out.1)
plot(heart.data$trestbps, heart.data$thalach, col = hc.out.1)
plot(heart.data$trestbps, heart.data$oldpeak, col = hc.out.1)

plot(heart.data$chol, heart.data$thalach, col = hc.out.1)
plot(heart.data$chol, heart.data$oldpeak, col = hc.out.1)

plot(heart.data$thalach, heart.data$oldpeak, col = hc.out.1)

# ---------------------------------------------------------------------------------
# Data Exploration BY Plot(Relationship Between Trestbps Other Variables)

heart.data = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", header = FALSE, sep = ",", na.strings = '?')
names(heart.data) = c("age","sex","cp","trestbps","chol","fbs","Restecg","thalach","exang","oldpeak","slope","ca","thal","num")

chclass = c("numeric","character","character","numeric","numeric","character","character","numeric","character","numeric","character","character","character","character")
for (i in 1:14) {
  heart.data[,i] = as(heart.data[,i],Class=chclass[i])
}

heart.data$num[heart.data$num > 0] = 1

plot(heart.data$trestbps ~ heart.data$age)
plot(heart.data$trestbps ~ heart.data$sex)
plot(heart.data$trestbps ~ heart.data$cp)
plot(heart.data$trestbps ~ heart.data$chol)
plot(heart.data$trestbps ~ heart.data$fbs)
plot(heart.data$trestbps ~ heart.data$Restecg)
plot(heart.data$trestbps ~ heart.data$thalach)
plot(heart.data$trestbps ~ heart.data$exang)
plot(heart.data$trestbps ~ heart.data$oldpeak)
plot(heart.data$trestbps ~ heart.data$slope)
plot(heart.data$trestbps ~ heart.data$ca)
plot(heart.data$trestbps ~ heart.data$thal)
plot(heart.data$trestbps ~ heart.data$num)

# ---------------------------------------------------------------------------------
# The Relationship Between Trestbps With Other Variables
library(glmnet)
heart.data = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", header = FALSE, sep = ",", na.strings = '?')
names(heart.data) = c("age","sex","cp","trestbps","chol","fbs","Restecg","thalach","exang","oldpeak","slope","ca","thal","num")

chclass = c("numeric","character","character","numeric","numeric","character","character","numeric","character","numeric","character","character","character","character")
for (i in 1:14) {
  heart.data[,i] = as(heart.data[,i],Class=chclass[i])
}

heart.data$num[heart.data$num > 0] = 1
heart.data = na.omit(heart.data)
sum(is.na(heart.data))

x = model.matrix(trestbps ~., data = heart.data)[,-1]
y = heart.data$trestbps

set.seed(1)
train = sample(nrow(x), nrow(x)/2)
test = -(train)
x.train = x[train,]
y.train = y[train]
x.test = x[test,]
y.test = y[test]

set.seed(1)
cv.out = cv.glmnet(x, y, alpha = 1)
plot(cv.out)
bestlambda = cv.out$lambda.min

lasso.final = glmnet(x,y, alpha = 1, lambda = bestlambda)
coef(lasso.final)

lasso.predict = predict(lasso.final, s = bestlambda, newx = x.test)
mean((lasso.predict - y.test)^2)

# ---------------------------------------------------------------------------------
# Method 1 Logistic Regression (Estimation Method: Maximum Likelihood Method)
heart.data = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", header = FALSE, sep = ",", na.strings = '?')
names(heart.data) = c("age","sex","cp","trestbps","chol","fbs","Restecg","thalach","exang","oldpeak","slope","ca","thal","num")
heart.data$num[heart.data$num > 0] = 1
heart.data = na.omit(heart.data)
sum(is.na(heart.data))

chclass = c("numeric","character","character","numeric","numeric","character","character","numeric","character","numeric","character","character","character","character")
for (i in 1:14) {
  heart.data[,i] = as(heart.data[,i],Class=chclass[i])
}

heart.data$num<-as.factor(heart.data$num)
contrasts(heart.data$num)

logreg.mod = glm(num ~., data = heart.data, family = binomial)
summary(logreg.mod)

logreg.final = glm(num ~ sex + cp + thalach + exang + ca + thal, data = heart.data, family = binomial)
summary(logreg.final)

set.seed(1)
train = sample(nrow(heart.data), nrow(heart.data)/2)
test = -(train)
heart.data.train = heart.data[train,]
heart.data.train.output = heart.data$num[train]
heart.data.test = heart.data[test,]
heart.data.test.output = heart.data$num[test]

logreg.train = glm(num ~ sex + cp + thalach + exang + ca + thal, data = heart.data.train, family = binomial)
logreg.prob = predict(logreg.train, newdata = heart.data.test, type = "response")
logreg.pre = rep(0, length(heart.data.test.output))
logreg.pre[logreg.prob > 0.5] = 1
mean(logreg.pre == heart.data.test.output)
summary(logreg.final)

# ---------------------------------------------------------------------------------
# Method 2 Logistic Regression (Estimation Method: relationship between independent variables)
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
heart.data$num[heart.data$num > 0] <- 1
chclass <-c("numeric","character","character","numeric","numeric","character","character","numeric","character","numeric","character","character","character","character")
for (i in 1:14) {
  heart.data[,i]<-as(heart.data[,i],Class=chclass[i])
}
heart.data<-na.omit(heart.data)
library(glmnet)
set.seed(1)
train<-sample(nrow(heart.data),nrow(heart.data)/2)
test<-(-train)
x<-heart.data[,-14]
y<-heart.data$num
x<-model.matrix(~.,data=x)[,-1]
y<-as.factor(y)
x.train<-x[train,]
x.test<-x[test,]
y.train<-y[train]
y.test<-y[test]
lasso.1<-cv.glmnet(x.train,y.train,alpha=1,family="binomial",nfolds = 20)
plot(lasso.1)
lasso.1$lambda.min
lasso.1<-glmnet(x.train,y.train,alpha=1,family="binomial",lambda = lasso.1$lambda.min)
p.pre<-predict(lasso.1,newx = x.test,type="response")
y.pre<-rep(0,nrow(p.pre))
y.pre[p.pre>0.5]<-1
mean(y.pre==y.test)
coef(lasso.1)








