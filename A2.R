Train <- read.csv(file="em_save_train.csv", header=TRUE, sep=",")
Train
Validate <- read.csv(file="em_save_validate.csv", header=TRUE, sep=",")
validate
Validate

#Intercept,  compactness_se  ,concavity_worst ,
#radius_se,  radius_worst,  smoothness_worst,  texture_worst.
Breast_Cancer <- read.csv(file = "Breast Cancer.csv", header=TRUE, sep=",")
library(MASS)
names(Smarket)
library(ISLR)
names(Smarket)
names(Breast_Cancer)
names(Train)
names(Validate)
attach(Breast_Cancer)
lda.fit=lda(diagnosis~compactness_se, concavity_worst, radius_se, radius_worst, smoothness_worst + texture_worst, data = Validate, subset = Train)
glm.fit=glm(diagnosis~compactness_se, concavity_worst, radius_se, radius_worst, smoothness_worst + texture_worst,
            data=Validate, family=binomial)
names(Validate)
lda.fit=lda(diagnosis~compactness_se, radius_se,texture_worst,data= Breast_Cancer, family=binomial)
names(Breast_Cancer)
head(Breast_Cancer)
dimnames(Breast_Cancer)

Breast_Cancer.lda <- lda(diagnosis ~ ., data=Breast_Cancer)
Breast_Cancer.lda
lda.fit=lda(diagnosis~.,data=Validate)
Validate
lda.fit
lda.pred=predict (lda.fit , Validate)
names(lda.pred)
lda.class =lda.pred$class
table(lda.class,diagnosis.B)
lda.class
table(lda.class)
mean(lda.class == diagnosis.B)
lda.fit2=lda(diagnosis~.,data=Train)
lda.pred2=predict (lda.fit , Train)
lda.fit2
lda.pred2
lda.class2 =lda.pred2$class
lda.class2
table(lda.class2)
