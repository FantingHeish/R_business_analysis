
library("ggplot2") # a data visualization package
library("xtable") # processing of regression output
library("knitr") # used for report compilation and table display
library("caret") # confusion matrix
library ("pROC") # confusion matrix ROC Curve
RFMdata <- read.csv(file = "3_sustainable_consumption.csv")
head(RFMdata,5)
summary(RFMdata)
model1 <- glm(purchaselabel~gender_rev+agegroup+GDP+BMI+trustngo+children+education_rev+trustngo, data=RFMdata, family = "binomial")
summary(model1)
exp(coef(model1))
model2 <- glm(purchaselabel~ 1, data=RFMdata, family = "binomial")
anova(model1, model2, test = "Chisq")#comparing 2 models

#--------------------

RFMdata$Base.Probability <- predict(model1, RFMdata, type="response")
RFMdata$Predicted.Purchase <- 1*(RFMdata$Base.Probability>=0.5)
library(caret)
confusionMatrix(table(RFMdata$Predicted.Purchase,RFMdata$purchaselabel),positive = "1")
myroc <- roc(RFMdata$purchaselabel, RFMdata$Base.Probability)
plot(myroc,legacy.axes=TRUE)
text(0.5, 0.8, labels = sprintf("AUC = %.5f",myroc$auc))

