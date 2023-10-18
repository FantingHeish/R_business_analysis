library("xtable") # processing of regression output
library("knitr") # used for report compilation and table display
library("ggplot2") # very popular plotting library ggplot2
library("mlogit") # multinomial logit
library("caret") # ConfusionMatrix


cbc.df<-read.csv("5_conjoint.csv", stringsAsFactors = TRUE)
str(cbc.df)
head(cbc.df)
summary(cbc.df)

# Prepare the data
# 2.1 Define reference levels - used when estimating model

library(mlogit)
cbc.df$Origin <- relevel(cbc.df$Origin, ref = "Peru")
cbc.df$Manufacture <- relevel(cbc.df$Manufacture, ref = "UnderDeveloped") 
cbc.df$Energy <- relevel(cbc.df$Energy, ref = "Low") 
cbc.df$Nuts <- relevel(cbc.df$Nuts, ref = "No") 
cbc.df$Tokens <- relevel(cbc.df$Tokens, ref = "No")
cbc.df$Organic <- relevel(cbc.df$Organic, ref = "No")
cbc.df$Premium <- relevel(cbc.df$Premium, ref = "No")
cbc.df$Fairtrade <- relevel(cbc.df$Fairtrade , ref = "No")
cbc.df$Sugar <- relevel(cbc.df$Sugar, ref = "Low")


## 2.2 Define data format
library(dfidx)
cbc.mlogit <- dfidx(cbc.df, choice="Choice",
                    idx=list(c("Choice_id", "Consumer_id"), "Alternative"))


#-------------------

# 3 Multinomial conjoint model estimation with mlogit()
library("knitr")
model<-mlogit(Choice ~ 0+Origin+Manufacture+Energy+Nuts+Tokens+Organic+Premium+Fairtrade+Sugar+Price, data=cbc.mlogit) 
kable(summary(model)$CoefTable)


# 3.2 Model fit
# preference is only driven by the Origin effect
model.constraintOrigin <-mlogit(Choice ~ 0+Origin, data = cbc.mlogit)
# kable(summary(model.constraintOrigin)$CoefTable)
lrtest(model, model.constraintOrigin)

# Manufacture
model.constraintManufacture <-mlogit(Choice ~ 0+Manufacture, data = cbc.mlogit)
# kable(summary(model.constraintManufacture)$CoefTable)
lrtest(model, model.constraintManufacture)

# Energy
model.constraintEnergy <-mlogit(Choice ~ 0+Energy, data = cbc.mlogit)
# kable(summary(model.constraintEnergy)$CoefTable)
lrtest(model, model.constraintEnergy)

# Nuts
model.constraintNuts <-mlogit(Choice ~ 0+Nuts, data = cbc.mlogit)
#kable(summary(model.constraintNuts)$CoefTable)
lrtest(model, model.constraintNuts)

# Tokens
model.constraintTokens <-mlogit(Choice ~ 0+Tokens, data = cbc.mlogit)
# kable(summary(model.constraintTokens)$CoefTable)
lrtest(model, model.constraintTokens)

# Organic
model.constraintOrganic <-mlogit(Choice ~ 0+Organic, data = cbc.mlogit)
# kable(summary(model.constraintOrganic)$CoefTable)
lrtest(model, model.constraintOrganic)

# Premium
model.constraintPremium <-mlogit(Choice ~ 0+Premium, data = cbc.mlogit)
# kable(summary(model.constraintPremium)$CoefTable)
lrtest(model, model.constraintPremium)

# Fairtrade
model.constraintFairtrade <-mlogit(Choice ~ 0+Fairtrade, data = cbc.mlogit)
# kable(summary(model.constraintFairtrade)$CoefTable)
lrtest(model, model.constraintFairtrade)

# Sugar
model.constraintSugar <-mlogit(Choice ~ 0+Sugar, data = cbc.mlogit)
# kable(summary(model.constraintSugar)$CoefTable)
lrtest(model, model.constraintSugar)


#--------------------

# 4 Interpreting Conjoint Analysis Findings

## 4.1 Predicted Market Share
kable(head(predict(model,cbc.mlogit)))
predicted_alternative <- apply(predict(model,cbc.mlogit),1,which.max) 
selected_alternative <- cbc.mlogit$Alternative[cbc.mlogit$Choice>0] 
confusionMatrix(table(predicted_alternative,selected_alternative),positive = "1")

## 4.3 Willingness to pay
  # What is the _____ value of A relative to B ?
# Origin: the country of origin of the cocoa, Peru/Venezuela/Ecuador
(coef(model)["OriginVenezuela"]-coef(model)["OriginEcuador"]) / (-coef(model)["Price"])

# Manufacture: Manufacturing locations, Under developed Countries/Developing Coutries
(coef(model)["ManufactureDeveloped"]-coef(model)["ManufactureDeveloping"]) / (-coef(model)["Price"])

# Nuts: Presence of Nuts and Fruits
(coef(model)["NutsNuts and Fruit"]-coef(model)["NutsNuts only"]) / (-coef(model)["Price"])

#TokensDonate 
(coef(model)["TokensDonate"]-coef(model)["TokensKeep & Use"]) / (-coef(model)["Price"])

  #----------
  #  Willingness to Pay for an Attribute Upgrade
# Energy
coef(model)["EnergyHigh"] /(-coef(model)["Price"])

# Organic
coef(model)["OrganicYes"] /(-coef(model)["Price"])

# PremiumYes 
coef(model)["PremiumYes"] /(-coef(model)["Price"])

# Fairtrade: Faire trade certified, yes/no
coef(model)["FairtradeYes"] /(-coef(model)["Price"])

# Sugar, low/high
coef(model)["SugarHigh"] /(-coef(model)["Price"])


