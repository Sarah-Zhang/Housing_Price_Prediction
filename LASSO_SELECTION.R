### select best lambda

library(glmnet, quietly = TRUE)
library(ISLR, quietly = TRUE)
library(tidyverse)
library(plyr)
library(magrittr)
library(olsrr)
library(faraway)

rawData <- read.csv('housing.txt')
housing <- rawData
Morty <- read.csv('Morty.txt')

# Condition1: Proximity to various conditions
# Condition2: Proximity to various conditions (if more than one is present)
# Combine 2 conditions into dummy variables
for (level in levels(housing$Condition1)){
  eval(parse(text=paste0('housing$Condition', level, '=0')))
}
for (i in 1:nrow(housing)){
  if (!is.na(housing[i,'Condition1'])){
    eval(parse(text=paste0('housing[i, "Condition', housing[i,'Condition1'], '"]=', 1)))
  }
  if (!is.na(housing[i,'Condition2'])){
    eval(parse(text=paste0('housing[i, "Condition', housing[i,'Condition2'], '"]=', 1)))
  }  
}
housing %<>% dplyr::select(-Condition1, -Condition2)

# fix level names
housing$Exterior2nd = revalue(housing$Exterior2nd, c("Brk Cmn"="BrkComm"))
housing$Exterior2nd = revalue(housing$Exterior2nd, c("Wd Shng"="WdShing"))
# Exterior1st: Exterior covering on house
# Exterior2nd: Exterior covering on house (if more than one material)
housing$Exterior1st = revalue(housing$Exterior1st, c("Wd Sdng"="WdSdng"))
housing$Exterior2nd = revalue(housing$Exterior2nd, c("Wd Sdng"="WdSdng"))

for (level in levels(housing$Exterior1st)){
  eval(parse(text=paste0('housing$Exterior', level, '=0')))
}
for (i in 1:nrow(housing)){
  if (!is.na(housing[i,'Exterior1st'])){
    eval(parse(text=paste0('housing[i, "Exterior', housing[i,'Exterior1st'], '"]=', 1)))
  }
  if (!is.na(housing[i,'Exterior2nd'])){
    eval(parse(text=paste0('housing[i, "Exterior', housing[i,'Exterior2nd'], '"]=', 1)))
  }  
}
housing %<>% dplyr::select(-Exterior1st, -Exterior2nd)

housing$ExteriorCmentBd[is.na(housing$ExteriorCmentBd)] <- 0
housing$ExteriorOther[is.na(housing$ExteriorOther)] <- 0



# Factorize
housing$MSSubClass <- as.factor(housing$MSSubClass)
housing$CentralAir <- housing$CentralAir == 'Y'


# ordinals
housing$LotShape <- ordered(housing$LotShape, c('IR3','IR2','IR1','Reg'))
housing$LandSlope <- ordered(housing$LandSlope, c('Sev','Mod','Gtl'))
housing$Utilities <- ordered(housing$Utilities, c('EL0','NoSewa','NoSewr','AllPub'))
housing$KitchenQual <- ordered(housing$KitchenQual, c('Fa','TA','Gd','Ex'))
housing$Functional <- ordered(housing$Functional, c('Sal','Sev','Maj2','Maj1','Mod','Min2','Min1','Typ'))
housing$PavedDrive <- ordered(housing$PavedDrive, c('N','P','Y'))
housing$HeatingQC <- ordered(housing$HeatingQC, c('Po','Fa','TA','Gd','Ex'))
housing$ExterQual <- ordered(housing$ExterQual, c('Po','Fa','TA','Gd','Ex'))
housing$ExterCond <- ordered(housing$ExterCond, c('Po','Fa','TA','Gd','Ex'))

# ordinals with NAs
housing$BsmtQual <- ordered(housing$BsmtQual, c('Po','Fa','TA','Gd','Ex'))
housing$BsmtCond <- ordered(housing$BsmtCond, c('Po','Fa','TA','Gd','Ex'))
housing$BsmtExposure <- ordered(housing$BsmtExposure, c('No','Mn','Av','Gd'))
housing$BsmtFinType1 <- ordered(housing$BsmtFinType1, c('Unf','Rec','LwQ','GLQ','BLQ','ALQ'))
housing$BsmtFinType2 <- ordered(housing$BsmtFinType2, c('Unf','Rec','LwQ','GLQ','BLQ','ALQ'))
housing$Electrical <- ordered(housing$Electrical, c('Mix','FuseP','FuseF','FuseA','SBrkr'))
housing$FireplaceQu <- ordered(housing$FireplaceQu, c('Po','Fa','TA','Gd','Ex'))
housing$GarageFinish <- ordered(housing$GarageFinish, c('Unf','RFn','Fin'))
housing$GarageQual <- ordered(housing$GarageQual, c('Po','Fa','TA','Gd','Ex'))
housing$GarageCond <- ordered(housing$GarageCond, c('Po','Fa','TA','Gd','Ex'))
housing$PoolQC <- ordered(housing$PoolQC, c('Fa','TA','Gd','Ex'))


# missing values
housing$LotFrontage[is.na(housing$LotFrontage)] <- mean(housing$LotFrontage, na.rm = T)
housing$MasVnrArea[is.na(housing$MasVnrArea)] <- mean(housing$MasVnrArea, na.rm = T)

# 0s that reprensent NAs
housing$X2ndFlrSF[housing$X2ndFlrSF==0] <- mean(housing$X2ndFlrSF[housing$X2ndFlrSF!=0])
housing$BsmtFinSF1[is.na(housing$BsmtFinType1)] <- mean(housing$BsmtFinSF1[!is.na(housing$BsmtFinType1)])
housing$BsmtFinSF2[is.na(housing$BsmtFinType2)] <- mean(housing$BsmtFinSF1[!is.na(housing$BsmtFinType2)])
housing$BsmtUnfSF[is.na(housing$BsmtFinType2)] <- mean(housing$BsmtUnfSF[!is.na(housing$BsmtFinType2)])
housing$TotalBsmtSF[is.na(housing$BsmtFinType2)] <- mean(housing$TotalBsmtSF[!is.na(housing$BsmtFinType2)])
housing$GarageYrBlt[is.na(housing$GarageType)] <- mean(housing$GarageYrBlt[!is.na(housing$GarageType)])
housing$GarageArea[is.na(housing$GarageType)] <- mean(housing$GarageArea[!is.na(housing$GarageType)])
housing$PoolArea[is.na(housing$PoolQC)] <- mean(housing$PoolArea[!is.na(housing$PoolQC)])
housing$MiscVal[is.na(housing$MiscFeature)] <- mean(housing$MiscVal[!is.na(housing$MiscFeature)])

# Add NA level
for (i in 1:ncol(housing)){
  if (is.factor(housing[[i]]) & sum(is.na(housing[[i]])) > 0){
    housing[[i]] = addNA(housing[[i]])
  }
}

housing %<>% mutate(SalePrice = log(SalePrice))
housing %<>% dplyr::select(-Id)
housing %<>% dplyr::select(-GrLivArea, -TotalBsmtSF)
housing %<>% dplyr::select(-OverallQual, -OverallCond)

x = model.matrix( ~ .-1, housing %>% dplyr::select(-SalePrice))
y <- housing$SalePrice

set.seed(123)
bestLambdaList = rep(0,10)
for (i in 1:10){     
  cv.out = cv.glmnet(x, y, alpha = 1, nfolds=5) 
  bestLambdaList[i] = cv.out$lambda.min
}

best.lambda <- mean(bestLambdaList)
best.lambda
final.model <- glmnet(x, y, alpha = 1, lambda = best.lambda) 

Coef.Lasso.All <- coef(final.model)
nonZero.Coef.Total <- sum(Coef.Lasso.All > 0)
nonZero.Coef.Total
nonZero.Coef <- names(Coef.Lasso.All[which(Coef.Lasso.All > 0), ])
subsetHousing <- cbind(dplyr::select(data.frame(x), one_of(nonZero.Coef)), y)

# Anova - F_test

model0 <- lm(y ~ ., data = subsetHousing)
summary(model0)

# HeatingQC.Q         -3.879e-03  2.165e-02  -0.179 0.857786
#As we can see in the summary above, HeatingQC.Q has a very large p-value, which is 0.857786
#we consider remove this parameter from the full model due to lack of significence.
model1 <- update(model0, . ~ . - HeatingQC.Q)
anova(model0, model1)
#As we can see in the summary above, the p-value for anova F-test is 0.8578, 
#which is pretty big. Thus, there is NO evidence showing that HeatingQC.Q has an effect on SalePrice.
summary(model1)

#BsmtExposure.L      3.478e-03  1.148e-02   0.303 0.761986
#As we can see in the summary above, BsmtExposure.L has a very large p-value, which is 0.761986
#we consider remove this parameter from the reduced model due to lack of significence.
model2 <- update(model1, . ~ . - BsmtExposure.L)
anova(model1, model2)
#As we can see in the summary above, the p-value for anova F-test is 0.762, 
#which is pretty big. Thus, there is NO evidence showing that BsmtExposure.L has an effect on SalePrice.
summary(model2)

#GarageArea          1.278e-05  3.250e-05   0.393 0.694228
#As we can see in the summary above, GarageArea has a very large p-value, which is 0.694228
#we consider remove this parameter from the reduced model due to lack of significence.
model3 <- update(model2, . ~ . - GarageArea)
anova(model2, model3)
#As we can see in the summary above, the p-value for anova F-test is 0.6942, 
#which is pretty big. Thus, there is NO evidence showing that GarageArea has an effect on SalePrice.
summary(model3)

#SaleTypeCon         1.053e-01  1.040e-01   1.012 0.311567
#As we can see in the summary above, SaleTypeCon has a pretty large p-value, which is 0.311567
#we consider remove this parameter from the reduced model due to lack of significence.
model4 <- update(model3, . ~ . - SaleTypeCon)
anova(model3, model4)
#As we can see in the summary above, the p-value for anova F-test is 0.3116, 
#which is pretty big. Thus, there is NO evidence showing that SaleTypeCon has an effect on SalePrice.
summary(model4)

#StreetPave          9.624e-02  6.100e-02   1.578 0.114877
#As we can see in the summary above, StreetPave has a large p-value, which is 0.114877
#we consider remove this parameter from the reduced model due to lack of significence.
model5 <- update(model4, . ~ . - StreetPave)
anova(model4, model5)
#As we can see in the summary above, the p-value for anova F-test is 0.1149, 
#which is pretty big. Thus, there is NO evidence showing that StreetPave has an effect on SalePrice.
summary(model5)

#ExterCond.L         5.475e-02  3.559e-02   1.538 0.124152
#As we can see in the summary above, ExterCond.L has a large p-value, which is 0.124152
#we consider remove this parameter from the reduced model due to lack of significence.
model6 <- update(model5, . ~ . - ExterCond.L)
anova(model5, model6)
#As we can see in the summary above, the p-value for anova F-test is 0.1242, 
#which is pretty big. Thus, there is NO evidence showing that ExterCond.L has an effect on SalePrice.
summary(model6)

#MSZoningFV          6.892e-02  3.859e-02   1.786 0.074353 .
#As we can see in the summary above, MSZoningFV has a large p-value, which is 0.074353
#we consider remove this parameter from the reduced model due to lack of significence.
model7 <- update(model6, . ~ . - MSZoningFV)
anova(model6, model7)
#As we can see in the summary above, the p-value for anova F-test is 0.07435, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that MSZoningFV has an effect on SalePrice.
summary(model7)

#MasVnrArea          4.602e-05  2.553e-05   1.803 0.071666 .
#As we can see in the summary above, MasVnrArea has a large p-value, which is 0.071666
#we consider remove this parameter from the reduced model due to lack of significence.
model8 <- update(model7, . ~ . - MasVnrArea)
anova(model7, model8)
#As we can see in the summary above, the p-value for anova F-test is 0.07167, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that MasVnrArea has an effect on SalePrice.
summary(model8)

#NeighborhoodVeenker 8.320e-02  4.450e-02   1.870 0.061733 .
#As we can see in the summary above, NeighborhoodVeenker has a large p-value, which is 0.061733
#we consider remove this parameter from the reduced model due to lack of significence.
model9 <- update(model8, . ~ . - NeighborhoodVeenker)
anova(model8, model9)
#As we can see in the summary above, the p-value for anova F-test is 0.06173, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that NeighborhoodVeenker has an effect on SalePrice.
summary(model9)

#OpenPorchSF         1.216e-04  6.350e-05   1.915 0.055690 .
#As we can see in the summary above, OpenPorchSF has a large p-value, which is 0.055690
#we consider remove this parameter from the reduced model due to lack of significence.
model10 <- update(model9, . ~ . - OpenPorchSF)
anova(model9, model10)
#As we can see in the summary above, the p-value for anova F-test is 0.05569, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that OpenPorchSF has an effect on SalePrice.
summary(model10)
coef.p.big <- summary(model10)$coefficients[summary(model10)$coefficients[,4]>0.01,]
coef.p.big

#HeatingGasW         7.993e-02  3.675e-02   2.175 0.029813 * 
#As we can see in the summary above, HeatingGasW has a relatively large p-value, which is 0.029813
#we consider remove this parameter from the reduced model due to lack of significence.
model11 <- update(model10, . ~ . - HeatingGasW)
anova(model10, model11)
#As we can see in the summary above, the p-value for anova F-test is 0.02981, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that HeatingGasW has an effect on SalePrice.
summary(model11)

#RoofMatlMembran     3.427e-01  1.474e-01   2.324 0.020266 * 
#As we can see in the summary above, RoofMatlMembran has a relatively large p-value, which is 0.02027
#we consider remove this parameter from the reduced model due to lack of significence.
model12 <- update(model11, . ~ . - RoofMatlMembran)
anova(model11, model12)
#As we can see in the summary above, the p-value for anova F-test is 0.02981, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that RoofMatlMembran has an effect on SalePrice.
summary(model12)

#KitchenQual.Q       3.665e-02  1.505e-02   2.435 0.015028 *
#As we can see in the summary above, KitchenQual.Q has a relatively large p-value, which is 0.029813
#we consider remove this parameter from the reduced model due to lack of significence.
model13 <- update(model12, . ~ . - KitchenQual.Q)
anova(model12, model13)
#As we can see in the summary above, the p-value for anova F-test is 0.01503, 
#which is smaller than 0.01 significent level. Thus, there is NO evidence showing that KitchenQual.Q has an effect on SalePrice.
summary(model13)


# As for now, we believe that the "best" model is model13, the one without parameters KitchenQual.Q, RoofMatlMembran, 
# HeatingGasW, OpenPorchSF, NeighborhoodVeenker, ExterCond.L, StreetPave, MSZoningFV, MasVnrArea, SaleTypeCon, GarageArea, 
# BsmtExposure.L, HeatingQC.Q. We are going to use an alternative approach to find the "best" model and hopefully we can the same "best" model.

# Alternative - BIC 

BIC.Stepwise = step(model0,scope = ~MSSubClass50 + MSSubClass60 + MSSubClass70 + 
     MSZoningFV + MSZoningRL + LotArea + StreetPave + LandContourHLS + 
     LotConfigCulDSac + NeighborhoodBrkSide + NeighborhoodClearCr + 
     NeighborhoodCrawfor + NeighborhoodNoRidge + NeighborhoodNridgHt + 
     NeighborhoodSomerst + NeighborhoodStoneBr + NeighborhoodVeenker + 
     YearRemodAdd + RoofMatlMembran + RoofMatlWdShngl + MasVnrArea + 
     ExterQual.L + ExterCond.L + FoundationPConc + BsmtExposure.L + 
     BsmtFinSF1 + HeatingGasW + HeatingQC.L + HeatingQC.Q + CentralAirTRUE + 
     X1stFlrSF + X2ndFlrSF + BsmtFullBath + FullBath + HalfBath + 
     KitchenQual.L + KitchenQual.Q + TotRmsAbvGrd + Functional.L + 
     Fireplaces + GarageTypeAttchd + GarageCars + GarageArea + 
     PavedDrive.L + WoodDeckSF + OpenPorchSF + ScreenPorch + PoolArea + 
     SaleTypeCon + SaleTypeNew + SaleConditionNormal + ConditionNorm + 
     ExteriorBrkFace, direction = "both", k = log(1460))
# As we can see from the result above, the model with the minimum AIC value, AIC=-5422.88, is the one without 
# parameter KitchenQual.Q, RoofMatlMembran, HeatingGasW, OpenPorchSF, NeighborhoodVeenker, ExterCond.L, 
# StreetPave, MSZoningFV, MasVnrArea, SaleTypeCon, GarageArea, BsmtExposure.L, HeatingQC.Q, which is the same as "model13" in the anova F-test part.

subsetHousing %<>% dplyr::select(-OpenPorchSF, -NeighborhoodVeenker, -MasVnrArea, -MSZoningFV, -ExterCond.L, -StreetPave, -SaleTypeCon, -GarageArea, 
                                 -BsmtExposure.L, -HeatingQC.Q, -KitchenQual.Q, -HeatingGasW, -RoofMatlMembran)

best.Model <- model13
summary(best.Model)


# Model Diagnosis
# Nonnormality
# calculate the standardized residuals
e <- best.Model$residuals
std.residuals <- (e - mean(e)) / sd(e)
# plot the qqplot of the residuals
# qqnorm(std.residuals)
# qqline(std.residuals, col= 'blue')
#As we can see above in the Normal Q-Q Plot, the residuals fit the diagnosis line pretty well except for the tails.
residual.List = 0
for (i in 1:100){     
  residual.List = residual.List + ks.test(std.residuals, rnorm(1000))[2][[1]]
}
residual.List/100
#As we can see above in the Kolmogorov-Smirnov test, the p-value is 7.968e-05, which is very small, 
#then we reject the null hypothesis that the residuals are normally distributed.
#Thus, we conclude that even though the residuals looks normally distributed, 
#we fail to meet our assumptions according to the Kolmogorov-Smirnov test.


#Outlier removal
#ols_dffits_plot(best.Model)
dffits_bs <- abs(dffits(best.Model))
dffits_bs[is.na(dffits(best.Model))] <- 10
x_new <- subsetHousing %>% dplyr::select(-c(y))
dffits_data_bs <- subsetHousing[dffits_bs < 2 * sqrt(ncol(x_new)/nrow(x_new)), ]

model_new <- lm(y ~ ., data = dffits_data_bs)
summary(model_new)

#ANOVA F-test
#NeighborhoodClearCr 4.555e-02  2.542e-02   1.792 0.073407 . 
#As we can see in the summary above, neighborhoodClearCr has a relatively large p-value, which is 0.073407
#we consider remove this parameter from the reduced model due to lack of significence.
model_new1 <- update(model_new, . ~ . - NeighborhoodClearCr)
anova(model_new, model_new1)
#As we can see in the summary above, the p-value for anova F-test is 0.07341, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that NeighborhoodClearCr has an effect on SalePrice.
summary(model_new1)

#RoofMatlWdShngl     1.494e-01  7.429e-02   2.011 0.044556 *
#As we can see in the summary above, RoofMatlWdShngl has a relatively large p-value, which is 0.073407
#we consider remove this parameter from the reduced model due to lack of significence.
model_new2 <- update(model_new1, . ~ . - RoofMatlWdShngl)
anova(model_new1, model_new2)
#As we can see in the summary above, the p-value for anova F-test is 0.04456, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that RoofMatlWdShngl has an effect on SalePrice.
summary(model_new2)

#PoolArea            4.630e-03  1.838e-03   2.519 0.011894 *
#As we can see in the summary above, PoolArea has a relatively large p-value, which is 0.011894
#we consider remove this parameter from the reduced model due to lack of significence.
model_new3 <- update(model_new2, . ~ . - PoolArea)
anova(model_new2, model_new3)
#As we can see in the summary above, the p-value for anova F-test is 0.01189, 
#which is bigger than 0.01 significent level. Thus, there is NO evidence showing that PoolArea has an effect on SalePrice.
summary(model_new3)

# BIC
BIC.Stepwise.New = step(model_new,scope = ~MSSubClass50 + MSSubClass60 + MSSubClass70 + 
                          MSZoningRL + LotArea + LandContourHLS + LotConfigCulDSac + 
                          NeighborhoodBrkSide + NeighborhoodCrawfor + NeighborhoodNoRidge + 
                          NeighborhoodNridgHt + NeighborhoodSomerst + NeighborhoodStoneBr + 
                          YearRemodAdd + ExterQual.L + FoundationPConc + BsmtFinSF1 + 
                          HeatingQC.L + CentralAirTRUE + X1stFlrSF + X2ndFlrSF + BsmtFullBath + 
                          FullBath + HalfBath + KitchenQual.L + TotRmsAbvGrd + Functional.L + 
                          Fireplaces + GarageTypeAttchd + GarageCars + PavedDrive.L + 
                          WoodDeckSF + ScreenPorch + SaleTypeNew + SaleConditionNormal + 
                          ConditionNorm + ExteriorBrkFace + NeighborhoodClearCr + RoofMatlWdShngl +
                          PoolArea, direction = "both", k = log(1460))

#Same result as ANOVA F-test
best.model.new <- model_new3
summary(best.model.new)
dffits_data_bs %<>% dplyr::select(-PoolArea, -RoofMatlWdShngl, -NeighborhoodClearCr)

# Nomality Test
e.new <- best.model.new$residuals
std.residuals.new <- (e.new - mean(e.new)) / sd(e.new)
# plot the qqplot of the residuals
# qqnorm(std.residuals.new)
# qqline(std.residuals.new, col= 'blue')

#ks.test
residual.List.new = 0
for (i in 1:100){     
  residual.List.new = residual.List.new + ks.test(std.residuals.new, rnorm(1000))[2][[1]]
}
residual.List.new/100



# VIF
#standardize data
X = scale(model.matrix( ~ .-1, dffits_data_bs %>% dplyr::select(-y)))
lm <- lm(dffits_data_bs$y ~ X)
#vif check collinearity
faraway::vif(lm)
# pass. no collinearity


# Nonlinearity
# plot(x = best.Model$fitted.values, y = std.residuals)
# abline(h=0)
# abline(h=-4, col='red')
# par(mfrow = c(2,2))
# plot(best.model.new)



# Du Shen's plots
# plot(x=best.model.new$fitted.values, y=best.model.new$residuals, main = 'Residuals Plot',
#      xlab='Fitted Values', ylab='Residuals')

 semistudentres = resid(best.model.new)/sqrt(mean(best.model.new$residuals^2))
# plot(x=best.model.new$fitted.values, y=semistudentres^2, main='Squared Residuals Plot',
#      xlab='Fitted Values', ylab='Residuals^2')

# Du Shen's Comment
# Non-linearity: The residuals plot shows the residuals have no specific trend versus fitted values, indicating that the linear association may be appropriate
# Nonconstancy: The squared residuals plot shows that the deviation of the residuals does not increase as the fitted values increase, indicating no nonconstancy of error variance.

