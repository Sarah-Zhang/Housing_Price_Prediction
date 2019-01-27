library(glmnet, quietly = TRUE)
library(ISLR, quietly = TRUE)
library(tidyverse)
library(plyr)
library(magrittr)
library(faraway)

housing <- read.csv('/Users/sarahzhang/Desktop/Final\ Project/housing.txt')
Morty <- read.csv('/Users/sarahzhang/Desktop/Final\ Project/Morty.txt')

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

# Coef with p-value bigger than 0.01 significent level
coef.p.big <- summary(model0)$coefficients[summary(model0)$coefficients[,4]>0.01,]

# HeatingQC.Q         -3.879e-03  2.165e-02  -0.179 0.857786
model1 <- update(model0, . ~ . - HeatingQC.Q)
anova(model0, model1)

#BsmtExposure.L      3.478e-03  1.148e-02   0.303 0.761986
model2 <- update(model1, . ~ . - BsmtExposure.L)
anova(model1, model2)

#GarageArea          1.278e-05  3.250e-05   0.393 0.694228
model3 <- update(model2, . ~ . - GarageArea)
anova(model2, model3)

#SaleTypeCon         1.053e-01  1.040e-01   1.012 0.311567
model4 <- update(model3, . ~ . - SaleTypeCon)
anova(model3, model4)

#StreetPave          9.624e-02  6.100e-02   1.578 0.114877
model5 <- update(model4, . ~ . - StreetPave)
anova(model4, model5)

#ExterCond.L         5.475e-02  3.559e-02   1.538 0.124152
model6 <- update(model5, . ~ . - ExterCond.L)
anova(model5, model6)

#MSZoningFV          6.892e-02  3.859e-02   1.786 0.074353 .
model7 <- update(model6, . ~ . - MSZoningFV)
anova(model6, model7)

#MasVnrArea          4.602e-05  2.553e-05   1.803 0.071666 .
model8 <- update(model7, . ~ . - MasVnrArea)
anova(model7, model8)

#NeighborhoodVeenker 8.320e-02  4.450e-02   1.870 0.061733 .
model9 <- update(model8, . ~ . - NeighborhoodVeenker)
anova(model8, model9)

#OpenPorchSF         1.216e-04  6.350e-05   1.915 0.055690 .
model10 <- update(model9, . ~ . - OpenPorchSF)
anova(model9, model10)

#HeatingGasW         7.993e-02  3.675e-02   2.175 0.029813 * 
model11 <- update(model10, . ~ . - HeatingGasW)
anova(model10, model11)

#RoofMatlMembran     3.427e-01  1.474e-01   2.324 0.020266 * 
model12 <- update(model11, . ~ . - RoofMatlMembran)
anova(model11, model12)

#KitchenQual.Q       3.665e-02  1.505e-02   2.435 0.015028 *
model13 <- update(model12, . ~ . - KitchenQual.Q)
anova(model12, model13)

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

subsetHousing %<>% dplyr::select(-OpenPorchSF, -NeighborhoodVeenker, -MasVnrArea, -MSZoningFV, -ExterCond.L, -StreetPave, -SaleTypeCon, -GarageArea, 
                                 -BsmtExposure.L, -HeatingQC.Q, -KitchenQual.Q, -HeatingGasW, -RoofMatlMembran)

best.Model <- model13
summary(best.Model)

# Model Diagnosis

# Nonnormality
e <- best.Model$residuals
std.residuals <- (e - mean(e)) / sd(e)

# qqplot
qqnorm(std.residuals)
qqline(std.residuals, col= 'blue')

# ks.test
residual.List = 0
for (i in 1:100){     
  residual.List = residual.List + ks.test(std.residuals, rnorm(1000))[2][[1]]
}
residual.List/100

#Outlier removal
library(olsrr)
ols_dffits_plot(best.Model)
dffits_bs <- abs(dffits(best.Model))
dffits_bs[is.na(dffits(best.Model))] <- 10
x_new <- subsetHousing %>% dplyr::select(-c(y))
dffits_data_bs <- subsetHousing[dffits_bs < 2 * sqrt(ncol(x_new)/nrow(x_new)), ]

model_new <- lm(y ~ ., data = dffits_data_bs)
summary(model_new)

#ANOVA F-test

#NeighborhoodClearCr 4.555e-02  2.542e-02   1.792 0.073407 . 
model_new1 <- update(model_new, . ~ . - NeighborhoodClearCr)
anova(model_new, model_new1)

#RoofMatlWdShngl     1.494e-01  7.429e-02   2.011 0.044556 *
model_new2 <- update(model_new1, . ~ . - RoofMatlWdShngl)
anova(model_new1, model_new2)

#PoolArea            4.630e-03  1.838e-03   2.519 0.011894 *
model_new3 <- update(model_new2, . ~ . - PoolArea)
anova(model_new2, model_new3)

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

best.model.new <- model_new3
summary(best.model.new)
dffits_data_bs %<>% dplyr::select(-PoolArea, -RoofMatlWdShngl, -NeighborhoodClearCr)

e.new <- best.model.new$residuals
std.residuals.new <- (e.new - mean(e.new)) / sd(e.new)

# qqplot
qqnorm(std.residuals.new)
qqline(std.residuals.new, col= 'blue')

# ks.test
residual.List.new = 0
for (i in 1:100){     
  residual.List.new = residual.List.new + ks.test(std.residuals.new, rnorm(1000))[2][[1]]
}
residual.List.new/100

# VIF
X = scale(model.matrix( ~ .-1, dffits_data_bs %>% dplyr::select(-y)))
lm <- lm(dffits_data_bs$y ~ X)
faraway::vif(lm)

# Non-linearity
plot(x=best.model.new$fitted.values, y=best.model.new$residuals, main = 'Residuals Plot',
     xlab='Fitted Values', ylab='Residuals')

# Nonconstancy
semistudentres = resid(best.model.new)/sqrt(mean(best.model.new$residuals^2))
plot(x=best.model.new$fitted.values, y=semistudentres^2, main='Squared Residuals Plot',
     xlab='Fitted Values', ylab='Residuals^2')

# Picture summary of Best Model
par(mfrow = c(2,2))
plot(best.model.new)