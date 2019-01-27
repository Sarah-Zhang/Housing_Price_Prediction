library(plyr)
library(magrittr)

rawdata <- read.csv('housing.txt')
housing <- rawdata

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
