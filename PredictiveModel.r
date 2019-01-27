library(glmnet)
library(magrittr)
library(dplyr)
library(plyr)

rawData = read.csv('housing.txt')
housing = rawData
set.seed(5)

#### Data Cleaning ####

housing %<>% mutate(SalePrice = log(SalePrice))
housing %<>% dplyr::select(-Id)
#housing %<>% dplyr::select(-GrLivArea, -TotalBsmtSF)
#housing$MSSubClass <- as.factor(housing$MSSubClass)
housing %<>% dplyr::select(-RoofMatl, -Heating, -Street, -Utilities) # not useful. Most obs have the same type, and it's not significant
housing$MasVnrArea[is.na(housing$MasVnrArea)] <- 0

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
housing %<>% dplyr::select(-Condition1, -Condition2, -ConditionNorm)

# fix level names
housing$Exterior1st = revalue(housing$Exterior1st, c("Wd Sdng"="WdSdng"))
housing$Exterior2nd = revalue(housing$Exterior2nd, c("Wd Sdng"="WdSdng","Wd Shng"="WdShing","Brk Cmn"="BrkComm",'CmentBd'='CemntBd'))

for (level in levels(housing$Exterior2nd)){
  eval(parse(text=paste0('housing$Exterior', level, '=0')))
}
for (i in 1:nrow(housing)){
  eval(parse(text=paste0('housing[i, "Exterior', housing[i,'Exterior1st'], '"]=', 1)))
  eval(parse(text=paste0('housing[i, "Exterior', housing[i,'Exterior2nd'], '"]=', 1)))
}
housing %<>% dplyr::select(-Exterior1st, -Exterior2nd)

for (level in levels(housing$BsmtFinType1)){
  eval(parse(text=paste0('housing$BsmtFinType', level, '=0')))
}
for (i in 1:nrow(housing)){
  if (!is.na(housing[i,'BsmtFinType1'])){
    eval(parse(text=paste0('housing[i, "BsmtFinType', housing[i,'BsmtFinType1'], '"]=', 1)))
  }
  if (!is.na(housing[i,'BsmtFinType2'])){
    eval(parse(text=paste0('housing[i, "BsmtFinType', housing[i,'BsmtFinType2'], '"]=', 1)))
  }  
}
housing %<>% dplyr::select(-BsmtFinType1, -BsmtFinType2)

housing[which(housing$LotArea>100000), 'LotArea'] = NA
housing$X2ndFlrSF_NA = 0
housing[which(housing$X2ndFlrSF==0), 'X2ndFlrSF_NA'] = 1
housing[which(housing$X2ndFlrSF==0), 'X2ndFlrSF'] = NA
factor.map <- c('Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) 
for (var in c('HeatingQC','ExterQual','ExterCond','BsmtQual','BsmtCond','FireplaceQu',
              'GarageQual','GarageCond','PoolQC','KitchenQual')){
  housing[var] = factor.map[as.character(housing[[var]])]
  # housing[which(is.na(housing[var])),var] = 0
}
# factor.map <- c('Mix'=1,'FuseP'=2,'FuseF'=3,'FuseA'=4,'SBrkr'=5)
# housing['Electrical'] = factor.map[as.character(housing[['Electrical']])]


# Dealing with missing values, mean for numeric and new level for factor
for (i in 1:ncol(housing)){
  if (is.factor(housing[[i]]) & sum(is.na(housing[[i]])) > 0){
    housing[[i]] = addNA(housing[[i]])
  } else {
    housing[is.na(housing[,i]), i] <- mean(housing[,i], na.rm = TRUE)
  }
}

#### Feaure Engineer ####

# Area and rooms
housing$area_per_room = housing$GrLivArea / housing$TotRmsAbvGrd
housing$area_yrsold = housing$GrLivArea * (housing$YearBuilt - min(housing$YearBuilt) + 1)
housing$area_per_bath = with(housing, GrLivArea / (FullBath+HalfBath+BsmtFullBath+BsmtHalfBath))
#housing$lowqual_prop = housing$LowQualFinSF / housing$GrLivArea

# functional rooms
#housing$kitchen_multi = housing$KitchenAbvGr * housing$KitchenQuala
#housing$fireplace_multi = housing$Fireplaces * housing$FireplaceQu
#housing$bsmt_multi = housing$TotalBsmtSF * housing$BsmtCond
#housing$garage_multi = housing$GarageArea * housing$GarageQual

#### Model Fitting
lm.model = lm(SalePrice ~ ., data = housing)

X = model.matrix( ~ . + (GrLivArea+MSZoning+OverallQual+Neighborhood)^3 - 1,
                  housing %>% dplyr::select(-SalePrice)) # Dummy encoding
# X = scale(unscaledX) # Scaling
# X = X[,!apply(X, 2, function(x) all(is.nan(x)))] # Omit zero variance column
y = housing$SalePrice

# lassoModel = glmnet(X,y)
# plot(lassoModel,'lambda',main='Coefficient Path for Lasso')

train_id = sample(nrow(housing),as.integer(nrow(housing)*0.7))
train_X = X[train_id,]
train_y = y[train_id]
holdout_X = X[-train_id,]
holdout_y = y[-train_id]

validScore = rep(0,5)
validScore_origin = rep(0,5)
i = 1
for (alpha in seq(0,1,0.2)){
  cv.model = cv.glmnet(train_X, train_y, nfolds=5, alpha=alpha) # Cross validation
  bestLambda = cv.model$lambda.min
  bestModel = glmnet(train_X,train_y,lambda=bestLambda,alpha=alpha)
  y_fit = predict(bestModel, holdout_X)
  mse = mean((y_fit - holdout_y)^2)
  mse_origin = mean((exp(y_fit) - exp(holdout_y))^2)
  validScore[i] = mse
  validScore_origin[i] = mse_origin
  i = i + 1
}
print(validScore)
print(sqrt(validScore_origin))

# lassoBest = glmnet(X,y,lambda=bestLambda) # Fit the model with the best lambda
# lassoBest$beta[which(lassoBest$beta[,1]>0),]
