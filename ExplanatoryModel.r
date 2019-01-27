library(dplyr)
library(magrittr)
library(MASS)

# setwd('Desktop')
# rawData = read.csv('housing.txt')
# housing = rawData

for (i in 1:ncol(housing)){
  if (is.factor(housing[[i]]) & sum(is.na(housing[[i]])) > 0){
    housing[[i]] = addNA(housing[[i]])
  } else {
    housing[is.na(housing[,i]), i] <- mean(housing[,i], na.rm = TRUE)
  }
}

housing %<>% mutate(SalePrice = log(SalePrice))
housing %<>% dplyr::select(-Id)
# drop total living area and total basement area due to perfect multi-colinearality
housing %<>% dplyr::select(-GrLivArea, -TotalBsmtSF)
# Overall Quality / Overall Condition
housing %<>% dplyr::select(-OverallQual, -OverallCond)

# Variable selection by lasso
X = model.matrix( ~ .-1, housing %>% dplyr::select(-SalePrice)) # Dummy encoding
y = housing$SalePrice

# lassoModel = glmnet(X,y)
# plot(lassoModel,'lambda',main='Coefficient Path for Lasso')

set.seed(123)
bestLambdaList = rep(0,10)
for (i in 1:10){     # repeat CV for 10 times
  lassoCV = cv.glmnet(X, y, nfolds=5) # Cross validation
  bestLambdaList[i] = lassoCV$lambda.min
}
bestLambda = mean(bestLambdaList)
print(paste0('BestLambda: ', round(bestLambda)))
print(paste0('Best CV: ', round(min(lassoCV$cvm))))

lassoBest = glmnet(X,y,lambda=bestLambda) # Fit the model with the best lambda
nonZeroVar = names(lassoBest$beta[which(lassoBest$beta[,1]>0),])

### Fitting new model with varibles selected by lasso ###
subsetHousing = cbind(select(data.frame(X), one_of(nonZeroVar)), y)
model0 = lm(y ~ ., data = subsetHousing)
summary(model0)

### F test ###
# 从summary里看哪个变量不显著， 抓这个出来做F test
model1  = update(model0, . ~ . - MSZoningFV)
anova(model0, model1)
# 这里Anova显示两个模型没有显著差别，所以MSZoningFV这个变量就可以去掉了，以此类推


### 这是我以前写的一个自动做F test选出最佳模型的function，还没来得及改，但先放在这里给你们看看
forward = function(d,alpha=0.05){
  m0 = lm(d[,1]~1,data=d)
  i = 2
  min_p = 1
  bestModel = 0
  bestVar = 0
  top = 10
  Added = TRUE
  varList = c(1:(ncol(d)-1))
  while(Added){
    for(i in varList){
      #mi = update(m0,.~.+I(newData[,i]),data=newData)
      mi = update(m0,as.formula(paste0('.~.+I(d[,',i,'])')),data=d)
      ano = anova(m0,mi)
      p = ano$`Pr(>F)`[2]
      if(p<min_p & p<alpha){
        min_p = p
        bestModel = mi
        bestVar = i
      }
      #i = i + 1
    }
    if(min_p>0.1 | is.na(varList[1])){Added = FALSE}
    min_p = 1
    varList = varList[-which(varList==bestVar)]
    m0 = bestModel
  }
  return(bestModel)
}


# NullLM = lm(SalePrice ~ 1, data = housing)
# stepwise = step(NullLM, scope=list(lower=~1, upper = ~.), k=2, direction='forward')
