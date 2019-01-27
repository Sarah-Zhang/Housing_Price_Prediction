library(ggplot2)

# I just randomly plot something that may be interesting here to get more insight into 
# the data. Some valuable plots can be put into the report if you guys like. -Lingzhi

# Distribution of Y, assumed to be normal
qplot(x = SalePrice, geom = 'histogram', data = housing) # right-skewed normal
qplot(x = log(SalePrice), geom = 'histogram', data = housing) # take log, more normal now!

# ??? no idea why it's not of slope 1
ggplot(housing, aes(sample = log(SalePrice))) + stat_qq()+ ggtitle("Q-Q Plot for log SalePrice")

# Important feature
qplot(x = OverallQual, geom = 'histogram', data = housing)
qplot(OverallQual, log(SalePrice), geom = 'point', data = housing) # strong association!

qplot(x = LotArea, geom = 'histogram', data = housing)
qplot(LotArea, log(SalePrice), geom = 'point', data = housing) # need to deal with outlier

qplot(x = MasVnrArea, geom = 'histogram', data = housing)
qplot(MasVnrArea, log(SalePrice), geom = 'point', data = housing) # many 0s, weak asoc.

qplot(x = X1stFlrSF, geom = 'histogram', data = housing)
qplot(X1stFlrSF, log(SalePrice), geom = 'point', data = housing) # strong association!

qplot(x = X2ndFlrSF, geom = 'histogram', data = housing) # need to combine this two?
qplot(X2ndFlrSF, log(SalePrice), geom = 'point', data = housing) # strong association with many 0s

# GrlivArea = X1stFlrSF + X2ndFlrSF + LowQualFinSF
# 可以drop掉总面积
# Exploration 应该总面积更有实际意义
qplot(y=log(housing$SalePrice),x=housing$GrLivArea)

qplot(x = Neighborhood, geom = 'bar', data = housing)
qplot(Neighborhood, log(SalePrice), geom = 'boxplot', data = housing) # significant diff intuitively

qplot(GrLivArea, log(SalePrice), geom = 'point', data = housing)
qplot(YearBuilt, log(SalePrice), geom = 'point', data = housing)
qplot(TotRmsAbvGrd, log(SalePrice), geom = 'point', data = housing)

qplot(x = RoofMatl, geom = 'bar', data = housing) # buzhidaogaishuoshenme, zijikantuba



