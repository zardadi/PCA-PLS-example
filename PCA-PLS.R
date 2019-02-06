load("nutrition.rdata")
# First we need to clean the data
dim(nutrition)
nutrition <- na.omit(nutrition)
colnames(nutrition)
# choose 'calories' as our continious response variable
# we need to remove 'calories' and categorial variables from our PCA. 
## ------------------------------------------------------------------------
nupca <- prcomp(nutrition[,-c(1,2,28)], scale.=TRUE)
summary(nupca)
# By the Kaiser criterion we would keep 8 components and retain about 65% of the variability in the data
## ------------------------------------------------------------------------
# store the scores of the original observations on those 8 components for later
scr <- nupca$x[,1:8]
# now we can run PCReg for calories as a response
## ------------------------------------------------------------------------
pcregmod <- lm(nutrition$calories ~ scr)
summary(pcregmod)

# Cool! Everything appears interesting and useful for modeling.

# But, we did this unsupervised (without including calories). 
# Recall: PCA is an unsupervised technique. If you look back, there is no inherent response variable (Y) that we are optimizing.
# We simply rotated our original predictors in a 'clever' manner,and toss out the rotations that do not appear to contain
# important information. PCA (once you've removed some components) is generally viewed as a dimensionality reduction technique
# A more proper approach is to somehow consider the response variable Y while performing the PCA in the first place!
# Partial Least Squares (PLS) does exactly this.
# So maybe PLS can help us improve.
## ------------------------------------------------------------------------
#install.packages("pls")
library(pls)
# plsr using  a formula for calories as a response to all other numeric predictors.
## ------------------------------------------------------------------------
nuplsmod <- plsr(calories~., data=nutrition[,-c(1,28)], scale=TRUE, method="oscorespls")
summary(nuplsmod)

# Now it appears that 2 components are sufficient (cumlatively explained just over 89% of variation in 'calories'.
# Compared to PCReg - 8 components explained only 79% of the variation in calories.
# What if we take just to components for PCReg...
## ------------------------------------------------------------------------
pcregmod2 <- lm(nutrition$calories ~ scr[,1:2])
summary(pcregmod2)

# We two PCS components Adjusted R-squared is 0.23. Hence why PLS is generally a superior approach in a supervised context.

summary(lm(nutrition$calories ~ nuplsmod$scores[,1:2]))
# If we use first two components of PLS, then we see Adjusted R-squared is 89% 
## ------------------------------------------------------------------------

# A cautionary tale...
dwmod <- lm(calories~total_fat+carbohydrates, data=nutrition)
summary(dwmod)

# If we just use plain data set and without any knowledge of PCA, we might get better results!
# Just slightly more of the variation (90.8% vs 89.1%) in calories can be explained using an equivalent dimensionlaity from
# the original varibales.

# Conclusion: in machine learning, there is often no substitute for domain-knowledge.
