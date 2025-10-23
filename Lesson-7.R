## the more ar Eigen näitab trendi. ja nad on ligisstikku on korrelatsioon kõrge, kui üksteisest kaugel, siis madal (kraad kui kaugele nad suunduvad). kui palju muutlikkust on kaasatud. gradient on suunas see ja see ja siis üksikud plotid et mis on eriti ägedad. PC 70% oleks hea. PC tavaliselt keskkonnamuutujate kirjeldamiseks.

# 7 Combining community and environmental data
# The main aim of this lesson is to examine how the community structure (i.e. species richness and composition) is linked to environmental variables.

# 7.1 Reading data from previous days
library(vegan)
## Warning: package 'vegan' was built under R version 4.4.3
## Warning: package 'permute' was built under R version 4.4.3
load("files/community.rda") # loading from previous
load("files/clusters.rda")
load("files/ordi.rda")

# 7.2 Exploring and combining similar soil data
# In the previous class, we used ordination methods to put samples and taxa in order, so that similar items are close to each other. Ordination methods can also be used with correlated environmental data. If we do this, we will obtain a combined measurement of some environmental variable. An alternative method is to standardize and average measures. Let’s look at soil chemistry.
cor(soil.data[, -5]) # N, P and K strongly correlated
pairs(soil.data[, -5])

# We can use PCA for soils
soilPCA <- prcomp(soil.data[, 2:4], scale. = T)
biplot(soilPCA)
soil.pca1 <- scores(soilPCA)[, 1] # scale.=T standardizes units

# Averaging all three after rescaling
npk.scale <- scale(soil.data[, 2:4]) # rescaling mean 0 and sd 1
soil.nut <- rowSums(npk.scale)

plot(soil.nut, soil.pca1)
# NB! PCA axes can be changed!
soil.pca1 = soil.pca1 * -1
plot(soil.nut, soil.pca1)

#sample
# Make PCA with all soil variables found in envir.txt and examine how well the PC scores are correlated with the initial soil variables.
envir <- read.table(
  "files/envir.txt"
)
names(envir)
head(envir)
all.soil.pca1 <- prcomp(envir[, 6:12], scale. = T)
soil.cor <- cor(envir[, 6:12])
soil.cor
all.soil <- scores(all.soil.pca1, envir[, 6:12])
all.soil

# 7.3 Environment within clusters
# Instead of working with the original environmental data, we are going to use our new variable ´soil.pca1´ as a synthetic variable that reflects nutrient availabity in our sites. We will test whether the environment differs between samples representing different clusters. For this, we will first make some box plots, then we will perform an anova test and then we will examine whether the assumptions of anova are met.

## Using clusters and soil data (Soil pH as an example)
boxplot(
  soil.pca1 ~ as.factor(o.grel),
  col = 2:6,
  ylab = "Soil Nutrients",
  xlab = "Clusters"
)

# ANOVA test

o.anova <- aov(soil.pca1 ~ as.factor(o.grel))

summary(o.anova)
##                   Df Sum Sq Mean Sq F value Pr(>F)
## as.factor(o.grel)  4  11.02   2.754   1.404  0.261
## Residuals         25  49.02   1.961
# df - grouping. p-value is not statistically relevant here. residuals have to be normal, not right answer... homogenity of the variance has to be for anova.

# testing anova assumptions:

# 1. residuls of the model must be normally distributed

shapiro.test(resid(o.anova)) # p > 0.05
##
##  Shapiro-Wilk normality test
##
## data:  resid(o.anova)
## W = 0.92643, p-value = 0.03951
hist(resid(o.anova))

# 2. homogeneity of variances among groups

bartlett.test(soil.pca1, as.factor(o.grel)) # p > 0.05
##
##  Bartlett test of homogeneity of variances
##
## data:  soil.pca1 and as.factor(o.grel)
## Bartlett's K-squared = 16.131, df = 4, p-value = 0.002848

## If ANOVA assumptions are not met, we can make non-parametric test
## for example, the Kruskal-Wallis rank sum test

kruskal.test(soil.pca1 ~ as.factor(o.grel)) ## Not significantly different
##
##  Kruskal-Wallis rank sum test
##
## data:  soil.pca1 by as.factor(o.grel)
## Kruskal-Wallis chi-squared = 4.05, df = 4, p-value = 0.3993

## Let's now examine soil pH

o.anova <- aov(soil.data$pH.KCl ~ as.factor(o.grel))
shapiro.test(resid(o.anova)) # OK
##
##  Shapiro-Wilk normality test
##
## data:  resid(o.anova)
## W = 0.93286, p-value = 0.05851

bartlett.test(soil.data$pH.KCl, as.factor(o.grel)) # OK
##
##  Bartlett test of homogeneity of variances
##
## data:  soil.data$pH.KCl and as.factor(o.grel)
## Bartlett's K-squared = 3.2945, df = 4, p-value = 0.5098
hist(resid(o.anova))

summary(o.anova) #  significance
##                   Df Sum Sq Mean Sq F value  Pr(>F)
## as.factor(o.grel)  4  6.991  1.7477   5.042 0.00405 **
## Residuals         25  8.666  0.3466
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TukeyHSD(o.anova) # comparison of pairs, Tukey test
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
##
## Fit: aov(formula = soil.data$pH.KCl ~ as.factor(o.grel))
##
## $`as.factor(o.grel)`
##         diff        lwr       upr     p adj
## 2-1 -0.70000 -1.5645426 0.1645426 0.1545794
## 3-1  0.80500 -0.6067922 2.2167922 0.4667457
## 4-1 -0.11000 -1.5217922 1.3017922 0.9993478
## 5-1  0.26875 -0.6650628 1.2025628 0.9137158
## 3-2  1.50500  0.1843893 2.8256107 0.0198753
## 4-2  0.59000 -0.7306107 1.9106107 0.6864372
## 5-2  0.96875  0.1795342 1.7579658 0.0108067
## 4-3 -0.91500 -2.6440853 0.8140853 0.5388419
## 5-3 -0.53625 -1.9032119 0.8307119 0.7775773
## 5-4  0.37875 -0.9882119 1.7457119 0.9239282

boxplot(
  soil.data$pH.KCl ~ as.factor(o.grel),
  col = 2:6,
  ylab = "pH",
  xlab = "Clusters"
) #plot data to help them to interpret the results

# Parametric tests (e.g. ANOVA) are usually more powerful than non-parametric tests (e.g. Kruskal-Wallis) but results are often similar. Test how soil pH is related to clusters using Kruskal-Wallis test.

# 7.4 Environment related to ordination
# We ca use the function envfit to visually explore the correlation of ordination results with environmental parameters.
o.ev <- envfit(o.mds, soil.data[, -5])
o.ev
##
## ***VECTORS
##
##            NMDS1    NMDS2     r2 Pr(>r)
## pH.KCl  -0.99948 -0.03230 0.2577  0.021 *
## N..      0.35089 -0.93642 0.1541  0.120
## P.mg.kg -0.42218 -0.90651 0.2055  0.057 .
## K.mg.kg  0.17452 -0.98465 0.0522  0.514
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## Permutation: free
## Number of permutations: 999
plot(o.mds$points, pch = o.grel, col = o.grel)
plot(o.ev, add = T) # Adding correlations to graph

#Let's add contour levels for pH and NPK
ordisurf(o.mds, soil.data$pH.KCl, add = T)
##
## Family: gaussian
## Link function: identity
##
## Formula:
## y ~ s(x1, x2, k = 10, bs = "tp", fx = FALSE)
##
## Estimated degrees of freedom:
## 4.66  total = 5.66
##
## REML score: 29.02233
ordisurf(o.mds, soil.pca1, add = T, col = "green")
##
## Family: gaussian
## Link function: identity
##
## Formula:
## y ~ s(x1, x2, k = 10, bs = "tp", fx = FALSE)
##
## Estimated degrees of freedom:
## 4.52  total = 5.52
##
## REML score: 52.6909

oo <- ordisurf(o.mds, soil.data$pH.KCl, bubble = 5) # visualizing environment with circle size
plot(oo, what = "persp") # 3D image of study variable (z) along ordination axes (x and y)

# 7.5 Constrained ordination
# Exploring only variation that can be explained by the selected environmental variables (=constraints). Comparing the taxonomic space vs. environmental space.

## Linear relationships, similar to to PCA. While the former is indirect  gradient analyses, RDA is a direct one (env.data enter in the analysis)
o.rda <- rda(
  vas.plants ~ pH.KCl + N.. + P.mg.kg + K.mg.kg,
  data = soil.data,
  scale = T
) # here formulas are suggested, scale=T to put all measures in same units
plot(o.rda)

# A simpler plot
plot(o.rda, type = "n") #triplot
points(o.rda, "sites", col = "red", pch = 16)
points(o.rda, display = "bp") # constraints arrow
text(o.rda, "bp")

anova(o.rda) # overall significance by randomizations
## Permutation test for rda under reduced model
## Permutation: free
## Number of permutations: 999
##
## Model: rda(formula = vas.plants ~ pH.KCl + N.. + P.mg.kg + K.mg.kg, data = soil.data, scale = T)
##          Df Variance      F Pr(>F)
## Model     4   18.204 1.8714  0.001 ***
## Residual 25   60.796
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
anova(o.rda, by = "mar") # each parameter separately
## Permutation test for rda under reduced model
## Marginal effects of terms
## Permutation: free
## Number of permutations: 999
##
## Model: rda(formula = vas.plants ~ pH.KCl + N.. + P.mg.kg + K.mg.kg, data = soil.data, scale = T)
##          Df Variance      F Pr(>F)
## pH.KCl    1    5.202 2.1391  0.001 ***
## N..       1    5.844 2.4033  0.005 **
## P.mg.kg   1    4.311 1.7728  0.013 *
## K.mg.kg   1    2.587 1.0636  0.332
## Residual 25   60.796
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
anova(o.rda, by = "axis") # significance along axes
## Permutation test for rda under reduced model
## Forward tests for axes
## Permutation: free
## Number of permutations: 999
##
## Model: rda(formula = vas.plants ~ pH.KCl + N.. + P.mg.kg + K.mg.kg, data = soil.data, scale = T)
##          Df Variance      F Pr(>F)
## RDA1      1    6.397 2.6306  0.010 **
## RDA2      1    6.331 2.6033  0.010 **
## RDA3      1    3.454 1.4202  0.227
## RDA4      1    2.022 0.8313  0.708
## Residual 25   60.796
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Canonical Correspondence Analysis is similar, but it assumes unimodal responses (like correspondence analysis did)
o.cca <- cca(vas.plants ~ pH.KCl + N.. + P.mg.kg + K.mg.kg, data = soil.data)
anova(o.cca)
## Permutation test for cca under reduced model
## Permutation: free
## Number of permutations: 999
##
## Model: cca(formula = vas.plants ~ pH.KCl + N.. + P.mg.kg + K.mg.kg, data = soil.data)
##          Df ChiSquare      F Pr(>F)
## Model     4    0.8410 1.5721  0.004 **
## Residual 25    3.3434
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 7.6 Multivariate ANOVA based on dissimilarities
# We can partition the dissimilarities among different sources of variation. In this case, no ordination is used; instead the multivariate space is considered. Significance values are obtained from permutation tests. Also known as PERMANOVA (function adonis).

o.adonis <- adonis2(
  vas.plants ~ pH.KCl + N.. + P.mg.kg + K.mg.kg,
  data = soil.data,
  method = "manhattan",
  by = "mar"
)
o.adonis
## Permutation test for adonis under reduced model
## Marginal effects of terms
## Permutation: free
## Number of permutations: 999
##
## adonis2(formula = vas.plants ~ pH.KCl + N.. + P.mg.kg + K.mg.kg, data = soil.data, method = "manhattan", by = "mar")
##          Df SumOfSqs      R2      F Pr(>F)
## pH.KCl    1   1070.7 0.09237 3.1563  0.003 **
## N..       1   1094.0 0.09438 3.2249  0.005 **
## P.mg.kg   1    859.0 0.07411 2.5323  0.017 *
## K.mg.kg   1    249.7 0.02154 0.7360  0.663
## Residual 25   8480.7 0.73164
## Total    29  11591.4 1.00000
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 7.7 Explore species spatial distribution and perform a simple Species Distribution Model (SDM)
# Exploring if species presences/absences are related to some parameters. Can be used to predict changes in distribution if the environment changes (e.g., due to global change).

# Selecting a common taxa (We need specialv ersion of SDMs to work with very rare taxa)
pa <- vas.plants[, "RUBUsaxa"] > 0 # presence/absence
plot(pa ~ soil.data$pH.KCl)

mod <- lm(pa ~ soil.data$pH.KCl) # linear model
summary(mod)
##
## Call:
## lm(formula = pa ~ soil.data$pH.KCl)
##
## Residuals:
##      Min       1Q   Median       3Q      Max
## -0.83734 -0.29760 -0.04173  0.35433  0.72338
##
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)
## (Intercept)       -0.8372     0.3987  -2.100  0.04489 *
## soil.data$pH.KCl   0.3814     0.1088   3.506  0.00155 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.4305 on 28 degrees of freedom
## Multiple R-squared:  0.3051, Adjusted R-squared:  0.2803
## F-statistic: 12.29 on 1 and 28 DF,  p-value: 0.001552
abline(mod, col = "blue") # does not fit well, we need non linear model ...

mod_glm <- glm(pa ~ soil.data$pH.KCl, family = binomial)
summary(mod_glm)
##
## Call:
## glm(formula = pa ~ soil.data$pH.KCl, family = binomial)
##
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)
## (Intercept)       -8.2388     3.1321  -2.630  0.00853 **
## soil.data$pH.KCl   2.3744     0.8891   2.671  0.00757 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## (Dispersion parameter for binomial family taken to be 1)
##
##     Null deviance: 41.455  on 29  degrees of freedom
## Residual deviance: 30.007  on 28  degrees of freedom
## AIC: 34.007
##
## Number of Fisher Scoring iterations: 5
pr.glm <- predict(mod_glm, type = "response")

points(pr.glm ~ soil.data$pH.KCl, col = "red")

# Now all soil data
mod_glm_all <- glm(pa ~ ., soil.data[, -5], family = binomial) #~. means that all parameters from data are included
summary(mod_glm_all)
##
## Call:
## glm(formula = pa ~ ., family = binomial, data = soil.data[, -5])
##
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept) -11.721287   4.923054  -2.381   0.0173 *
## pH.KCl        2.796036   1.124265   2.487   0.0129 *
## N..          -1.649511   3.110033  -0.530   0.5958
## P.mg.kg       0.099349   0.062678   1.585   0.1130
## K.mg.kg       0.002568   0.023451   0.110   0.9128
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## (Dispersion parameter for binomial family taken to be 1)
##
##     Null deviance: 41.455  on 29  degrees of freedom
## Residual deviance: 24.174  on 25  degrees of freedom
## AIC: 34.174
##
## Number of Fisher Scoring iterations: 6
pr.glm_all <- predict(mod_glm_all, type = "response")

#Predicted distribution map
plot(xy[, -3], cex = pr.glm_all * 2.5 + 1)
points(xy[pa > 0, -3], pch = 16, cex = 0.9, col = "darkgreen") # Actual presence-absence

# Regression trees can handle more complex relationships.
library(tree)
## Warning: package 'tree' was built under R version 4.4.3

o.tree <- tree(pa ~ ., data = soil.data[, -5])
plot(o.tree)
text(o.tree)

pr.tree <- predict(o.tree) # using the tree to predict

plot(xy[, -3], cex = pr.tree * 2.5 + 1)
points(xy[pa > 0, -3], pch = 16, cex = 0.9, col = "darkgreen")

## Scenarios for future changes

## drop of pH 0.5 units

new.soil <- soil.data[, -5]
new.soil[, 1] <- new.soil[, 1] - 0.5
pr.tree.1 <- predict(o.tree, new.soil)
plot(xy[, -3], cex = pr.tree.1 * 2.5 + 1)
points(xy[pa > 0, -3], pch = 16, cex = 0.9, col = "darkgreen")

points(xy[pa > 0 & pr.tree.1 < 0.5, -3], pch = 16, cex = 0.9, col = "red")
