rm(list = ls()) ## clean last session data

##oma asi
install.packages("viridis")
library(viridis)
library(ggplot2)

## total number of taxa

load("files/community.rda", verbose = T) # loading from previous
## Loading objects:
##   vas.plants
##   tree.counts
##   forest.types
##   xy
##   soil.data
##   tables.join

dim(vas.plants) # how many samples, how many taxa in the table
## [1] 30 79

rowSums(vas.plants) # total sum of rows (sites) colSums works for columns

rowSums(vas.plants > 0) # total count of rows (sums logical table if non-zero values)

plot(
  sort(rowSums(vas.plants > 0), decreasing = T),
  ylab = "Species richness",
  xlab = "Rank"
) # making a graph where species richness is sorted from largest to smallest (x axis)

# Rank-abundance graphs
plot(
  NA,
  xlim = c(1, max(rowSums(vas.plants > 0))),
  ylim = c(0.1, 1),
  log = "y",
  ylab = "Relative abundance (log)",
  xlab = "Rank",
  main = "Rank-abundance graph"
) # empty graph
for (i in c(3, 5, 25, 28)) {
  # selecting 4  sites with id-s 3, 5, 25 and 28
  spInSite <- vas.plants[i, vas.plants[i, ] > 0]
  spInSiteRel <- as.numeric(spInSite / max(spInSite))
  points(
    sort(spInSiteRel, decreasing = T),
    lwd = 2,
    pch = 16,
    type = "o",
    col = i
  )
}

# NB! color is defined by id number (each number has a color, here it is important that they just differ).  We can identify to which site each line belongs adding a  legend:
legend(
  "topright",
  legend = c(3, 5, 25, 28),
  pch = 16,
  lwd = 2,
  col = c(3, 5, 25, 28)
)

richness <- rowSums(vas.plants > 0) # definign richness for comparison as an object

library(vegan)
## Loading required package: permute
## Loading required package: lattice
## This is vegan 2.6-2

diversity.shannon <- diversity(vas.plants, index = "shannon") ##liigirikkuse ja liikide ohtruse ühtlus. gini jällegi annab enam kaalu tavalisematele, dominantide ohtrus
eff.richness <- exp(diversity.shannon)
diversity.simpson <- diversity(vas.plants, index = "invsimpson") ##dominantsi indeks. kui läheneb 1, on 1 dominantne liik. mida madalam, seda ühtlasem

pairs(data.frame(richness, eff.richness, diversity.shannon, diversity.simpson))
evenness <- diversity.shannon / log(richness) ## kui ühtlaselt on liigid koosluses jaotunud. ühikuta

##Unequal sampling? Rarefaction and extrapolations

rarefy(tree.counts, 3)
plot(rowSums(tree.counts > 0), rarefy(tree.counts, 3))

## Sometimes it is nice to see species accumulation graphs from 2,3, ... n individuals
## First some max values
max.tree.count <- max(rowSums(tree.counts)) # max number of trees in a site

max.tree.rich <- max(rowSums(tree.counts > 0)) # max richness

# Now ploting
plot(
  NA,
  xlim = c(2, max.tree.count),
  ylim = c(0, max.tree.rich),
  xlab = "Number of trees",
  ylab = "Species richness"
)

# Including all 30 sites!
for (i in 1:nrow(tree.counts)) {
  trees <- 2:rowSums(tree.counts[i, ])
  rar <- rarefy(tree.counts[i, ], trees)
  # Let's select colors randomly
  lines(
    x = trees,
    y = rar[1, ],
    pch = 16,
    col = rgb(
      red = sample(100, 1),
      green = sample(100, 1),
      blue = sample(100, 1),
      maxColorValue = 100
    )
  )
}

## Let's evaluate total richness separately for sites with low and sites with high soil pH:
high.pH <- soil.data$pH.KCl > median(soil.data$pH.KCl)
plot(specaccum(tree.counts[high.pH, ]), lwd = 2, col = "blue")
plot(specaccum(tree.counts[!high.pH, ]), col = "red", add = T)
legend(
  "bottomright",
  legend = c("High pH", "Low pH"),
  col = c("blue", "red"),
  lwd = 2
)


## Beta diversity
# multiplicative beta diversity
ncol(vas.plants) / mean(rowSums(vas.plants > 0))
## [1] 5.895522

# additive beta diversity
ncol(vas.plants) - mean(rowSums(vas.plants > 0))
## [1] 65.6

# Comparing beta diversities between High vs. low pH soil subsamples
high.ph.gamma <- sum(colSums(vas.plants[high.pH, ]) > 0)
low.ph.gamma <- sum(colSums(vas.plants[!high.pH, ]) > 0)

high.ph.gamma
## [1] 74
low.ph.gamma
## [1] 47

high.ph.gamma / mean(rowSums(vas.plants[high.pH, ] > 0))
## [1] 4.302326
low.ph.gamma / mean(rowSums(vas.plants[!high.pH, ] > 0))
## [1] 4.895833

high.ph.gamma - mean(rowSums(vas.plants[high.pH, ] > 0))
## [1] 56.8
low.ph.gamma - mean(rowSums(vas.plants[!high.pH, ] > 0))
## [1] 37.4

# Dark
install.packages(DarkDiv)
library(DarkDiv)
# Using default method based on hypergeometric distribution

dark <- DarkDiv(vas.plants)

str(dark) # output is a list!
## List of 4
##  $ indication: num [1:79, 1:79] 0 2.82 1.12 2.82 -0.48 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:79] "T.ACTAspic" "T.AEGOpoda" "T.ANEMnemo" "T.ASAReuro" ...
##   .. ..$ : chr [1:79] "I.ACTAspic" "I.AEGOpoda" "I.ANEMnemo" "I.ASAReuro" ...
##  $ AllProbs  : num [1:30, 1:79] 0.7 0.324 0.663 0.421 0.308 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:30] "X001Vapramae" "X002Illi" "X003Vitipalu" "X004Konguta" ...
##   .. ..$ : chr [1:79] "ACTAspic" "AEGOpoda" "ANEMnemo" "ASAReuro" ...
##  $ Pool      : num [1:30, 1:79] 0.7 0.324 0.663 0.421 0.308 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:30] "X001Vapramae" "X002Illi" "X003Vitipalu" "X004Konguta" ...
##   .. ..$ : chr [1:79] "ACTAspic" "AEGOpoda" "ANEMnemo" "ASAReuro" ...
##  $ Dark      : num [1:30, 1:79] 0.7 0.324 0.663 0.421 0.308 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:30] "X001Vapramae" "X002Illi" "X003Vitipalu" "X004Konguta" ...
##   .. ..$ : chr [1:79] "ACTAspic" "AEGOpoda" "ANEMnemo" "ASAReuro" ...

# There are several tables in the output, but we only need the table "Dark"

dark <- dark$Dark

dark[1:5, 1:5] # We have NA values since observed species get value NA
##               ACTAspic  AEGOpoda   ANEMnemo  ASAReuro ATHYfil.fe
## X001Vapramae 0.6999908 0.5663234 0.80163864        NA  0.5758858
## X002Illi     0.3241750 0.2668499 0.19313708 0.2095176  0.2404332
## X003Vitipalu 0.6629486        NA         NA 0.6197400  0.5111869
## X004Konguta  0.4212651 0.3635984 0.59325066 0.4547887  0.5246721
## X005Vehendi  0.3084245 0.1446135 0.02658442 0.2058245  0.1505195

richness <- rowSums(vas.plants > 0)
dark.div <- rowSums(dark > 0.7, na.rm = T)
# For simplicity we make binary dark diversity based on threshold of 0.7
# na.rm=T needed to skip NA values (otherwise summing will result NA as well)

species.pool <- richness + dark.div # species pool is the sum of observed richenss and dark diversity

plot(species.pool, richness)
abline(0, 1)

plot(richness, dark.div)

completeness <- richness / species.pool * 100 # completeness reflects how large proportion of species pool has been realized locally

plot(species.pool, completeness)

cor(soil.data$pH.KCl, richness) # just a correlation coefficient
## [1] 0.7247559

# Pearson correlation test requires normality of both variables and linearity
hist(soil.data$pH.KCl)

hist(richness)

# Normality test: Shapiro-Wilks test
shapiro.test(soil.data$pH.KCl)
##
##  Shapiro-Wilk normality test
##
## data:  soil.data$pH.KCl
## W = 0.94732, p-value = 0.1432
shapiro.test(richness)
##
##  Shapiro-Wilk normality test
##
## data:  richness
## W = 0.97798, p-value = 0.7696

# Linear fit
plot(soil.data$pH.KCl, richness)


# OK. If not ok, then try to transform data (e.g. log). If this does not help, use rank correlation (e.g  Spearman)

cor.o <- cor.test(soil.data$pH.KCl, richness) # recording output from a test as an object
cor.o
##
##  Pearson's product-moment correlation
##
## data:  soil.data$pH.KCl and richness
## t = 5.5661, df = 28, p-value = 5.919e-06
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.4932877 0.8603750
## sample estimates:
##       cor
## 0.7247559

str(cor.o) ## data type for test output is a list as well (i.e. a mixture of different types of objects).
## List of 9
##  $ statistic  : Named num 5.57
##   ..- attr(*, "names")= chr "t"
##  $ parameter  : Named int 28
##   ..- attr(*, "names")= chr "df"
##  $ p.value    : num 5.92e-06
##  $ estimate   : Named num 0.725
##   ..- attr(*, "names")= chr "cor"
##  $ null.value : Named num 0
##   ..- attr(*, "names")= chr "correlation"
##  $ alternative: chr "two.sided"
##  $ method     : chr "Pearson's product-moment correlation"
##  $ data.name  : chr "soil.data$pH.KCl and richness"
##  $ conf.int   : num [1:2] 0.493 0.86
##   ..- attr(*, "conf.level")= num 0.95
##  - attr(*, "class")= chr "htest"

cor.o$p.value # retrieving a component of the list (p-value)
## [1] 5.91914e-06

# Regression model -- we have hypothesis of dependent (richness) and independent (pH) variables
model <- lm(richness ~ soil.data$pH.KCl)

summary(model)
##
## Call:
## lm(formula = richness ~ soil.data$pH.KCl)
##
## Residuals:
##     Min      1Q  Median      3Q     Max
## -8.8523 -2.8222 -0.5426  2.9875  7.4495
##
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)
## (Intercept)        -7.152      3.766  -1.899   0.0679 .
## soil.data$pH.KCl    5.720      1.028   5.566 5.92e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 4.066 on 28 degrees of freedom
## Multiple R-squared:  0.5253, Adjusted R-squared:  0.5083
## F-statistic: 30.98 on 1 and 28 DF,  p-value: 5.919e-06

abline(model, col = "darkred", lty = 2, lwd = 2) # adding regression line to the graph.

# Regression expects that model residuals are normally distributed (i.e. testing if there is a deviation from normal distribution)
shapiro.test(resid(model))
##
##  Shapiro-Wilk normality test
##
## data:  resid(model)
## W = 0.96457, p-value = 0.403

# OK -- no deviation!
