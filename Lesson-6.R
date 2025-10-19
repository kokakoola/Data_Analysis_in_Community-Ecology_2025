# # 6 Ordinations
# # Ordinations aim to put samples and taxa in order so that more similar items are close to each other. They helps to visualize the similarity in structure between ecological communities.

## 6.1 Taxa as axes
# If we have just two taxa, we can plot samples on a 2-dimensional space where the axes reflect the abundance of each of these species. Then the distance between samples is the distance between points on the plot. We can imagine a 3-dimensional space (x, y and z). However, generally we have many more taxa. Let’s select the 5 taxa with the highest IndVal value and make pairwise graphs!

load("files/community.rda", verbose = T) # loading from previous lectures
load("files/clusters.rda", verbose = T)
comm.data <- log1p(vas.plants)

freq.spp <- c(58, 18, 10, 27, 38) # 5 most important spp from clusters
vas.plants[, freq.spp]

pairs(vas.plants[, freq.spp], pch = 16, col = rgb(0, 0, 1, 0.3))

## 6.2 Principal Component Analysis (PCA)
# Good for even data (no high peaks), does not work with categorical

library(vegan)
## Loading required package: permute
## Loading required package: lattice

# Let's make an artificial table with 2 taxa a and b
test <- data.frame(a = c(1, 2, 3, 2, 1), b = c(3, 2, 2, 4, 4))
test
plot(test, type = "o", xlim = c(1, 5), ylim = c(1, 5))

# How does the same data looks in PCA axes?

o.pca <- prcomp(test) # PCA
plot(scores(o.pca), asp = 1, type = "o")

# since we had originally 2 dimensions then we have the same shape but PC1 is describing the most variable direction.

summary(o.pca)

## Importance of components:
##                           PC1    PC2
## Standard deviation     1.1713 0.5727
## Proportion of Variance 0.8071 0.1929
## Cumulative Proportion  0.8071 1.0000

# how much of total variation is described by the principal components.

# Now, using real community data
o.pca <- prcomp(comm.data)
summary(o.pca) # how much of total variation is described by the principal components

pairs(scores(o.pca)[, 1:4], pch = 16, cex = 0.5) # ordination plots

# however, generally only PC1 and PC2 are used

o.pca$sdev # Standard deviation of each principal component

o.pca$rotation[, 1:2] # Eigenvectors - Subset of first 2 principal components

o.pca$x[, 1:2] # Sites Scores - Position of the site in the space

### Let's Visualize it
par(mfrow = c(1, 2)) # 2 plots on one single row
plot(scores(o.pca)[, 1:2], asp = 1, pch = o.grel) # Plot sites on the first two components with shape changing depending on the clusters
legend("topleft", legend = unique(o.grel), pch = unique(o.grel))

# Eigenvectors of frequent species -- describing their contribution to principal components, can be visualized by arrows (vectors)
x <- o.pca$rotation[freq.spp, 1]
y <- o.pca$rotation[freq.spp, 2]
plot(x, y, type = "n", asp = 1, xlab = "PC1", ylab = "PC2")
abline(h = 0, v = 0, col = "gray")
arrows(0, 0, x, y, length = 0.03, col = "red")
text(x, y, colnames(comm.data)[freq.spp], cex = 0.6)


par(mfrow = c(1, 1)) # remove plot settings

# 6.3 Principal Coordinates Analysis
# Ordination based on eigenvalues and any distance matrix. If you select Euclidean distance, it is equal to PCA.

# NB! PCOA works from a distance/dissimilarity matrix, not from raw variables, so the “original variables” are not in the analysis.

library(ape)

vegdist <- vegdist(comm.data, "euclidean")
o.pco <- pcoa(vegdist)
par(mfrow = c(1, 2))
plot(
  o.pco$vectors[, 1],
  o.pco$vectors[, 2],
  asp = 1,
  pch = o.grel,
  xlab = "PCO1",
  ylab = "PCO2"
)
plot(scores(o.pca)[, 1:2], asp = 1, pch = o.grel)

## Should be identical (NB! axis direction does not have meaning)

# Now using Bray-Curtis distance

plot(
  o.pco$vectors[, 1],
  o.pco$vectors[, 2],
  asp = 1,
  pch = o.grel,
  xlab = "PCO1",
  ylab = "PCO2"
)
vegdist <- vegdist(comm.data, "bray")
o.pco <- pcoa(vegdist)
plot(
  o.pco$vectors[, 1],
  o.pco$vectors[, 2],
  asp = 1,
  pch = o.grel,
  xlab = "PCO1 Bray",
  ylab = "PCO2 Bray"
)

o.pco$values # Eigenvalues and cumulative variance
# vaata Cum corr veergu et mis korreleeruvad. saab joonistada ka välja kastidega (corrplot package).

o.pco$vectors[, 1:2] # Coordinates of sites

par(mfrow = c(1, 1))

# 6.4 Correspondence Analysis
# This technique (also called Reciprocal Averaging) tries to ordinate both samples and taxa in parallel. It expects unimodal response curves of taxa and aims to find the weighted averages for taxa and sites.
o.ca <- cca(comm.data)

summary(o.ca)$cont$importance[, 1:3]

o.ca$CA$v[, 1:2] ## Species

o.ca$CA$u[, 1:2] ## Sites

plot(o.ca$CA$u[, 1:2], pch = o.grel, asp = 1)

plot(o.ca) # Biplot where both sites and taxa are given (their averaged locations)

# Adding manually sites and species to obtain a more clean image
plot(o.ca, type = "n")
points(o.ca, display = "sites", cex = 0.8, pch = o.grel, col = "red")
text(o.ca, display = "species", cex = 0.7, col = "blue", select = freq.spp)

plot(o.ca, type = "n", xlim = c(-2, 3.5))
points(o.ca, display = "sites", cex = 0.8, pch = 16, col = "red")
ordipointlabel(
  o.ca,
  cex = 0.7,
  display = "species",
  col = "blue",
  add = T,
  select = freq.spp
) ## Tries to optimize the location of the text labels to  avoid overlap

tabasco(comm.data, o.ca) # ordering tables using CA for both samples and taxa.

## 6.5 Nonmetric Multidimensional Scaling (NMDS)
# Not based on maximum variation but on shifting iteratively of objects within a low number of axes so that the distance between samples is maximally kept. The algorithm starts from a random order or PCA. Compares the difference between real distance and the distance within the ordination space (this difference is called “stress”). Often used nowadays because computing is not limiting any more. A rule of thumb: stress ca 0.05 means an excellent representation in reduced dimensions, 0.1 is great, 0.2 is satisfactory, and stress >0.3 means a poor representation.

# We also explore how to limit clusters and draw species richness on the ordination graph.
o.mds <- metaMDS(comm.data, distance = "euclidean", k = 3)
stressplot(o.mds, vegdist) # stress is the distance of points from the line! Looks fine!
plot(o.mds) # biplot, samples taxa
plot(o.mds$points, pch = o.grel) # only samples - ja see meie rist vales kohas on õiges kohas

ordihull(o.mds, o.grel, col = 1:5) # connecting clusters grupeerib visuaalselt

## Adding species diversity to plot
plot(o.mds$points, pch = o.grel, col = o.grel)
ordisurf(
  o.mds,
  diversity(comm.data),
  col = "grey",
  main = "Shannon diversity",
  add = T
) #võta mu ordinatsioon ja ploti üle diversity data halli värvi ja ära tee uut ploti vaid lisa eelmisele (see add= T). joonistab merelained ja me näeme shannonit - suurem number rohkem diversityt

ordihull(o.mds, o.grel, col = 1:5) #teeb jälle kriipsud ümber
legend(
  "bottomleft",
  legend = unique(o.grel),
  col = unique(o.grel),
  pch = unique(o.grel),
  lwd = 1
)

#6.6 3D visualizations
library(vegan3d)
## Warning: package 'vegan3d' was built under R version 4.5.1
## Registered S3 methods overwritten by 'vegan3d':
##   method            from
##   plot.orditkplot   vegan
##   points.orditkplot vegan
##   scores.orditkplot vegan
##   text.orditkplot   vegan
##
## Attaching package: 'vegan3d'
## The following objects are masked from 'package:vegan':
##
##     orditkplot, panel.ordi3d, prepanel.ordi3d

ordiplot3d(o.mds, type = "h", pch = o.grel, col = o.grel)
ordirgl(o.mds, col = o.grel, pch = o.grel) # Should open a new window!


save(o.mds, file = "ordi.rda")
