# 1. Using the ground layer bryophytes data (bry.tab.txt), explore  two different ordination techniques to show the similarity patterns of sites.

# useful helpers during lesson
par(mfrow = c(1, 2)) #visualisation side-by side
par(mfrow = c(1, 1)) # reset view area

# load data
bry.tab <- read.table("files/bry.tab.txt", header = T, sep = " ")
bry.data <- log1p(bry.tab)

## PCA method to find out how much variance is descibed by component
library(vegan)
bry.pca <- prcomp(bry.data)
summary(bry.pca) # 70% is covered on PC4, so actually 4 components should be evaluated to get relevant outcome.
summary(bry.pca)$importance[, 1:2]
# PC1      PC2
# Standard deviation     0.9403036 0.649348
# Proportion of Variance 0.3603900 0.171870
# Cumulative Proportion  0.3603900 0.532250
# 2 components represent only 53% of variance

pairs(scores(bry.pca)[, 1:4], pch = 16, cex = 0.5, col = rgb(0, 0, 1, 0.3))

## describe distances for further work
brydist <- vegdist(bry.data, method = "bray") ## for the dissimilarity matrix. Bray method was validated as most suitable in last homework

## describe clusters for further work
set.seed(191025)
k.bry <- kmeans(bry.data, 5) # the cluster number is from last homework
clu.bry <- k.bry$cluster # saving clusters for future use in ordinations

## frequent species for plots
freqs <- colSums(bry.data > 0)
freq.bry <- order(freqs, decreasing = TRUE)[1:5]

# PCOA method
library(ape)
bry.pcoa <- pcoa(brydist)

plot(
    bry.pcoa$vectors[, 1], ## vector for PC1
    bry.pcoa$vectors[, 2], ## vector for PC2
    asp = 1,
    pch = clu.bry, ## grouping
    xlab = "PCO1",
    ylab = "PCO2"
)

plot(scores(bry.pca)[, 1:2], asp = 1, pch = clu.bry) ## plotted site locations, clusters with different symbols. just for taking a look
legend("topleft", legend = unique(clu.bry), pch = unique(clu.bry)) #the form of the clusters is quite visible and well defined

x <- bry.pca$rotation[freq.bry, 1] # eigenvectors for frequent bryophytes
y <- bry.pca$rotation[freq.bry, 2]
plot(x, y, type = "n", asp = 1, xlab = "PC1", ylab = "PC2")
abline(h = 0, v = 0, col = "gray")
arrows(0, 0, x, y, length = 0.03, col = "red")
text(x, y, colnames(bry.data)[freq.bry], cex = 0.6) ## visualise abundant species correlation

## Correspondence analysis method
bry.ca <- cca(bry.data)
summary(bry.ca)$cont$importance[, 1:3]
bry.ca$CA$v[, 1:2]
plot(bry.ca, display = "species")
plot(bry.ca, display = "sites")
bry.data["007Porgumae", "Eurynchium..Sp.2"] #[1] 0.09531018
# 007 Porgumae and Eurynchium Sp2 are really far from others and won't fit well into general ecological pattern. It reveals strong assotiation between the site and species and indicates that the species was only there or dominant.
which(bry.data[, "Eurynchium..Sp.2"] > 0) #indeed, Eurynchium 2 lives only in Porgumae.
bry.data["007Porgumae", ] # with another Eurynchium and no other species are available in Porgumae.

plot(bry.ca, type = "n")
points(bry.ca, display = "sites", cex = 0.8, pch = clu.bry, col = "red")
text(bry.ca, display = "species", cex = 0.7, col = "blue", select = freq.spp)

plot(bry.ca, type = "n", xlim = c(-2, 2), ylim = c(-2, 1))
points(bry.ca, display = "sites", cex = 0.8, pch = 16, col = "red")
ordipointlabel(
    bry.ca,
    cex = 0.7,
    display = "species",
    col = "blue",
    add = T,
    select = bry.spp
) # the further away from zero the more selective is the species. Close red dots show similarity in species. The axises are the main components from PCA, x axis more important. So the further is species or spot from zero the more different it is, the higher is variation.
points(bry.ca, display = "sites", col = clu.bry, pch = 16)
legend(
    "topright",
    legend = paste("Cluster", 1:5),
    col = 1:5,
    pch = 16,
    cex = 0.8
) # now we see the previously defined clusters what were used in NMDS as well on graph

## No good data for Correspondence analysis as the distribution is not unimodal, the distances are too uneven. But interesting.

# 2. Present an optimal graph that is both readable and informative. Show clusters and diversity on the graph.

## NMDS method
bry.mds <- metaMDS(bry.data, distance = "euclidean", k = 3) ## the stress with 3 dimensions is approx 0.09 what is a representative value
brydist <- vegdist(bry.data, method = "bray") ## for the distance the bray was best in last lesson
stressplot(bry.mds, brydist) # nonmetric fit R2 = 0,993 indicates that 99% of rank-based structure of the original dissimilarities is preserved in the ordination - what is excellent outcome

plot(bry.mds) #not visually very informative
plot(bry.mds$points, pch = clu.bry, col = clu.bry) #groupig by clusters makes it well readable

ordihull(bry.mds, clu.bry, col = clu.bry)
legend(
    "bottomleft",
    legend = unique(clu.bry),
    col = unique(clu.bry),
    pch = unique(clu.bry),
    lwd = 1
)

ordisurf(
    bry.mds,
    diversity(bry.data),
    col = "black",
    main = "Shannon diversity",
    add = T
) ## adding shannon to plot to show the diversity in the groups

# The graph separates visible clusters and represents diversity via Shannon lines

# 3. Prepare a heatmap of the bryophyte table using the function tabasco. Try different ordination methods.
tabasco(bry.data, bry.ca)
tabasco(bry.data, bry.pca)
tabasco(bry.data, bry.mds)
