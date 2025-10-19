# 1. Using the ground layer bryophytes data (bry.tab.txt), explore  two different ordination techniques to show the similarity patterns of sites.

# 2. Present an optimal graph that is both readable and informative. Show clusters and diversity on the graph.

# 3. Prepare a heatmap of the bryophyte table using the function tabasco. Try different ordination methods.

bry.tab <- read.table("files/bry.tab.txt", header = T, sep = " ")
bry.data <- log1p(bry.tab)


## PCA method
library(vegan)
bry.pca <- prcomp(bry.data)
summary(bry.pca) # 80% is covered on PC6, 90% at PC8
summary(bry.pca)$importance[, 1:2]
# PC1      PC2
# Standard deviation     0.9403036 0.649348
# Proportion of Variance 0.3603900 0.171870
# Cumulative Proportion  0.3603900 0.532250

pairs(scores(bry.pca)[, 1:4], pch = 16, cex = 0.5, col = rgb(0, 0, 1, 0.3))

bry.pca$sdev ## standard deviations for 1:4
bry.pca$rotation[, 1:2] ## eigenvectors for 1:2
bry.pca$x[, 1:2] ## site positions for 1:2

#visualisation
par(mfrow = c(1, 2))

set.seed(191025)
k.bry <- kmeans(bry.data, 5) # the cluster number is from last homework
clu.bry <- k.bry$cluster

plot(scores(bry.pca)[, 1:2], asp = 1, pch = clu.bry)
legend("topleft", legend = unique(clu.bry), pch = unique(clu.bry)) #the form of the clusters is quite visible and well defined
plot(scores(bry.pca))

freq.spp <- order(colSums(bry.tab), decreasing = TRUE)[1:5]

x <- bry.pca$rotation[freq.spp, 1]
y <- bry.pca$rotation[freq.spp, 2]
plot(x, y, type = "n", asp = 1, xlab = "PC1", ylab = "PC2")
abline(h = 0, v = 0, col = "gray")
arrows(0, 0, x, y, length = 0.03, col = "red")
text(x, y, colnames(bry.data)[freq.spp], cex = 0.6)

## NMDS method
bry.mds <- metaMDS(bry.data, distance = "euclidean", k = 3)
brydist <- vegdist(bry.data, method = "bray")
stressplot(bry.mds, brydist)

plot(bry.mds)
plot(bry.mds$points, pch = clu.bry)
ordihull(bry.mds, clu.bry, col = 1:5)

plot(bry.mds$points, pch = clu.bry, col = clu.bry)
ordisurf(
    bry.mds,
    diversity(bry.data),
    col = "grey",
    main = "Shannon diversity",
    add = T
)
ordihull(bry.mds, clu.bry, col = 1:5)
legend(
    "bottomleft",
    legend = unique(clu.bry),
    col = unique(clu.bry),
    pch = unique(clu.bry),
    lwd = 1
)

bry.ca <- cca(bry.data)
summary(bry.ca)$cont$importance[, 1:3]
quartz()
tabasco(bry.data, bry.ca, Colv = T)
dev.off()
tabasco(bry.data, bry.pca)
tabasco(bry.data, bry.mds)
