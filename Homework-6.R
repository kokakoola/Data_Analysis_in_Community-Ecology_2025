# 1. Using the ground layer bryophytes data (bry.tab.txt), explore  two different ordination techniques to show the similarity patterns of sites.

# 2. Present an optimal graph that is both readable and informative. Show clusters and diversity on the graph.

# 3. Prepare a heatmap of the bryophyte table using the function tabasco. Try different ordination methods.

bry.tab <- read.table("files/bry.tab.txt", header = T, sep = " ")
comm.data <- (log1p(bry.tab))

o.pca <- prcomp(comm.data) # PCA
plot(scores(o.pca), asp = 1, type = "o")

summary(o.pca)
pairs(scores(o.pca)[, 1:4], pch = 16, cex = 0.5)

o.pca$sdev
o.pca$rotation[, 1:2]

o.pca$x[, 1:2]
