# 1. Read a table for another group of taxa (ground layer bryophytes) from the same 30 forest sites that we have used in the class:  bry.tab.txt (available from files) and find a suitable hierarchical clustering. Explain your choice of methods.

# From last class to make the code work as standlaone
load("files/community.rda", verbose = T) # loading from previous
comm.data <- vas.plants # copy of a community data

library(here) #just trying here out.
bry.tab <- read.table(here("files", "bry.tab.txt"), header = T, sep = " ") #useless.

## Checking data
all.equal(rownames(bry.tab), rownames(comm.data)) #controlling whether colnames match. "30 string mismatches"
rownames(comm.data) <- gsub("^X", "", rownames(comm.data)) # [1] "2 string mismatches"
rownames(bry.tab)[rownames(bry.tab) != rownames(comm.data)][1]
# "006 Erumae" but the task says we need SAME forest sites. and the last task uses vas.plants (available in comm.data)
common <- intersect(rownames(bry.tab), rownames(comm.data))
bry.tab <- bry.tab[common, , drop = FALSE]
comm.data <- comm.data[common, , drop = FALSE]
all.equal(rownames(bry.tab), rownames(comm.data))
# TRUE

hist(comm.data[bry.tab > 0]) #no normal distribution
hist(log1p(comm.data[bry.tab > 0])) #no normal distribution
hist(sqrt(comm.data[bry.tab > 0])) #no normal distribution
which.max(colSums(bry.tab)) #do we have any dominant species? Hylocomium.splendens 4 - naah, its fine. ecological data, we'll use bray what doesn't need normal distribution

library(vegan)
dist <- vegdist(bry.tab, method = "bray") #bray is suitable method for ecological data.

clust.a <- hclust(dist, method = "average") # average is balanced and suitable for bray in general.

#But better to be sure...
methods <- c("single", "complete", "average", "ward.D2")
par(mfrow = c(2, 2))
for (i in seq_along(methods)) {
  clust <- hclust(dist, method = methods[i])
  plot(as.dendrogram(clust), main = methods[i])
} #average looks good

#How many clusters? I don't trust my eyes to count them
groups <- cutree(clust.a, k = 5)
group_dist <- vegdist(table(1:nrow(bry.tab), groups), method = "euclidean")
mantel(dist, group_dist, method = "spearman") # For 5 clusters bray I get best correlation: Mantel statistic r: 0.6987650 Significance:0.001

## Checking whether correlation could be even better:
mantel_results <- numeric(length(methods))

for (i in seq_along(methods)) {
  clust <- hclust(dist, method = methods[i])
  groups <- cutree(clust, k = 5)
  group_dist <- vegdist(table(1:nrow(bry.tab), groups), method = "euclidean")
  mantel_test <- mantel(dist, group_dist, method = "spearman")
  mantel_results[i] <- mantel_test$statistic
}

# Adding names
names(mantel_results) <- methods
mantel_results
# single  complete   average   ward.D2
# 0.5804210 0.5831328 0.6987650 0.5942466

# Average is the best. But actually it was the second task!

# 2. Find the optimal number of clusters for bryophytes when using k-means clustering (NOTE: in the class we did this for the vascular plants dataset with hierarchical clustering).

correls <- numeric() # making a numeric vector

for (i in 2:(nrow(bry.tab) - 1)) {
  clusters <- cutree(clust, k = i) # defining clusters
  clusdist <- vegdist(table(1:28, clusters), "bray") # dist calculates Bray distance
  correls[i] <- cor(dist, clusdist) # correlation as a measure
}
plot(correls, type = "h", xlab = "No of clusers", ylab = "Correlation")
max.corr <- which.max(correls) # which is max correlation?
points(max.corr, correls[max.corr], pch = 8, col = "red", cex = 2)
axis(side = 1, at = max.corr, labels = max.corr, col.axis = "red")

# 5 it is!

# Using k-means clustering, test how does the match between the clusters of vascular plants and bryophtyes changes. For this, you should use the function fisher.test() setting the argument simulate.p.value = T (otherwise the calculations will be too heavy when a large number of clusters is considered) and look how does the p value changes with the number of clusters considered. Indicate for what number of clusters the match is strongest (lowest p value). [NOTE: To simplify, you can use the same number of clusters for vascular plants and bryophytes (e.g. 3 clusters in plants vs 3 clusters in bryophytes, 4 clusters vs 4 clusters, etc)]
