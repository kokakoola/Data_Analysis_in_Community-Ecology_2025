# 1. Read a table for another group of taxa (ground layer bryophytes) from the same 30 forest sites that we have used in the class:  bry.tab.txt (available from files) and find a suitable hierarchical clustering. Explain your choice of methods.

# From last class to make the code work as standlaone
load("files/community.rda", verbose = T) # loading from previous
comm.data <- vas.plants # copy of a community data

bry.tab <- read.table("files/bry.tab.txt", header = T, sep = " ")

## Preparing data for analyze
all.equal(rownames(bry.tab), rownames(comm.data)) #controlling whether colnames match. "30 string mismatches"
rownames(comm.data) <- gsub("^X", "", rownames(comm.data)) # [1] "2 string mismatches"
rownames(bry.tab)[rownames(bry.tab) != rownames(comm.data)][1]
# "006 Erumae" but the task says we need same forest sites. and the last task uses vas.plants (available in comm.data)
common <- intersect(rownames(bry.tab), rownames(comm.data))
bry.tab <- bry.tab[common, , drop = FALSE]
comm.data <- comm.data[common, , drop = FALSE]
all.equal(rownames(bry.tab), rownames(comm.data))
# TRUE

# Normality check
hist(bry.tab[bry.tab > 0]) #not very normal
hist(log1p(bry.tab[bry.tab > 0])) #pretty normal distribution - I'll go with that
hist(sqrt(bry.tab[bry.tab > 0])) #no normal distribution

#Most abundant
which.max(colSums(bry.tab)) #do we have any dominant species? Hylocomium.splendens

## Choosing distance calculation method
library(vegan)
dist.b <- vegdist(log1p(bry.tab), method = "bray") #bray is suitable method for ecological data.

#But better to be sure...
methods <- c("single", "complete", "average", "ward.D2")
par(mfrow = c(2, 2)) #side-by-side view

for (i in seq_along(methods)) {
  clust <- hclust(dist.b, method = methods[i])
  plot(as.dendrogram(clust), main = methods[i])
} #average looks good, lets go with that

clust.a <- hclust(dist.b, method = "average") # average is balanced and suitable for bray in general.

#How many clusters? Trying out with different k-values and here is the best outcome
groups <- cutree(clust.a, k = 5)
group_dist <- vegdist(table(1:nrow(bry.tab), groups), method = "euclidean")
mantel(dist.b, group_dist, method = "spearman") # For 5 clusters bray I get best correlation: Mantel statistic r: 0.7306 Significance:0.001

#Visualizing clusters
par(mfrow = c(1, 1)) #back to single view
plot(as.dendrogram(clust.a))
rect.hclust(clust.a, 5, border = "red") ##looks like 5-7

## Checking whether correlation could be even better, mantel again, tried out values 5-7
mantel_results <- numeric(length(methods))

for (i in seq_along(methods)) {
  clust <- hclust(dist.b, method = methods[i])
  groups <- cutree(clust, k = 7)
  group_dist <- vegdist(table(1:nrow(bry.tab), groups), method = "euclidean")
  mantel_test <- mantel(dist.b, group_dist, method = "spearman")
  mantel_results[i] <- mantel_test$statistic
}

names(mantel_results) <- methods # Adding names
mantel_results
# single  complete   average   ward.D2
# 0.5832393 0.5653713 0.7305282 0.5653713

# k=7. Actually k=5-7 gives pretty much the same result with Average

####--------------------------------------------------------------------

# 2. Find the optimal number of clusters for bryophytes when using k-means clustering (NOTE: in the class we did this for the vascular plants dataset with hierarchical clustering).

correls <- numeric() # making a numeric vector

for (i in 2:(nrow(bry.tab) - 1)) {
  clusters <- cutree(clust, k = i) # defining clusters
  clusdist <- vegdist(table(1:28, clusters), "bray") # dist calculates Bray distance
  correls[i] <- cor(dist.b, clusdist) # correlation as a measure
}
plot(correls, type = "h", xlab = "No of clusers", ylab = "Correlation")
max.corr <- which.max(correls) # which is max correlation?
points(max.corr, correls[max.corr], pch = 8, col = "red", cex = 2)
axis(side = 1, at = max.corr, labels = max.corr, col.axis = "red")
# 7 clusters when visualized

####--------------------------------------------------------------------

# 3. Using k-means clustering, test how does the match between the clusters of vascular plants and bryophtyes changes. For this, you should use the function fisher.test() setting the argument simulate.p.value = T (otherwise the calculations will be too heavy when a large number of clusters is considered) and look how does the p value changes with the number of clusters considered. Indicate for what number of clusters the match is strongest (lowest p value). [NOTE: To simplify, you can use the same number of clusters for vascular plants and bryophytes (e.g. 3 clusters in plants vs 3 clusters in bryophytes, 4 clusters vs 4 clusters, etc)]

pvals <- numeric() #empty object
set.seed(181025) # repeatable answers

for (i in 2:(nrow(bry.tab) - 1)) {
  # iterate reasonable amount (like 1 cant't be cluster)
  km1 <- kmeans(comm.data, i)
  km2 <- kmeans(bry.tab, i) # k-means for both dataset
  test <- fisher.test(
    table(km1$cluster, km2$cluster),
    simulate.p.value = TRUE
  )
  pvals[i] <- test$p.value #p-s for each, starting from index
}

pvals
best.k <- which.min(pvals)
best.k
# [1] 5
