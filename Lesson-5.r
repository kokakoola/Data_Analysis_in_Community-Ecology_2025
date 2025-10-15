# 5 Community (dis)similarity
# Here we explore how similar ecological communities are. With this, we mean how much their species composition overlaps. Dissimilarity is just the opposite of similarity, both measures carry the same information.

# 5.1 Reading and transforming community data
# Let’s read our community data and explore abundance data. Because abundance data tends to be skewed (few species have high abundance and many have low abundance), it is often reasonable to transform abundance to get a better distribution.

# Reading previously saved data
load("files/community.rda", verbose = T) # loading from previous
comm.data <- vas.plants # copy of a community data

# Most abundant species
max.spp <- which.max(colSums(comm.data))
max.spp

hist(comm.data[comm.data > 0]) # histogram of requencies (other than 0)

hist(sqrt(comm.data[comm.data > 0])) # square root transformation

hist(log1p(comm.data[comm.data > 0])) # log (x+1) transformation

# We can see that log-tranformation gives approximately normal distribution, it downweights frequent taxa.

# 5.2 Community distance matrix
# We can measure dissimilarity by using several indices. Here are some examples.

# Euclidean distance: kvanitattiivsed andmed, nt biomass, katvus

# 𝑑𝑗𝑘=∑𝑛𝑖=1(𝑥𝑖𝑗−𝑥𝑖𝑘)2‾‾√

# where 𝑥
#  is abundance of species 𝑖
#  in sites 𝑗
#  and 𝑘
#  and 𝑛
#  is the total number of taxa.

# Bray-Curtis distance: liigiline koosseis ja ökol. andmestik, mitte tundlik nullidele

# 𝑑𝑗𝑘=∑𝑛𝑖=1|𝑥𝑖𝑗−𝑥𝑖𝑘|∑𝑛𝑖=1(𝑥𝑖𝑗+𝑥𝑖𝑘)

# Euclidean distance does not have an upper limit, whereas Bray-Curtis distance is bounded between 0 and 1.

# With the vegdist function we can calculate various distance measures. It returns a triangular distance matrix, because distance from sample A to B is the same as from B to A.

library(vegan)

vegdist(comm.data[1:5, ], "euclidean") # triangular distance matrix

# Calculating some distances and ploting against each other.

vd1 <- vegdist(comm.data, "euclidean")
vd2 <- vegdist(log1p(comm.data), "euclidean") # log transformation
vd3 <- vegdist(log1p(comm.data), "bray")
pairs(cbind(vd1, vd2, vd3))

# 5.3 Hierachical clustering
# Using a distance matrix we can perform a clustering of our samples. Initially all samples form their own clusters, then we start to join the most similar sites and form clustering trees.

# Single linkage is based on the most similar members of two clusters. Complete linkage is based on the most dissimilar member of two clusters. Average linkage is based on calculating the average similarity between all members. Ward method is more complex, aiming to minimize the variance within clusters. You can check the Wildi book for more details.

# If we have a hierarchical cluster tree, we can always cut this to any number of clusters.
# •	Single linkage – kipub “venitama” andmeid → ei sobi sageli ökosüsteemide jaoks.
# •	Complete linkage – tihti liiga konservatiivne, kõik klastriliikmed peavad olema lähestikku.
# •	Average linkage (UPGMA) – kompromiss, sageli sobib, eriti liigirikkuse andmetele.
# •	Ward.D2 – minimeerib rühmasisest variatiivsust, kõige populaarsem, kui andmed on kvantitatiivsed ja normaaljaotusega (või standardiseeritud).

vd <- vd2 # selecting a distance matrix for future calculations
o.clu.s <- hclust(vd, method = "single") # kõige sarnasemad järjekorras abc aga c ei pruugi üldse olla sarnane a-ga!
o.clu.c <- hclust(vd, method = "complete") # kõige erinevamad
o.clu.a <- hclust(vd, method = "average") #Klastrite kauguseks võetakse kõigi liikmete vahekauguste aritmeetiline keskmine kahe klastri vahel. Lihtne, töötab hästi korrelatsioonidel või Bray–Curtis kaugustel. Võib tekitada venitatud jaotusi, tundlik ebavõrdsele hajumisele.
o.clu.w <- hclust(vd, method = "ward.D2") # see Eucleidian. Klastreid liidetakse nii, et väheneks klastritevaheline hajuvus (variance). Kasutab ruutkauguseid (e.g. Euclidean^2). Tekitab kompaktseid ja sarnase suurusega klastri; hea struktuuri leidmiseks. Vajab Euclidean kaugust, ei tööta hästi Bray–Curtis või teiste mitte-Euclidean mõõtudega.

par(mfrow = c(2, 2)) # several figures together 2 rows and 2 columns!

plot(as.dendrogram(o.clu.s), main = "single")
plot(as.dendrogram(o.clu.c), main = "complete")
plot(as.dendrogram(o.clu.a), main = "average")
plot(as.dendrogram(o.clu.w), main = "ward.D2")

o.clu <- o.clu.w # selecting the most logical (Ward linkage)

par(mfrow = c(1, 1)) # single figure again.

## Cutting tree to parts
o.grel <- cutree(o.clu, k = 5)
plot(as.dendrogram(o.clu))
rect.hclust(o.clu, 5, border = "red")

# Similarity between sites (colors), ordered along clusters
image(
  as.matrix(vd)[order(o.grel), order(o.grel)],
  asp = T,
  col = hcl.colors(8, palette = "viridis")
)

# 5.4 k-means clustering
# Not hierarchical – just give number of clusters needed. Computation is complex, based on machine learning and iterations.
k.o <- kmeans(comm.data, 5)
k.o$cluster
image(
  as.matrix(vd1)[order(k.o$cluster), order(k.o$cluster)],
  asp = T,
  col = hcl.colors(8, palette = "viridis")
)

# 5.5 How many clusters?
# Above we defined 5 clusters but can we find how many clusters are optimal? One method is to inspect at what number of groups the correlation between the real distance matrix between sites and the distance matrix between clusters maximizes. This distance in clusters can only include 0 (in the same cluster) and 1 (in different clusters) but we can still calculate the correlation.

# Lets define a vector for correlations
correls <- numeric() # making a numeric vector

for (i in 2:(nrow(comm.data) - 1)) {
  # loop for possible cluster numbers. We do not use 1 (all in the same cluster) and number of samples (all in different clusters)

  clusters <- cutree(o.clu, k = i) # defining clusters
  clusdist <- vegdist(table(1:30, clusters), "bray") # dist calculates Bray distance
  # distance is 0 (in different cluster) or 1 (same cluster)
  # table(1:30,clusters) makes a table of 30 sites vs clusters, 1 if a site is in a cluster

  correls[i] <- cor(vd, clusdist) # correlation as a measure
}

plot(correls, type = "h", xlab = "No of clusers", ylab = "Correlation")
max.corr <- which.max(correls) # which is max correlation?
points(max.corr, correls[max.corr], pch = 16, col = "red", cex = 2)
axis(side = 1, at = max.corr, labels = max.corr, col.axis = "red")

# Now we will compare how the clusters that we defined using maths are related to human-defined forest types.
forest.types # this vector contains the forest type each site is categorized as
image(
  as.matrix(vd1)[order(forest.types), order(forest.types)],
  asp = T,
  col = hcl.colors(8, palette = "viridis")
)

table(o.grel, forest.types) # cross-table

table(k.o$cluster, forest.types)

table(k.o$cluster, o.grel)

# Fisher exact test of two groups
fisher.test(table(o.grel, forest.types))
##
##  Fisher's Exact Test for Count Data
##
## data:  table(o.grel, forest.types)
## p-value = 6.84e-07
## alternative hypothesis: two.sided
fisher.test(table(k.o$cluster, forest.types))
##
##  Fisher's Exact Test for Count Data
##
## data:  table(k.o$cluster, forest.types)
## p-value = 2.452e-08
## alternative hypothesis: two.sided
fisher.test(k.o$cluster, o.grel)
##
##  Fisher's Exact Test for Count Data
##
## data:  k.o$cluster and o.grel
## p-value = 5.902e-08
## alternative hypothesis: two.sided

# 5.7 Which species define clusters?
# We want to know which species actually define clusters. There are several indices which show how well species differ between clusters. Here we use the IndVal Index. There are ways to calculate statistical significance as well for different clusters.

install.packages("indicspecies")
library(indicspecies) ## indikaatorliigid

# IndVal index (species indicator values)

indval <- multipatt(comm.data, o.grel, func = "IndVal")
indval$sign

# For example, let’s see which species are sinificantly indicating cluster 1:
subset(indval$sign, indval$sign$s.1 == 1 & indval$sign$p.value < 0.05)

# 5.8 Clustering also taxa
# Sometimes we might want to cluster taxa which ofter co-occur. For that we need to transpose our sites x taxa matrix using the function t.

comm.data.2 <- log1p(comm.data[, colSums(comm.data > 0) > 3]) # omitting rare taxa which cannot co-occur much anyway

tdis <- vegdist(t(comm.data.2), "euclidean")
tdis.clus <- hclust(tdis, method = "ward.D2")
plot(as.dendrogram(tdis.clus))

## Plotting both dendrograms together!

tabasco(comm.data.2, o.clu, tdis.clus)

## Saving for future!
save(o.grel, file = "files/clusters.rda")
