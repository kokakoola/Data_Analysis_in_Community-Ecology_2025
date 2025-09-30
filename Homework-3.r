#In the excel file “trees.xlsx”, tree diameters have been measured in two angles (D1 and D2). Sometimes the tree was too large to measure the diameter and then the circumference was measured and entered to column D1, preceeded with “ü” (from Estonian word “ümbermõõt” = circumference). Prepare a script which calculates tree basal area (area of cross-section).

library(readxl)
trees <- read_xlsx("files/trees.xlsx")
str(trees) ## checking format of cols. D1 is char :(

D1 <- trees$D1 ## saving col as variable to manipulate separately

fun.d <- function(x) {
  return(x / pi)
} ## rehearsing function

D1.corrected <- ifelse(
  grepl("^ü", D1),
  fun.d(as.numeric(substring(D1, 2))),
  as.numeric(D1)
) ## cleanup of D1: remove "ü" +  calculate diameter instead circumferernce +  convert to numeric

trees$D1 <- D1.corrected ## replace D1 in table with corrected column

trees$D.mean <- rowMeans(trees[, c("D1", "D2")], na.rm = TRUE) ## means of D1, D2 +  remove NA in D2
trees$basal.area <- pi * (trees$D.mean / 2)^2 ## Basal area

#Prepare a table sites x tree species, which contain the basal area sums. Note that ala = site, puuliik = species (codes of tree species). Convert the table to data.frame using the function as.data.frame.matrix! Read the file with soil data done in the demonstration “soil.data.csv” and store it in an object called "soil". Check if the observations in your tree area matrix and soil data are in the same order.

tree_matrix <- xtabs(
  basal.area ~ ala + puuliik,
  data = trees
)
tree_df <- as.data.frame(tree_matrix)

soil <- read.csv("files/soil.data.csv", sep = ";")
head(soil)
head(tree_df)

all(substring(rownames(tree_df), 4) == soil$ala) ## TRUE!

#Split the table with tree basal areas to two groups, sites with soil pH > 3.5 and the rest. In both tables, remove species which only have zeros (i.e. do not occur in any of the sites of the group). Find tree species which only grow at low soil pH. Use our own function tables.join to merge the splitted tables together. Check if the data is identical to that of the initial table.

high_ph <- tree_df[soil$pH > 3.5, ]
low_ph <- tree_df[soil$pH <= 3.5, ]

filtered_high_ph <- subset(high_ph, Freq > 0)
filtered_low_ph <- subset(low_ph, Freq > 0)

species_low <- setdiff(filtered_low_ph$puuliik, filtered_high_ph$puuliik)

tables.join <- function(t1, t2) {
  # asking two tables which got names t1 and t2
  t2[, setdiff(names(t1), names(t2))] = 0
  t1[, setdiff(names(t2), names(t1))] = 0
  t12 = rbind(t1, t2)
  return(t12) # returns value from function
}

joined_again <- tables.join(high_ph, low_ph)
identical(joined_again, tree_df) ## FALSE.
all.equal(joined_again, tree_df)
## [1] "Attributes: < Component “row.names”: Mean relative difference: 0.4885332 >"
## [2] "Component “ala”: 330 string mismatches"
## [3] "Component “puuliik”: 330 string mismatches"
## [4] "Component “Freq”: Mean relative difference: 1.603313"
