# Read the file describing ground bryophytes from forest sites: bry.tab.txt. Check if the file has been loaded correctly.

bryophytes <- read.table(
  "files/bry.tab.txt",
  header = T,
  sep = " ",
  check.names = FALSE
)

colnames(bryophytes) <- gsub("\\.", " ", colnames(bryophytes))

# Find evenness of (1) bryophytes, (2) vascular plants, (3) all plants (i.e. vascular plants and bryophytes combined).

## Read table
vasplants <- read.table(
  "files/vascular.plants.txt",
  header = T,
  check.names = FALSE
)

## Correct tables to match
vasplants <- t(vasplants) #swap row-col
rownames(vasplants) <- gsub("^X", "", rownames(vasplants)) #remove extra X
setdiff(row.names(vasplants), row.names(bryophytes)) #check datasets equality. [1] "006.Erumae" "073Pausakunnu". 073Pausakunnu and 073Sulaoja are very different locations. Too bad.
row.names(vasplants) <- gsub("\\.", " ", row.names(vasplants)) ## correct . in  006
identical(order(rownames(vasplants)), order(rownames(bryophytes))) ## Check order - TRUE
common <- intersect(row.names(vasplants), row.names(bryophytes)) ## remove location 073

## Clean data to work with and evnenness-calculations
vasplants <- vasplants[common, ]
bryophytes <- bryophytes[common, ]
allplants <- cbind(vasplants, bryophytes)

fun.even <- function(x) {
  richness <- rowSums(x > 0)
  diversity.shannon <- diversity(x, index = "shannon")
  evenness <- diversity.shannon / log(richness)
  return(evenness)
}
even.bryophytes <- fun.even(bryophytes)
even.vasplants <- fun.even(vasplants)
even.allplants <- fun.even(allplants)

# Perform a correlation test to see if vascular plant evenness and bryophyte evenness values are correlated. Select the proper type of correlation (Pearson or Spearman).

shapiro.test(even.vasplants) # W = 0.84834, p-value = 0.000703
shapiro.test(even.bryophytes) # W = 0.8472, p-value = 0.0006664

log.vas <- log(even.vasplants)
log.bryo <- log(even.bryophytes) # some are 0

log.vas <- log(even.vasplants + 1e-6)
log.bryo <- log(even.bryophytes + 1e-6) # better

shapiro.test(log.vas)
shapiro.test(log.bryo) # no normality still, Pearson is out

cor.test(even.vasplants, even.bryophytes, method = "spearman")
## There is a moderate positive correlation between evenness in bryophytes and vascular plants (Spearman’s ρ = 0.44, p = 0.017)
