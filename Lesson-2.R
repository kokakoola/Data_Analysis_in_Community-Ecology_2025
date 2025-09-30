a = 3
diversity = c(18, 27, 10, 22, 25, 8, 12)
soil.ph = c(4.5, 6.4, 3.9, 5.3, 5.9, 3.2, 4.2)
soil.type = c(
  "mineral",
  "mineral",
  "peat",
  "mineral",
  "mineral",
  "peat",
  "peat"
)

## salvesta objekt faili
save(diversity, soil.ph, soil.type, file = "my.data.Rda")
?rm

## call saved data
load("my.data.Rda")
ls()

## salvesta skript faili
a = 5
print("Hello something")

## kutsu skript välja
source("2.R")
a

## if(condition) {code to execute}
soil = "peat"
soil = "mineral"

if (soil == "peat") {
  print("Wet weather")
} else {
  print("Foo")
}

if (soil == "peat") {
  # commands for conditions are between {}
  print("Has been wet")
}

answer = ifelse(soil.type == "peat", "wet", "dry") # conditions, if true, if not
print(answer)
## [1] "dry" "dry" "wet" "dry" "dry" "wet" "wet"

## ifelse
plot(soil.ph, diversity, col = ifelse(soil.type == "peat", "brown", "black"))

## for loop
soil.type

for (i in 1:7) {
  print(soil.type[i])
}

for (i in soil.type) {
  print(i)
}

## conditional object
soils.high.div = NULL
for (i in 1:length(diversity)) {
  if (diversity[i] > 20) soils.high.div = c(soils.high.div, soil.type[i])
}
soils.high.div
diversity
soil.type

## my wrong way
soil.type == "mineral"

## same without loop
soils.high.div <- soil.type[diversity > 20]
soils.high.div

### data frames
my.data = data.frame(soil.ph, soil.type, diversity)
my.data

## structure
str(my.data)
names(my.data)
colnames(my.data)
rownames(my.data)
head(my.data)

## beware of data type!
as.factor((my.data$soil.type))
as.character(my.data$soil.type)
as.numeric(my.data$soil.type)

## access cols, rows
my.data[1:5, 3] ## bad to use, you forget what is what
my.data[, "soil.type"]
my.data[c("diversity", "soil.type")]

## read and explore text file
envir = read.table("files/envir.txt", header = T, sep = " ", row.names = 1)
envir
ncol(envir)
nrow(envir)
dim(envir) # dimensions
row.names(envir)
names(envir) ## same as prev
str(envir) #structure
head(envir)
summary(envir)

## access data
envir[1:4, 6:12]

my.cols <- c(6:9, 5) # ph, N, P, K and site nime
soil.data <- envir[, my.cols]
soil.data

## remove some
head(envir[, -c(1:3, 4, 6:12)])
c(1:3, 4, 6:12)

## logical operators
my.rows <- envir$pH.KCl > median(envir$pH.KCl)
my.rows

high.ph.soils <- soil.data[my.rows, ]
low.ph.soils <- soil.data[!my.rows, ]

#Using the subset function (the same as prev)
high.ph.soils1 <- subset(soil.data, pH.KCl > median(pH.KCl))
high.ph.soils == high.ph.soils1
all.equal(high.ph.soils, high.ph.soils1)

## use sample function
set.seed(170925) ## set seed helps to get same random. Usually Date.
sample(6) ##random numbers
sample(nrow(soil.data)) ## random order

rand.soils <- soil.data[sample(nrow(soil.data)), ] ## generate random soils
rand.soils

my.coordinates <- envir[, c("lon", "lat", "Proovi.nimi")]
write.table(my.coordinates, file = "soordinates.txt")
write.table(soil.data, file = "soil.data.csv", sep = ";", row.names = F)

### Packages
install.packages("readxl")
library(readxl)
test <- read_excel("files/excel.xlsx")
test
test1 <- as.data.frame(test) ##normalize xsl
test1
