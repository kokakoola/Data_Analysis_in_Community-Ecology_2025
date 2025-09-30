rm(list = ls()) #clear old data

xy = read.table("files/coordinates.txt")
soil.data = read.table("files/soil.data.csv", sep = ";", header = T)
high.ph.soils = subset(soil.data, pH.KCl > median(pH.KCl)) ## subset eraldab tellitud jupi
low.ph.soils = subset(soil.data, pH.KCl <= median(pH.KCl))

combine1 = cbind(xy, soil.data) # putting together columns (rbind for rows)
head(combine1)

combine2 = rbind(high.ph.soils, low.ph.soils) # putting together rows
dim(combine2) ##dimensions

combine3 = cbind(xy, combine2) # trying again, but ....
plot(combine1$lon, combine1$pH.KCl)
points(combine3$lon, combine3$pH.KCl, pch = "x", col = "green") # adding points, symbol "x"
head(combine3)

# The 'merge' function can help in merging two datasets that share a common column with unique identifiers, like the "Proovi.nimi" one in our example:
combine3 = merge(xy, combine2, by = "Proovi.nimi") # merging by common column
head(combine3)

## tunni osa, kapsas

xy[rows:left, col:right]
points(combine3$lon, combine3$pH.KCl, phc = "x", col = "red")
order()
combine4 <- cbind(
  xy[order(xy$Proovi.nimi), ],
  combine2[order(combine2$Proovi.nimi)]
)
points(combine4$lon, combine4$pH.KCl, pch = "+", col = "blue")
order(combine2$Proovi.nimi)

#siit alates ok
vas.plants <- read.table(("files/vascular.plants.txt")) #alati uuri, kas tabel on ikka korras mille importisid.
str(vas.plants)
dim(vas.plants)
rownames(vas.plants)
colnames(vas.plants)
vas.plants[1:5, 1:5]

#transpose
vas.plants <- t(vas.plants) ##keerab ümber rea/veeru
str(vas.plants) #strucure

vas.plants <- as.data.frame(vas.plants) ## common teha, korrastamiseks. kui muutuja üle kirjutad, ole kindel et kirjutad õigesti üle - su vana var hävib.
vas.plants[1:5, 1:5] # vaatamiseks et kas õnnestus

#kiire visualisatsioon andmetest
image(t(vas.plants), axes = F, ylab = "Sites", xlab = "Taxa")

apply(X = vas.plants, MARGIN = 1, F = max, simplify = TRUE) #calculate max values for each taxon in the object

library(readxl)

data <- read_excel("files/trees.xlsx")
str(data)
data <- as.data.frame(data)
head(data)

tree.counts <- table(data[, c(1, 4)])
head(data[, 1:4]) #seletab mis me eespool valisime, saab vaadata nii
head(data[, c(1, 4)])

tree.counts <- table(data[, c("ala", "puuliik")]) #sama asi, aga colnamedega et oleks selgem mis tehti
head(tree.counts)
str(tree.counts)
tree.counts[1:5, 1:5]

## xtabs() - formula(coverage/dominance)
tree.height <- xtabs(H ~ ala + puuliik, data = data)
str(tree.height)
head(tree.height)

summary(data)

#andmete korrastamine complete.cases()  vaatab kus reas on andmed puudu ja eemaldab juhendi järgi

complete.cases(data) #näitab False kui kuskil puudu


data[complete.cases((data)), ] #left part - rows

#uus muutuja kus on ainult terviklikud read:
data1 <- data[complete.cases((data)), ]
#kus on ainult False read:
data[!complete.cases(data), ]

# is.na()
is.na(data) #false tähendab ok ja true et jah NA
data[is.na(data$D2), ]

##väga äge kombineeritud plot community datast ja asukohtades
cbind(rownames(vas.plants), as.character(xy$Proovi.nimi))
i <- 3
plot(xy[, 1:2], cex = .3)
plot(xy[, c("lon", "lat")], cex = .3)
points(xy[, 1:2], cex = vas.plants[, i] * 2, col = "green")
title(main = colnames(vas.plants)[i])

colnames(vas.plants)[i]

sites1 <- row.names(vas.plants)
sites1

sites2 <- as.character((soil.data$Proovi.nimi))
sites2
nchar(sites2)

## substring()

substring(sites1, 2, 4) #take the letter of each element from position 2 to pos 4

## igale poole text vahele panna:
paste(1:30, sites2, sep = "-")

#grep ehk leia
grep("mae", sites1)
sites1[grep("mae", sites1)] #koma ei lähe lõppu sest see on 2-dimensionaalne ainult
sites1[grep("\\.", sites1)] #tee tühik ja pane punkt? ei saand aru

#asenda
gsub("mae", "oru", sites1) #asenda tekst teisega

substring(sites1, 2, 999) #alusta 2-st positsioonist ja kuni lõpuni, igaks juhuks 999
nchar((sites1)) #mitu tähte ikkagi on?

forest.types <- read.table("files/envir.txt")$forest.types
unique(forest.types)
table(forest.types) #mitmes kohas

forest.types.1 <- forest.types[soil.data$pH.KCl < 3]
forest.types.2 <- forest.types[soil.data$pH.KCl >= 3]
#...

my_function = function() {
  return(x)
}
x = "Hello World"
my_function()

t1 <- data.frame(a = c(1, 0, 1), b = c(2, 2, 0))
t2 = data.frame(b = c(3, 0), c = c(0, 4))

t2[, setdiff(names(t1), names(t2))] = 0
t1[, setdiff(names(t2), names(t1))] = 0
t1

tables.join <- function(t1, t2) {
  # asking two tables which got names t1 and t2
  t2[, setdiff(names(t1), names(t2))] = 0
  t1[, setdiff(names(t2), names(t1))] = 0
  t12 = rbind(t1, t2)
  return(t12) # returns value from function
}

t12 <- tables.join(t1, t2)

t12

## tables.join(t1 = a.table, t2 = b.table)
## tables.join(a.table, b.table)

save(
  vas.plants,
  tree.counts,
  forest.types,
  xy,
  soil.data,
  tables.join,
  file = "community.rda"
)
