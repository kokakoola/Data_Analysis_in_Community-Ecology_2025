## Read file soil.data.csv to object "soil". Check if the file has been read correctly.

soil <- read.csv("soil.data.csv", header = TRUE, sep = ";")
head(soil)

## Using a for loop, make three graphs side-by-side showing how soil pH is related to soil nutrients (N, P and K). Make axes labels. Hint: explore the argument mfrow in the function par() to define the graph locations.
pH <- soil$pH.KCl
N <- soil$N..
P <- soil$P.mg.kg
K <- soil$K.mg.kg

par(mfrow = c(1, 3))

NPK <- list(N = N, P = P, K = K)

for (name in names(NPK)) {
    plot(pH, NPK[[name]], ylab = name)
}

## Read the file problematic.txt (available in Moodle). Use the read.table() function with parameters skip, header, and sep. Explore variable types and correct individual values if necessary. Note that numeric values from factor can be done through text! Prepare a scatterplot with diversity and soil.pH, and a boxplot diversity vs. soil type.

data <- read.table(
    "files/problematic.txt",
    header = TRUE,
    skip = 2,
    sep = ",",
    stringsAsFactors = FALSE
)
summary(data)

data$soil.ph <- trimws(data$soil.ph)
data$soil.ph <- as.numeric(data$soil.ph)
which(is.na(data$soil.ph))
dput(data$soil.ph[7]) ## I still have a NA!

data$diversity <- trimws(data$diversity)
data$diversity <- as.numeric(data$diversity)
which(is.na(data$diversity)) ## I still have a NA!

par(mfrow = c(1, 2))
plot(data$soil.ph, data$diversity, xlab = "Soil pH", ylab = " Diversity")
boxplot(
    data$diversity ~ data$soil.type,
    xlab = "Soil type",
    ylab = "Diversity "
)
