5 + 7
(5 + 7) / 10
2.5 + 1.2
5 + 7
a = 5 + 7
a
A = a / 10
A
a = a + 1
a
my.var.1 = (2 + 8) + 4
my.var.1
sqrt(9)
log10(a)
max(a, b, c)
b = 3.8
4 = c
4 -> c
min(a, b, c)
my.logical = a == 12
my.logical
ls()
mode(my.logical)
my.logical1 = F
my.logical1
!my.logical
mode(my.logical)
mode(!my.logical)
mode(a)
my.text = "UT"
mode(my.text)
my.logical
my.logical1 = F
my.logical1
a = as.character()
mode(a)
2025 - 1991
today = 2025
independence = 1991
today - independence
mode(my.text)
mode(a)
my.logical = a == 12
my.logical
mode(my.logical)
my.logical1 = F
my.logical1
my.logical != my.logical1
!my.logical
as.numeric(a)
mode(a)
a <- as.character()
my.logical <- as.numeric()
my.vec1 = 1:6
my.vec2 = c(a, b, c, 1.2, 5.3, 2.2)
my.vec2

my.matrix = matrix(my.vec2, nrow = 3, ncol = 2)
a = 5 + 7
my.matrix
d = c(a, b, c)
d
length(my.vec2)
nrow(my.matrix)
dim(my.matrix)
seq(from = 1, to = 6, by = 0.2)
seq(from = 4, to = 6, length.out = 6)
rev(my.vec2)
rep(5, times = 3)
rep(1:3, each = 3)
single.vec = c(my.vec2, my.text)
single.vec

my.vec2 + 2
(my.matrix + 2) / my.vec2

min(my.vec2)
mean(my.vec2)
round(my.vec2)
my.vec2 - 1
my.vec2 + c(0, 0, 0, 100, 100, 100)
my.vec2 = my.vec2 + c(1, 0, 0, 0, 0, 0)
my.vec2
my.vec2 > 5
my.vec2 > 5 & my.vec1 < 3
any(my.vec2 > 10)
all(my.vec2 > 1)
rowSums(my.matrix)
colSums(my.matrix)
sum(my.vec2 <= 3)
my.vec2

## 1.8
my.vec2[1]
my.vec2[1] = 30
my.vec2[c(2, 4)] = c(22, 23)
my.vec2[my.vec2 > 10] = my.vec2[my.vec2 > 10] / 2

my.matrix[1, 1]
names(my.vec2) = c("a", "b", "c", "d", "e", "f")
my.vec2["c"]

## Functions
sqrt(9) # square root
## [1] 3
log10(a) # log based on 10
## [1] 1.113943
max(a, b, c)
## [1] 13
min(a, b, c)
## [1] 3.8
sum(a, b, c)
## [1] 20.8

## Object modes
mode(a)
## [1] "numeric"
my.text = "UT"
mode(my.text)
## [1] "character"
my.logical = a == 12
my.logical
## [1] FALSE
mode(my.logical)
## [1] "logical"
my.logical1 = F # you can use T for TRUE and F for FALSE!
my.logical1
## [1] FALSE

## Data dimensions
my.vec1 = 1:6 # a vector from 1 to 5
my.vec1
## [1] 1 2 3 4 5 6
my.vec2 = c(a, b, c, 1.2, 5.3, 2.2) # c is a function combine!
my.vec2
## [1] 13.0  3.8  4.0  1.2  5.3  2.2

my.matrix = matrix(my.vec2, nrow = 3, ncol = 2) # nrow and ncol are parameters
my.matrix
##      [,1] [,2]
## [1,] 13.0  1.2
## [2,]  3.8  5.3
## [3,]  4.0  2.2

length(my.vec2)
## [1] 6
nrow(my.matrix)
## [1] 3
dim(my.matrix) # number of rows and number of columns
## [1] 3 2

seq(from = 1, to = 6, by = 0.2) # more detailed sequences
##  [1] 1.0 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.4 3.6 3.8 4.0 4.2 4.4 4.6
## [20] 4.8 5.0 5.2 5.4 5.6 5.8 6.0
seq(from = 4, to = 6, length.out = 6)
## [1] 4.0 4.4 4.8 5.2 5.6 6.0
rev(my.vec2) # reversing
## [1]  2.2  5.3  1.2  4.0  3.8 13.0
rep(5, times = 3) # repeating
## [1] 5 5 5
rep(1:2, each = 3)
## [1] 1 1 1 2 2 2

### Manipulating vectors and matrices
my.vec2 + 2
## [1] 15.0  5.8  6.0  3.2  7.3  4.2
(my.matrix + 2) / my.vec2
##          [,1]     [,2]
## [1,] 1.153846 2.666667
## [2,] 1.526316 1.377358
## [3,] 1.500000 1.909091

# finding minimum, mean etc. from a vector or matrix
min(my.vec2)
## [1] 1.2
mean(my.vec2)
## [1] 4.916667
round(my.vec2)
## [1] 13  4  4  1  5  2

my.vec2 + c(0, 0, 0, 100, 100, 100)
## [1]  13.0   3.8   4.0 101.2 105.3 102.2

my.vec2 > 5
## [1]  TRUE FALSE FALSE FALSE  TRUE FALSE
my.vec2 > 5 & my.vec1 < 3 # both need to be true!
## [1]  TRUE FALSE FALSE FALSE FALSE FALSE

any(my.vec2 > 10)
## [1] TRUE
all(my.vec2 > 1)
## [1] TRUE

rowSums(my.matrix)
## [1] 14.2  9.1  6.2
colSums(my.matrix)
## [1] 20.8  8.7

### Accessing vectors and matrices

my.vec2
## [1] 13.0  3.8  4.0  1.2  5.3  2.2
my.vec2[1]
## [1] 13
my.vec2[4]
## [1] 1.2

my.vec2[1] = 30 # replacing a single value!
my.vec2
## [1] 30.0  3.8  4.0  1.2  5.3  2.2

my.vec2[c(2, 4)] = c(22, 23) # putting to position 2 value 22 and to position 4 value 23
my.vec2
## [1] 30.0 22.0  4.0 23.0  5.3  2.2

# Using a logical vector to make something

my.vec2 > 10 # where values are > 10
## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE

my.vec2[my.vec2 > 10] = my.vec2[my.vec2 > 10] / 2 # Replacing in my.vec2 all values > 10 with same values divided by 2
my.vec2
## [1] 15.0 11.0  4.0 11.5  5.3  2.2

my.matrix[1, 1] # first row number and then column number (in R always in this order, row then column)
## [1] 13

names(my.vec2) = c("a", "b", "c", "d", "e", "f") # assigning names
my.vec2
##    a    b    c    d    e    f
## 15.0 11.0  4.0 11.5  5.3  2.2
my.vec2["c"]
## c
## 4
my.vec2[3] # you can still use numbers as well!
## c
## 4

colnames(my.matrix) = c("c1", "c2")
row.names(my.matrix) = c("r1", "r2", "r3")
my.matrix
##      c1  c2
## r1 13.0 1.2
## r2  3.8 5.3
## r3  4.0 2.2
my.matrix["r2", "c2"]
## [1] 5.3
my.matrix[2, 2]
## [1] 5.3

which(my.vec2 > 5) # note that numbers are numerical indices not values!
## a b d e
## 1 2 4 5

my.vec2
##    a    b    c    d    e    f
## 15.0 11.0  4.0 11.5  5.3  2.2
sort(my.vec2) # note names now!
##    f    c    e    b    d    a
##  2.2  4.0  5.3 11.0 11.5 15.0
order(my.vec2) # indexes from low to high values
## [1] 6 3 5 2 4 1

my.vec2[order(my.vec2)]
##    f    c    e    b    d    a
##  2.2  4.0  5.3 11.0 11.5 15.0
