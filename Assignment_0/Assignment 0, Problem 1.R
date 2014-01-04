#Assignment 0, Problem 1
import1 <- "/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_0/sort_data.txt"
table1 <- read.table(file=import1)
newmatrix <- data.matrix(table1)
sortthematrix <- sort(newmatrix)
finalsortedmatrix <- matrix(sortthematrix,10,10)
finalsortedmatrix
