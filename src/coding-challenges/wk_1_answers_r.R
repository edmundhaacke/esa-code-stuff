# Easy Challenge
# Create a function that takes two arguments: the original price and the discount percentage as integers and returns the final price after the discount.
 
# Examples:
# discount(1500, 50) ➞ 750
# discount(89, 20) ➞ 71.2
# discount(100, 75) ➞ 25
 
# Notes:
# Your answer should be rounded to two decimal places.

dis <- function(price, discount){
    return(round(price * (1 - discount / 100), 2))
}

print(dis(1500, 50))
print(dis(89, 20))
print(dis(100, 75))

# Medium Challenge:
# Create a function that converts a date formatted as DD/MM/YYYY to YYYYDDMM.

# Examples:
# format_date("11/12/2019") ➞ "20191211"
# format_date("31/12/2019") ➞ "20193112"
# format_date("15/01/2019") ➞ "20191501"

# Notes:
# Return value should be a string.


format_date <- function(date){
    d = strsplit(date, "/")[[1]]
    return(paste0(d[3], d[2], d[1]))
}

print(format_date("11/12/2019"))
print(format_date("31/12/2019"))
print(format_date("15/01/2019"))

# Hard Challenge
# An identity matrix is defined as a square matrix with 1s running from the top left of the square to the bottom right. The rest are 0s. The identity matrix has applications ranging from machine learning to the general theory of relativity.
# Create a function that takes an integer n and returns the identity matrix of n x n dimensions. For this challenge, if the integer is negative, return the mirror image of the identity matrix of n x n dimensions.. It does not matter if the mirror image is left-right or top-bottom.
# Examples:
# id_mtrx(2) ➞ [
#   [1, 0],
#   [0, 1]
# ]
# id_mtrx(-2) ➞ [
#   [0, 1],
#   [1, 0]
# ]
# id_mtrx(0) ➞ []

# Notes:
# Incompatible types passed as n should return the string "Error".

id_mtrx <- function(n){
    if (!is.integer(n)) return("Error")
    wh = abs(n)
    mtrx = unlist(lapply(seq(1, wh), function(i){
        rw = unlist(lapply(seq(1, wh), function(j){
            return(ifelse(i==j, 1, 0))
        }))
        if (n < 0){
            rw = rev(rw)
        }
        return(rw)
    }))
    return(matrix(mtrx, ncol=wh, nrow=wh, byrow=TRUE))
}

print(id_mtrx(10L))
print(id_mtrx(-10L))
