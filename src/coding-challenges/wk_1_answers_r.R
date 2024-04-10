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

format_date <- function(date){
    d = strsplit(date, "/")[[1]]
    return(paste0(d[3], d[2], d[1]))
}

print(format_date("11/12/2019"))
print(format_date("31/12/2019"))
print(format_date("15/01/2019"))

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