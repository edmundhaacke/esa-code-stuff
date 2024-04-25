# Easy
# Create a function that takes two numbers and a mathematical operator + - / * and will perform a calculation with the given numbers.
# 
# Examples
# calculator(2, "+", 2) ➞ 4
# calculator(2, "*", 2) ➞ 4
# calculator(4, "/", 2) ➞ 2
# 
# Notes
# If the input tries to divide by 0, return: "Can't divide by 0!"

calculator<-function(value1,operator,value2){
  value<-case_when(operator=="+"~ value1+value2,
            operator=="-"~ value1-value2,
            operator=="*"~ value1*value2,
            operator=="/" & value2==0 ~ NA,
            operator=="/" & value2!=0 ~ value1/value2
        )
  case_when(is.na(value)~"Can't divide by 0!",
            !is.na(value)~ as.character(value))
  
}

calculator(2,"*",0)


# Medium
# You work for a manufacturer, and have been asked to calculate the total profit made on the sales of a product. 
# You are given a dictionary containing the cost price per unit (in dollars), sell price per unit (in dollars), 
# and the starting inventory. Return the total profit made, rounded to the nearest dollar.
# 
# Examples
# profit({
#   "cost_price": 32.67,
#   "sell_price": 45.00,
#   "inventory": 1200
# }) ➞ 14796
# 
# profit({
#   "cost_price": 225.89,
#   "sell_price": 550.00,
#   "inventory": 100
# }) ➞ 32411
# 
# profit({
#   "cost_price": 2.77,
#   "sell_price": 7.95,
#   "inventory": 8500
# }) ➞ 44030
# 
# Notes
# Assume all inventory has been sold.
# Profit = Total Sales - Total Cost


profit<-function(dictionary){
  cost_price<-dictionary[["cost_price"]]
  sell_price<-dictionary[["sell_price"]]
  inventory<-dictionary[["inventory"]]
  round((sell_price-cost_price)*inventory,0)

  }

profit(list(cost_price=32.67,sell_price=45.00,inventory=1200))


# Hard:
# Write a function that returns True if a string consists of ascending or ascending AND consecutive numbers.
 
# Examples
# ascending("232425") ➞ True
# (Consecutive numbers 23, 24, 25)

# ascending("2324256") ➞ False
# (No matter how this string is divided, the numbers are not consecutive)
 
# ascending("444445") ➞ True
# (Consecutive numbers 444 and 445)
 
# Notes
# A number can consist of any number of digits, so long as the numbers are adjacent to each other, and the string has at least two of them.

ascending <- function(s){
    # break down string into different length batches
    for (n in 1:(floor(nchar(s)/2) + 1)){
        # create list of s split into lengths
        grps <- unlist(lapply(seq(0, nchar(s), n), function(i){
            return(substr(s, (i+1), (i+n)))
        }))
        # remove any blanks
        grps <- grps[grps != ""]
        # check at least 2 elements in 
        if (length(grps) < 2) next;
        # iterate through each element, and check whether the next element is the ascended
        # version of the current element
        is_asc <- unlist(lapply(seq(1, length(grps)), function(i){
            # check within bounds
            if (i + 1 > length(grps)) return(TRUE)
            return((as.integer(grps[i]) + 1) == (as.integer(grps[i+1])))
        }))
        all_asc <- Reduce("&", is_asc)
        if (all_asc) return(TRUE)
    }
    return(FALSE)
}

print(ascending("232425"))
print(ascending("2324256"))
print(ascending("444445"))