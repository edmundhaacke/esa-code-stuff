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
