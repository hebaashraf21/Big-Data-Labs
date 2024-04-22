# Donia Gameel  1_24
# Heba Ashraf   2_32


# Install the arules and arulesViz packages
install.packages("arules")
install.packages("arulesViz")



#(1) Clean the workspace
rm(list = ls())

# Set the working directory
setwd("E:\\CMP4\\LAAAASSSTT TTTEEEERRRMM\\Big Data\\Lab 6 - Descriptive Analysis\\Association Rules")


#(2) Load the libraries
library(arules)
library(arulesViz)


#(3) Read the transactions from the CSV file
transactions <- read.transactions("AssociationRules.csv", format = "basket", header = FALSE)


#(4) Display the first 100 transactions
inspect(transactions[1:100])


#(5) Find the most frequent two items and their frequencies
item_freq <- itemFrequency(transactions, type = "absolute")
top_items <- head(sort(item_freq, decreasing = TRUE), 2)
top_items


#(6) Plot the 5 most frequent items
itemFrequencyPlot(transactions, topN = 5, main = "Top 5 Most Frequent Items")


#(7) Generate association rules
rules <- apriori(transactions, parameter = list(support = 0.01, confidence = 0.5, minlen = 2))


#(8) Sort the rules by support
sorted_rules_support <- sort(rules, by = "support", decreasing = TRUE)
# Show the first 6 rules
inspect(sorted_rules_support[1:6])


#(9) Sort the rules by confidence
sorted_rules_confidence <- sort(rules, by = "confidence", decreasing = TRUE)
# Show the first 6 rules
inspect(sorted_rules_confidence[1:6])

#(10) Sort the rules by lift
sorted_rules_lift <- sort(rules, by = "lift", decreasing = TRUE)
# Show the first 6 rules
inspect(sorted_rules_lift[1:6])

#(11) Plot the rules
plot(rules, shading = "lift")

# (12) what are the most interesting rules that are really useful
#      and provide a real business value and an insight to the concerned corporate?




# the most interesting rules that are likely to provide real business value and insights are those with high lift values. 
# Lift measures how much more likely the consequent (rhs) is, given the antecedent (lhs), compared to if the two were independent.

#Looking at the rules sorted by lift:
  
#{item15, item30, item56} => {item49} with lift 19.42
#{item30, item56, item84} => {item49} with lift 18.66
#{item15, item30, item49} => {item56} with lift 16.58
#{item15, item56} => {item49} with lift 15.42
#{item15, item49} => {item56} with lift 14.88
#{item30, item49, item84} => {item56} with lift 13.79










