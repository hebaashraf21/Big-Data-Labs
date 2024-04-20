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




#The most interesting rules are rules with high confidence, support, and lift.
#Examples:

#[1] High Confidence Rule: {item15, item49, item56} => {item30}

# Confidence: 1.0000000  Support: 0.0101  Lift: 3.022975
# This rule suggests that if items 15, 49, and 56 are purchased together,
# there is a high likelihood that item 30 will also be purchased. This could indicate a specific product bundle or promotional strategy.


#[2] High Support Rule: {item5} => {item13}

# Confidence: 0.5074344  Support: 0.1877 Lift: 1.025534
# This rule indicates that item 5 and item 13 are frequently purchased together,
# which could inform product placement or marketing strategies.

#[3] High Lift Rule: {item15, item30, item56} => {item49}
# Confidence: 0.7709924  Support: 0.0101  Lift: 19.42046
# This rule suggests a strong association between items 15, 30, and 56 leading to the purchase of item 49. This could be valuable for cross-selling or product bundling strategies.












