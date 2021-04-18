##market basket analysis on groceries dataset 
## reference: https://www.datacamp.com/community/tutorials/market-basket-analysis-r

install.packages("Rtools", dependencies = TRUE)
#install and load package arules
install.packages("arules", dependencies = TRUE)
## arules - Provides the infrastructure for representing, manipulating and 
## analyzing transaction data and patterns (frequent itemsets and association rules).

## arulesviz - Extends package 'arules' with various visualization techniques for association rules and itemsets. 
## The package also includes several interactive visualizations for rule exploration.

library(arules)
#install and load arulesViz
#install.packages("arulesViz", dependencies = TRUE)
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
#install and load readxml
#install.packages("readxml", dependencies = TRUE)
library(readxl)
#install and load knitr
#install.packages("knitr", dependencies = TRUE)
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate", dependencies = TRUE)
library(lubridate)
#install and load plyr
#install.packages("plyr", dependencies = TRUE)
library(plyr)
library(dplyr)

## get 1,500 obs with 35 variables columns named x1 + 
retail <- read_csv('Groceries_original.csv', col_names = FALSE)


## put a comma after date to help separate column into two separate columns there 
## are three years 2000,2001 and 2002 in this column so that I can use the separate command
retail$X1 <- sub(pattern = '2000', replacement = '2000,', x = retail$X1)
retail$X1 <- sub(pattern = '2001', replacement = '2001,', x = retail$X1)
retail$X1 <- sub(pattern = '2002', replacement = '2001,', x = retail$X1)

## remove redundant first row

retail <- retail[2:1500, ]


retail <- retail %>%
  separate(X1, c("date", "X1"), sep = ",") ## 99 rows filled with NAs 

summary(retail)
str(retail)

## Create a shopper column from row ID as will need it when using the melt function below (reshape2)
## found following tidyverse command in this url:
## https://stackoverflow.com/questions/23518605/add-an-index-numeric-id-column-to-large-data-frame/23518737
## make a column from row ID 
retail <- tibble::rowid_to_column(retail, "Shopper")

library(reshape2)  ## needed for melt function


retailm <- melt(retail, id = c("Shopper", "date"))  ## assume, melt takes all columns except those specified in id parameter
names(retailm) <- c("Shopper", "Date", "ItemNo", "Item")

## remove x from ItemNo column and replace with "ItemNo_1
retailm$ItemNo <- sub(pattern = 'X', replacement = 'ItemNo_', x = retailm$ItemNo)

## remove the NAs with 
## checking first how many NAs there are and where they are - all in Item column 
sapply(retailm, function(x) sum(is.na(x)))- ## (23,402 NAs present)

## Cab use following method to look at NAs and confirm their presence 
retail_NC <- retailm[!complete.cases(retailm), ] ## 23,402 out of 52465 are NA rows so 29063 are complete

## remove the NAs by filtering on complete cases in rows
retailm <- retailm[complete.cases(retailm), ] ## 29,063 obs remaining as complete

str(retailm)

## remove redundant column
retailm$variable <- NULL

## following creates transaction file 
transactionData <- ddply(retailm, c("Shopper","Date"), function(df1)paste(df1$Item, collapse = ","))


## create a new data object retailOne with all rows represeting a shopping basket, so can now get rid of Shopper and Date column as no longer needed
retailOne <- transactionData
retailOne$Shopper <- NULL
retailOne$Date <- NULL
names(retailOne) <- c("Item")



write.csv(retailOne,"groceries_D.csv", quote = FALSE, row.names = FALSE)


#"groceries_D": location of file with file name to be written to
#write() command quote: If TRUE it will surround character or factor column with double quotes. If FALSE
#nothing will be quoted #row.names: either a logical value indicating whether the row 
#names of x are to be written along with x, or a character vector of row names to be written.


##Next, need to convert data into an object of the transaction class. 
## This is done by using the R function read.transactions command of the arules package.


trR <- read.transactions('groceries_D.csv', format = 'basket', sep=',')


summary(trR) ## 27 items and summary list how often each of the 27 items are purchased

install.packages("RColorBrewer", dependencies = TRUE)

## row 111 of MBA DataCamp tutorial

# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

par(mfrow = c(1,1))
library(RColorBrewer)
itemFrequencyPlot(trR, topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

itemFrequencyPlot(trR,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")



##**************************************************************

## Next Step - generating rules 


####**********************************************************
####*
## Next step is to mine the rules using the APRIORI algorithm. The function apriori() is from package arules.


# Min Support as 0.001, confidence as 0.8.
association.rules1_default <- apriori(trR, parameter = list(supp=0.1, conf=0.8, maxlen=10)) ## 443 rules



##The apriori will take trR as the transaction object on which mining is to be applied. 
#parameter will allow you to set min_sup and min_confidence. The default values for parameter are 
##minimum support of 0.1, the minimum confidence of 0.8, maximum of 10 items (maxlen).
summary(association.rules1_default) ## giving 443 rules, 5 of rule length 2 and 438 of rule length 3

inspect(association.rules1_default[1:10])
inspect(association.rules1_default[11:20])


inspect(association.rules1_0.8[11:20])

## found another example in stackoverflow for apriori 
## rules <-  apriori(trans, parameter = list(support = 0.001, confidence = 0.5, maxlen = 2))
## https://stackoverflow.com/questions/37077774/arules-package-error-subscript-out-of-bounds-for-producing-recommendations

# create multiple association rules
# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005, 0.0025, 0.001)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)


# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)
rules_sup0.25 <- integer(length=9)
rules_sup0.1 <- integer(length=9)

# Apriori algorithm with a support level of 10% - no rules returned
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(trR, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}


# Apriori algorithm with a support level of 5% - no rules returned
for (i in 1:length(confidenceLevels)){
  
  rules_sup5[i] <- length(apriori(trR, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}
# Apriori algorithm with a support level of 1% 244 rules returned
for (i in 1:length(confidenceLevels)){
  
  rules_sup1[i] <- length(apriori(trR, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.5% == 2313 rules returned
for (i in 1:length(confidenceLevels)){
  
  rules_sup0.5[i] <- length(apriori(trR, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
}

## suport 0.25%
for (i in 1:length(confidenceLevels)){
  
  rules_sup0.25[i] <- length(apriori(trR, parameter=list(sup=supportLevels[5], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

## support 0.1%
for (i in 1:length(confidenceLevels)){
  
  rules_sup0.1[i] <- length(apriori(trR, parameter=list(sup=supportLevels[6], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}

## don't know why doing target = 'rules' in next line
## association.rules2 <- apriori(trR, parameter = list(support=0.003, conf=0.5, maxlen=2, target = 'rules'))


##rules_sup10 <- integer(length=9)
##rules_sup5 <- integer(length=9)
##rules_sup1 <- integer(length=9)
##rules_sup0.5 <- integer(length=9)
##rules_sup0.25 <- integer(length=9)
##rules_sup0.1 <- integer(length=9)

summary(rules_sup10) ## 
summary(rules_sup5) ## 
summary(rules_sup1)  ## 
summary(rules_sup0.5)
summary(rules_sup0.25)
summary(rules_sup0.1)

summary(association.rules1)
summary(association.rules2) ## 38 rules
summary(association.rules3)
summary(association.rules4)

basket.sorted <- sort(association.rules, by = "lift")
basket.sorted2 <- sort(association.rules2, by = "lift")
basket.sorted3 <- sort(association.rules3, by = "lift")

##Since there are 29064 rules, let's print only top 10:


inspect(basket.sorted3[1:10])
inspect(basket.sorted3[,])

inspect(sort(association.rules3, by = 'lift')[1:20])


## How can you limit the size and number of rules generated? You can do this by setting parameters
##in apriori. You set these parameters to adjust the number of rules you will get. If you want stronger
##rules, you can increase the value of conf and for more extended rules give higher value to maxlen.

shorter.association.rules <- apriori(trR, parameter = list(supp=0.001, conf=0.8, maxlen=3))


inspect(shorter.association.rules[,])

## remove redundant rules - that are subsets of larger rules 

subset.rules2 <- which(colSums(is.subset(association.rules2, association.rules2)) > 0) # get subset rules in vector
length(subset.rules)  #> 3913 - I get 44014 but same on row in screenshot

summary(subset.rules2)
length(association.rules2)
length(subset.rules2)
subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.

vegetables.association.rules2

## which() returns the position of elements in the vector for which value is trRUE.

##  colSums() forms a row and column sums for dataframes and numeric arrays.

##  is.subset() Determines if elements of one vector contain all the elements of other



## Finding Rules related to given items
## Sometimes, you want to work on a specific product. If you want to find out what causes influence 
## on the purchase of item X you can use appearance option in the apriori command. 
## appearance gives us options to set LHS (IF part) and RHS (THEN part) of the rule.

## To find out what customers buy before buying vegetables run the following line of code: 

vegetables.association.rules <- apriori(trR, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs", rhs="vegetables"))

## inspect vegetables associations

inspect(head(vegetables.association.rules))

##Similarly, to find the answer to the question Customers who bought vegetables also bought.... 
##you will keep vegetables on lhs:

## notice that you now say ,lhs = "vegetables" and default="rhs" instead of default = "lhs" and rhs = "vegetables" - notice lhs is first and 
vegetables.association.rules2 <- apriori(trR, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="vegetables",  default="rhs"))

## do another head inspect()

inspect(head(vegetables.association.rules2))




## SCATTER PLOT ### association.rules1_default

# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
subRules<-association.rules1_default[quality(association.rules1_default)$confidence>0.4]

#Plot SubRules
plot(subRules)


##plot(rulesObject, measure, shading, method)

##rulesObject: the rules object to be plotted

##measure: Measures for rule interestingness. Can be Support, Confidence, lift or combination of these depending upon method value.

##shading: Measure used to color points (Support, Confidence, lift). The default is Lift.

##method: Visualization method to be used (scatterplot, two-key plot, matrix3D).

plot(subRules, method="two-key plot")


## An amazing interactive plot can be used to present your rules that use arulesViz and plotly. You can hover over each rule and
## view all quality measures (support, confidence and lift).

plotly_arules(subRules)


## Let's select 10 rules from subRules having the highest confidence.

top10subRules <- head(subRules, n = 10, by = "confidence")

## now make it interactive

plot(top10subRules, method = "graph",  engine = "htmlwidget")

## From arulesViz graphs for sets of association rules can be exported in the GraphML format
## or as a Graphviz dot-file to be explored in tools like Gephi. 
## For example, the 1000 rules with the highest lift are exported by:

saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")


##Individual Rule Representation

## This representation is also called as Parallel Coordinates Plot. It is useful to visualized 
## which products along with which items cause what kind of sales.

## As mentioned above, the RHS is the Consequent or the item we propose the customer will buy; 
## the positions are in the LHS where 2 is the most recent addition to our basket and 
## 1 is the item we previously had.

# Filter top 20 rules with highest lift
subRules2 <- head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")

## Look at the topmost arrow. It shows that when I have 'CHILDS GARDEN SPADE PINK' and 
## 'CHILDS GARDEN RAKE PINK' in my shopping cart, I am likely to buy 'CHILDS GARDEN RAKE BLUE' 
## along with these as well.

### *****************************END****************************************

