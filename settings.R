# settings

# dependencies
if(!require(shiny)) install.packages("shiny")
if(!require(randomForest)) install.packages("randomForest")
if(!require(devtools)) install.packages("devtools")
if(!require(shinyIncubator)) devtools::install_github("shiny-incubator", "rstudio")

#  for test diagnostics
p.significant<-0.05

# for context mining
acc.rf.default<-.7
top.k<-5
class.ratio<-2 # class-imbalance learning

# for hypothesis mining
min.sup.cij<-10