library(dplyr)

Data <- read.csv("C:\\Users\\erinb\\OneDrive\\uni\\Y3_uni\\MA3MML\\MML1\\ST3MMLsport.csv")

Data <- Data %>% select(last_col(), everything())

wonn <- Data %>% select(-sex,-sport)



#histograms
par(mfrow = c(4,3))
lapply(names(wonn), function(col) {
  hist(wonn[[col]],
       main = paste("Histogram of", col),
       xlab = col,
       col = "lightblue",
       border = "white")
})

