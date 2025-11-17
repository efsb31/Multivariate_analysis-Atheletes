library(dplyr)
library(aplpack)
library(randomcoloR)


Data <- read.csv("C:\\Users\\erinb\\OneDrive\\uni\\Y3_uni\\MA3MML\\MML1\\ST3MMLsport.csv")

Data <- Data %>% select(last_col(), everything())

wonn <- Data %>% select(-sex,-sport)

SportDFs <- split(Data, Data[[1]])

attach(wonn)

#means
lapply(wonn,mean)

#histograms
par(mfrow = c(4,3))
lapply(names(wonn), function(col) {
  hist(wonn[[col]],
       main = paste("Histogram of", col),
       xlab = col,
       col = "lightblue",
       border = "white")
})

# pairs

pairs(wonn)


#chernoff
labels <- paste(Data[,1],1:50 )   


cols <- rainbow(ncol(wonn))

wonn_norm <- as.data.frame(
  lapply(wonn, function(x) (x - min(x)) / (max(x) - min(x)) + 0.2)
)

stars(wonn_norm,
      draw.segments = TRUE,
      col.segments = cols,
      scale = FALSE,
      labels = labels,
      main = "Star Plot of Athletes Data")

legend("topright",
       legend = colnames(wonn),
       fill = cols,
       cex = 0.7,
       border = NA)

#cluster analysis



