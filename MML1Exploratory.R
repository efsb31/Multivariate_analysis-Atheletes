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


means_list <- lapply(SportDFs, function(df) {
  numeric_df <- df[sapply(df, is.numeric)]   # keep only numeric columns
  sapply(numeric_df, mean, na.rm = TRUE)
})


overall_means <- colMeans(Data[, -which(names(Data) %in% c("sport","sex"))])
overall_means

aggregate(. ~ sport, data = Data[, -which(names(Data) == "sex")], mean)

#manova
# Split already done:
SportDFs <- split(Data, Data[[1]])

# But MANOVA uses the full dataset:
Data$Sport <- factor(Data[[1]])   # make sure sport is a factor

# Keep only numeric variables
numeric_df <- Data[sapply(Data, is.numeric)]

# Fit MANOVA
manova_result <- manova(as.matrix(numeric_df) ~ Sport, data = Data)

# Summary (Pillai's trace recommended)
summary(manova_result, test = "Pillai")



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

par(mfrow=c(4,13), mar=c(0.2,0.2,0.2,0.2))   # 5 rows, 10 columns

for(i in 1:50){
  stars(wonn_norm[i, , drop=FALSE],
        draw.segments = TRUE,
        col.segments = cols,
        scale = FALSE,
        labels = labels[i],
        cex = 1.5,
        main = "")
}

plot.new()   

legend("topright",
       #inset = c(0,-0.01),
       legend = colnames(wonn),
       fill = cols,
       cex = 1.3,
       border = NA)


#cluster analysis



