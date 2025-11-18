library(dplyr)
library(aplpack)
library(randomcoloR)

library(corrplot)


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

vars <- c("ferr", "bmi")


png("histogram_plot.png", width = 12, height = 3, units = "in", res = 300)
par(mfrow = c(1,2), mar=c(4,4,2,1))
lapply(vars, function(col) {
  hist(wonn[[col]],
       main = paste("Histogram of", col),
       xlab = col,
       cex = 0.5,
       col = "lightblue",
       border = "white")
})

dev.off()



# pairs

# ---- 1) Pairs plot ----
png("pairs_plot.png", width = 24, height = 12, units = "in", res = 300)

cols <- c(
  "B_Ball"  = "red",
  "Row"     = "blue",
  "Swim"    = "turquoise",
  "T_Sprnt" = "orange",
  "Gym"     = "purple"
)


par(mar = rep(0.2, 4))
pairs(
  Data[, 2:12],
  col = cols[Data$sport],
  pch = 4,
  bg = NA,
  lwd = 3,
  cex = 3,
  main = "Pairs plot (coloured by sport)",
  cex.labels = 4,
  cex.main = 3
)

dev.off()


plot.new()   

legend("topright",
       #inset = c(0,-0.01),
       legend = names(cols),
       fill = cols,
       horiz = TRUE,
       cex = 1,
       border = NA)



png("corr_heatmap.png", width = 7, height = 7, units = "in", res = 300)
par(mar = c(2,2,1,2))
corrplot(M,
         method = "color",
         tl.cex = 0.8,
         title = "Correlation heatmap",
         mar = c(0,0,2,0))
dev.off()




png("correlation_plot.png", width = 12, height = 3, units = "in", res = 300)
par(mfrow = c(1,2), mar=c(4,4,2,1))

cols <- c("B_ball" = "red",
          "Row"     = "blue",
          "Swim"   = "turquoise",
          "T_Sprnt"    = "orange",
          "Gym"        = "purple")

pairs(Data[,2:12],  # choose your numeric columns
      col = cols[Data$sport],
      pch = 16)



corrplot(cor(wonn), method = "color", tl.cex = 0.8)


dev.off()
#chernoff


labels <- paste(Data[,1],1:50 )   


cols <- rainbow(ncol(wonn))
cols <- c(
  "#D7191C", "#FDAE61",,
  "#1A9641", "#66BD63", "#A6D96A",
  "#2C7BB6", "#ABD9E9", "#74ADD1",
  "#4575B4", 
)

cols <- c(
  # Blood-related (5): red → orange
  "#B30000",  # dark red (rcc)
  "#FF4D00",  # red (wcc)
  "#FF8000",  # red-orange (hc)
  "#FFB300",  # orange (hg)
  "#FEE08B",  # light orange (ferr)
  
  "#009900",  # green (deep, not neon)
  "#33CC33",  # light green
  "#00A6A6",  # teal (distinct from both greens + blues)
  "#0073E6",  # bright blue
  "#003399",  # deep blue
  "#4B0082"   # indigo (high-contrast end of spectrum)
)

wonn_norm <- as.data.frame(
  lapply(wonn, function(x) (x - min(x)) / (max(x) - min(x)) + 0.2)
)


png("glyph_plot.png", width = 8, height = 4.5, units = "in", res = 300)
par(mfrow=c(5,10), mar=c(0.1,0.5,0.1,0.5))   # 5 rows, 10 columns

for(i in 1:50){
  stars(wonn_norm[i, , drop=FALSE],
        draw.segments = TRUE,
        col.segments = cols,
        scale = FALSE,
        labels = labels[i],
        cex = 1,
        main = "")
}

dev.off()

plot.new()   

legend("topright",
       #inset = c(0,-0.01),
       legend = colnames(wonn),
       fill = cols,
       horiz = TRUE,
       cex = 1.5,
       border = NA)


#cluster analysis



