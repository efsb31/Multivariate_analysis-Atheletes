library(dplyr)


Data <- read.csv("C:\\Users\\erinb\\OneDrive\\uni\\Y3_uni\\MA3MML\\MML1\\ST3MMLsport.csv")

Data <- Data %>% select(last_col(), everything())

nData <- Data %>% select(-sex,-sport)

#PCA
pca <- prcomp(nData,scale. =TRUE)

plot(pca,type ="l")

summary(pca)

pca$rotation

scores <- pca$x

cols <- c("B_ball" = "red",
          "Row"     = "blue",
          "Swim"   = "turquoise",
          "T_Sprnt"    = "orange",
          "Gym"        = "purple")

plot(scores[,1],scores[,2],
     col = cols[Data$sport],
     pch = 19,
     xlab = "PC1",
     ylab = "PC2",
     main = "PCA: PC1 vs PC2")
legend("topright", legend = names(cols),
       col = cols, pch = 19)



#CVA
library(candisc)
library(car)

fit <- manova(as.matrix(Data[,2:12]) ~ sport, data = Data)

cva_out <- candisc(fit)

cva_out$coeffs

scores <- cva_out$scores

plot(cva_out)