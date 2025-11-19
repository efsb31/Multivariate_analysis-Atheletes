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

png("CVA_bi_plot.png", width = 10, height = 5, units = "in", res = 300)

par(mar=c(4,4,2,2))
#biplots
cols <- c(
  "B_Ball" = "red",
  "Gym"    = "purple",
  "Row"    = "blue",
  "Swim"   = "turquoise",
  "T_Sprnt"= "orange"
)

plot(cva_out,
     col = cols,
     pch = 19,
     main = "Canonical variate biplot:CV1 vs CV2",
     xlim = c(-9,8))

legend("topright",
       legend = names(cols),
       col = cols,
       pch = 19,
       title = "Sport")

dev.off()

#CVA attempt 2

png("CVA_bi_plot.png", width = 10, height = 5, units = "in", res = 300)

fit <- manova(cbind(rcc, wcc, hc, hg, ferr, bmi, ssf, pcBfat, lbm, ht, wt) ~ sport,
              data = Data)

cva_out <- candisc(fit)

# Extract scores and loadings
scores   <- cva_out$scores
loadings <- cva_out$structure[, 1:2]   # CV1 + CV2

# Colour map
cols <- c(
  "B_Ball" = "red",
  "Gym"    = "purple",
  "Row"    = "blue",
  "Swim"   = "turquoise",
  "T_Sprnt"= "orange"
)

cols2 <- c(
  "B_Ball" = "#8B0000",   # dark red
  "Gym"    = "#551A8B",   # dark purple
  "Row"    = "#00008B",   # dark blue
  "Swim"   = "#00868B",   # dark turquoise
  "T_Sprnt"= "#FF8C00"    # dark orange
)


# Axis limits 
xlim <- c(-10,4)
ylim <- range(scores$Can2) * 1.1

par(mar = c(4,4,2,2))
# Plot ---
plot(scores$Can1, scores$Can2,
     col  = cols[ Data$sport ],
     pch  = 19,
     cex  = 1.2,
     xlab = "Canonical Variate 1",
     ylab = "Canonical Variate 2",
     main = "CVA Biplot: CV1 vs CV2",
     xlim = xlim,
     ylim = ylim)

sports <- unique(scores$sport)

for(s in sports) {
  this_group <- scores[scores$sport == s, ]
  
  dataEllipse(this_group$Can1, this_group$Can2,
              levels = 0.68,   
              add = TRUE,
              col = cols[s],   
              plot.points = FALSE,
              lwd = 2)
  
}

# Draw loadings
arrow_scale <- 4   # adjust if arrows too large/small

for(i in 1:nrow(loadings)) {
  arrows(0, 0,
         loadings[i,1] * arrow_scale,
         loadings[i,2] * arrow_scale,
         length = 0.1)
}

# Add variable labels
text(loadings[,1] * arrow_scale,
     loadings[,2] * arrow_scale,
     labels = rownames(loadings),
     pos = 3)

# Compute centroid positions for each sport
centroids <- aggregate(cbind(Can1, Can2) ~ sport, data = scores, mean)

# Add centroids 
points(centroids$Can1, centroids$Can2,
       pch = 4, col = cols2,lwd = 4, cex = 2)

# Add centroid labels
text(centroids$Can1, centroids$Can2,
     labels = centroids$sport, pos = 3, cex = 0.9)



# Add legend
legend("topright",
       legend = names(cols),
       col    = cols,
       pch    = 19,
       bty    = "n",
       title  = "Sport")

dev.off()




#scores plot

cols <- c(
  "B_Ball" = "red",
  "Gym"    = "purple",
  "Row"    = "blue",
  "Swim"   = "turquoise",
  "T_Sprnt"= "orange"
)


plot(scores$Can1, scores$Can2,
     col = cols[Data$sport],
     pch = 19,
     xlab = paste0("CV1 (", round(100 * cva_out$percent[1], 1), "%)"),
     ylab = paste0("CV2 (", round(100 * cva_out$percent[2], 1), "%)"),
     main = "Canonical Variate Scores Plot")

group_means <- aggregate(scores[,1:2], list(Data$sport), mean)
points(group_means$CV1, group_means$CV2,
       pch = 3, cex = 2, lwd = 2, col = cols[group_means$Group.1])


legend("topright",
       legend = names(cols),
       col = cols,
       pch = 19,
       title = "Sport")


eig <- cva_out$eigenvalues
prop <- eig / sum(eig) * 100
round(cbind(eig, prop), 2)

coeffs <- cva_out$coeffs.std
round(coeffs[,1:2], 3) 