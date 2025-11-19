library(dplyr)
library(candisc)
library(car)

Data <- read.csv("C:\\Users\\erinb\\OneDrive\\uni\\Y3_uni\\MA3MML\\MML1\\ST3MMLsport.csv")

#Data <- Data %>% select(last_col(), everything())

#nData <- Data %>% select(-sex,-sport)



png("CVA_bi_plot.png", width = 10, height = 5, units = "in", res = 300)

fit <- manova(as.matrix(Data[, 2:12]) ~ sport, data = Data)
cva_out <- candisc(fit)

# scores and loadings
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


# Axis limitS
xlim <- c(-10,4)
ylim <- range(scores$Can2) * 1.1

par(mar = c(4,4,2,2))
#Plot ---
plot(scores$Can1, scores$Can3,
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
              levels = 0.68,    # 1 SD
              add = TRUE,
              col = cols[s],    
              plot.points = FALSE,
              lwd = 2)
  
}

# Draw loadings (arrows)
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











#new atheletes
new <- data.frame(
  rcc    = c(4.35, 4.53),
  wcc    = c(7.8, 5),
  hc     = c(41.4, 40.7),
  hg     = c(14.1, 14),
  ferr   = c(30, 41),
  bmi    = c(22.03, 17.79),
  ssf    = c(117.8, 56.8),
  pcBfat = c(23.3, 12.55),
  lbm    = c(48.32, 38.3),
  ht     = c(169.1, 156.9),
  wt     = c(63, 44.8)
)

# Match column order to your CV coefficients
coef_mat <- cva_out$coeffs.raw[, 1:2]  
new_centred <- scale(new, center = colMeans(Data[, rownames(coef_mat)]), scale = FALSE)
new_mat <- as.matrix(new_centred[, rownames(coef_mat)])

#CV scores for the two new athletes
new_scores <- new_mat %*% as.matrix(coef_mat)

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


png("CVA_new_aths_plot.png", width = 10, height = 5, units = "in", res = 300)

par(mar = c(4,4,2,2))
#Plot ---
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
              levels = 0.68,    # 1 SD-style ellipse
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

points(new_scores[,1], new_scores[,2],
       pch = 8, cex = 2, lwd = 2, col = c("hotpink","darkgreen"))
text(new_scores[,1], new_scores[,2],
     labels = c("NewAthlete_1", "New2"), pos = 3)

dev.off()