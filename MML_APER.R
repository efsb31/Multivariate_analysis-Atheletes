library(MASS)   
library(dplyr)
library(candisc)
library(car)

Data <- read.csv("C:\\Users\\erinb\\OneDrive\\uni\\Y3_uni\\MA3MML\\MML1\\ST3MMLsport.csv")



set.seed(123)  

# Prepare data
Data$sport <- factor(Data$sport)

predictors <- c("rcc","wcc","hc","hg","ferr","bmi",
                "ssf","pcBfat","lbm","ht","wt")

#70/30 train–test split ----
n <- nrow(Data)
train_idx <- sample(1:n, size = floor(0.7 * n)) 

train_dat <- Data[train_idx, c("sport", predictors)]
test_dat  <- Data[-train_idx, c("sport", predictors)]

#Fit LDA model on training data 
lda_fit <- lda(sport ~ ., data = train_dat)

# Predict sport for the test data
lda_pred <- predict(lda_fit, newdata = test_dat)

# Confusion matrix AND APER
conf_mat <- table(
  Actual = test_dat$sport,
  Pred   = lda_pred$class
)

conf_mat

# Apparent error rate on the test set
APER <- mean(lda_pred$class != test_dat$sport)
APER