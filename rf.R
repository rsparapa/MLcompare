
options(mc.cores = 8)
library(randomForestSRC)
library(pROC)
y.train <- read.csv('MCW_train_data_label.csv')
y.test <- read.csv('MCW_test_data_label.csv')
N <- nrow(y.train)
## check <- read.csv('MCW_train_data.csv')
##x.test <- read.csv('MCW_test_data.csv')
x.train <- read.csv('all_data_3835_1.csv')
x.train$race <- as.factor(x.train$race)
x.train$train_index <- NULL 
x.test <- x.train[-(1:N), ]
x.train <- x.train[(1:N), ]
y.valid <- read.csv('valid_label_329.csv')
x.valid <- read.csv('valid_data_329.csv')
x.valid$race <- as.factor(x.valid$race)

## post <- list()
## for(i in 1:4) {
##     df <- data.frame(y = y.train[[i]])
##     df <- cbind(df, x.train)
##     post[[i]] <- rfsrc(y~., data = df)
## }
## saveRDS(post, 'RDS/rf.rds')

post=readRDS('RDS/rf.rds')

## check <- list()
## valid <- list()
## for(i in 1:4) {
##     valid[[i]] <- predict(post[[i]], x.valid)
##     check[[i]] <- roc(response = y.valid[[i]], 
##                   predictor = valid[[i]]$predicted)
## }
## saveRDS(check, 'RDS/valid-rf.rds')

check <- readRDS('RDS/valid-rf.rds')

## pred <- list()
## AUC <- matrix(nrow = 200, ncol = 4)
## for(j in 1:200) {
##     bs <- readRDS(paste0('breast/', j, '.rds'))
##     for(i in 1:4) {
##         pred[[i]] <- predict(post[[i]], data.frame(bs$x.valid))
##         AUC[j,i] <- mean(outer(pred[[i]]$predicted[bs$y.valid[[i]] == 0],
##                                    pred[[i]]$predicted[bs$y.valid[[i]] == 1],
##                                    '<'))
##     }
## }
## saveRDS(AUC, 'RDS/breast-rf.rds')
AUC <- readRDS('RDS/breast-rf.rds')

names(y.test)
rbind(round(apply(AUC, 2, quantile, probs = 0.025), digits = 3),
      round(c(check[[1]]$auc[1], check[[2]]$auc[1], check[[3]]$auc[1], check[[4]]$auc[1]), digits = 3),
      round(apply(AUC, 2, quantile, probs = 0.975), digits = 3))

names(y.test)
##[1] "CVD"  "HF"   "AFib" "CAD" 
pred <- list()
auc <- 0
for(i in 1:4) {
    df <- data.frame(y = y.test[[i]])
    df <- cbind(df, x.test)
    pred[[i]] <- predict(post[[i]], df)
    auc[i] <- round(mean(outer(pred[[i]]$predicted[y.test[[i]] == 0],
                     pred[[i]]$predicted[y.test[[i]] == 1],
                     '<')), digits = 3)
}

saveRDS(auc, 'RDS/auc-rf.rds')
## [1] 0.8001063
## [1] 0.8481542
## [1] 0.7588486
## [1] 0.7784509

library(pROC)
check <- list()
for(i in 1:4) check[[i]] <- roc(response = y.test[[i]], 
                  predictor = pred[[i]]$predicted)
saveRDS(check, 'RDS/roc-rf.rds')

AUC <- matrix(nrow = 200, ncol = 4)
for(j in 1:200) {
    bs <- readRDS(paste0('bs/', j, '.rds'))
    for(i in 1:4) {
        df <- data.frame(y = bs$y.test[[i]])
        df <- cbind(df, bs$x.test)
        pred[[i]] <- predict(post[[i]], df)
        AUC[j,i] <- mean(outer(pred[[i]]$predicted[bs$y.test[[i]] == 0],
                                   pred[[i]]$predicted[bs$y.test[[i]] == 1],
                                   '<'))
    }
}

names(y.test)
rbind(round(apply(AUC, 2, quantile, probs = 0.025), digits = 3),
      auc,
      round(apply(AUC, 2, quantile, probs = 0.975), digits = 3))


saveRDS(AUC, 'RDS/AUC-rf.rds')
detach("package:randomForestSRC", unload=TRUE)

