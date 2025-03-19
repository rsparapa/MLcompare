
options(mc.cores = 8)
library(catboost)
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
##     dftrain <- catboost.load_pool(data.matrix(x.train), 
##                                 label = y.train[[i]])
##     post[[i]] <- catboost.train(dftrain)
## }
## saveRDS(post, 'RDS/catboost.rds')

post=readRDS('RDS/catboost.rds')

## check <- list()
## valid <- list()
## for(i in 1:4) {
##     dfvalid <- catboost.load_pool(data.matrix(x.valid), 
##                                 label = y.valid[[i]])
##     valid[[i]] <- catboost.predict(post[[i]], dfvalid)
##     check[[i]] <- roc(response = y.valid[[i]], 
##                   predictor = valid[[i]])
## }
## saveRDS(check, 'RDS/valid-catboost.rds')

check <- readRDS('RDS/valid-catboost.rds')

pred <- list()
AUC <- matrix(nrow = 200, ncol = 4)
for(j in 1:200) {
    bs <- readRDS(paste0('breast/', j, '.rds'))
    for(i in 1:4) {
    dfvalid <- catboost.load_pool(data.matrix(x.valid), 
                                label = y.valid[[i]])
        pred[[i]] <- catboost.predict(post[[i]], dfvalid)
        AUC[j,i] <- mean(outer(pred[[i]][bs$y.valid[[i]] == 0],
                                   pred[[i]][bs$y.valid[[i]] == 1],
                                   '<'))
    }
}
saveRDS(AUC, 'RDS/breast-catboost.rds')

names(y.test)
rbind(round(apply(AUC, 2, quantile, probs = 0.025), digits = 3),
      round(c(check[[1]]$auc[1], check[[2]]$auc[1], check[[3]]$auc[1], check[[4]]$auc[1]), digits = 3),
      round(apply(AUC, 2, quantile, probs = 0.975), digits = 3))

pred <- list()
auc <- 0
for(i in 1:4) {
    dftest <- catboost.load_pool(data.matrix(x.test), 
                                label = y.test[[i]])
    pred[[i]] <- catboost.predict(post[[i]], dftest) 
    auc[i] <- round(mean(outer(pred[[i]][y.test[[i]] == 0],
                     pred[[i]][y.test[[i]] == 1],
                     '<')), digits = 3)
}
##[1] "CVD"  "HF"   "AFib" "CAD" 
print(auc)
saveRDS(auc, 'RDS/auc-catboost.rds')

library(pROC)
check <- list()
for(i in 1:4) check[[i]] <- roc(response = y.test[[i]], 
                  predictor = pred[[i]])
saveRDS(check, 'RDS/roc-catboost.rds')


AUC <- matrix(nrow = 200, ncol = 4)
for(j in 1:200) {
    bs <- readRDS(paste0('bs/', j, '.rds'))
    for(i in 1:4) {
    dftest <- catboost.load_pool(data.matrix(bs$x.test), 
                                label = y.test[[i]])
    pred[[i]] <- catboost.predict(post[[i]], dftest) 
        AUC[j,i] <- mean(outer(pred[[i]][bs$y.test[[i]] == 0],
                                   pred[[i]][bs$y.test[[i]] == 1],
                                   '<'))
    }
}
names(y.test)
rbind(round(apply(AUC, 2, quantile, probs = 0.025), digits = 3),
      auc,
      round(apply(AUC, 2, quantile, probs = 0.975), digits = 3))

saveRDS(AUC, 'RDS/AUC-catboost.rds')
detach("package:catboost", unload=TRUE)
