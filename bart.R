
options(mc.cores = 8)
library(BART3)
library(pROC)

y.train <- read.csv('MCW_train_data_label.csv')
y.test <- read.csv('MCW_test_data_label.csv')
N <- nrow(y.train)
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
##     post[[i]] <- mc.gbart(x.train, y.train[[i]], x.test, nskip = 1000,
##                           type = 'pbart', sparse = TRUE, seed = i)
## }
## saveRDS(post, 'RDS/bart.rds')

post=readRDS('RDS/bart.rds')

## check <- list()
## valid <- list()
## for(i in 1:4) {
##     valid[[i]] <- predict(post[[i]], x.valid)
##     check[[i]] <- roc(response = y.valid[[i]], 
##                   predictor = valid[[i]]$prob.test.mean)
## }
## saveRDS(check, 'RDS/valid-bart.rds')

check <- readRDS('RDS/valid-bart.rds')

pred <- list()
AUC <- matrix(nrow = 200, ncol = 4)
for(j in 1:200) {
    bs <- readRDS(paste0('breast/', j, '.rds'))
    for(i in 1:4) {
        pred[[i]] <- predict(post[[i]], data.frame(bs$x.valid))
        AUC[j,i] <- mean(outer(pred[[i]]$prob.test.mean[bs$y.valid[[i]] == 0],
                                   pred[[i]]$prob.test.mean[bs$y.valid[[i]] == 1],
                                   '<'))
    }
}
saveRDS(AUC, 'RDS/breast-bart.rds')

names(y.test)
rbind(round(apply(AUC, 2, quantile, probs = 0.025), digits = 3),
      round(c(check[[1]]$auc[1], check[[2]]$auc[1], check[[3]]$auc[1], check[[4]]$auc[1]), digits = 3),
      round(apply(AUC, 2, quantile, probs = 0.975), digits = 3))

##detach("package:BART3", unload=TRUE)


auc <- 0
for(i in 1:4) {
    auc[i] <- round(mean(outer(post[[i]]$prob.test.mean[y.test[[i]] == 0],
                     post[[i]]$prob.test.mean[y.test[[i]] == 1],
                     '<')), digits = 3)
}
auc

saveRDS(auc, 'RDS/auc-bart.rds')

pred <- list()
AUC <- matrix(nrow = 200, ncol = 4)
for(j in 1:200) {
    bs <- readRDS(paste0('bs/', j, '.rds'))
    for(i in 1:4) {
        pred[[i]] <- predict(post[[i]], data.frame(bs$x.test))
        AUC[j,i] <- mean(outer(pred[[i]]$prob.test.mean[bs$y.test[[i]] == 0],
                                   pred[[i]]$prob.test.mean[bs$y.test[[i]] == 1],
                                   '<'))
    }
}

names(y.test)
rbind(round(apply(AUC, 2, quantile, probs = 0.025), digits = 3),
      auc,
      round(apply(AUC, 2, quantile, probs = 0.975), digits = 3))

saveRDS(AUC, 'RDS/AUC-bart.rds')
##detach("package:BART3", unload=TRUE)
