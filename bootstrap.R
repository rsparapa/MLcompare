
options(mc.cores = 8)

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
M <- nrow(x.test)

for(j in 1:200) {
    set.seed(-j)
    i <- sample.int(M, M, TRUE)
    bs <- list(y.test = y.test[i, ], x.test = x.test[i, ])
    saveRDS(bs, paste0('bs/', j, '.rds'))
}
