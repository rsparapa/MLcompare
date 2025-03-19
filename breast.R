
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
y.valid <- read.csv('valid_label_329.csv')
x.valid <- read.csv('valid_data_329.csv')
x.valid$race <- as.factor(x.valid$race)
M <- nrow(x.valid)

for(j in 1:200) {
    set.seed(-j)
    i <- sample.int(M, M, TRUE)
    bs <- list(y.valid = y.valid[i, ], x.valid = x.valid[i, ])
    saveRDS(bs, paste0('breast/', j, '.rds'))
}
