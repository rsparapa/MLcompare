
library(pROC)

type <- c('logit', 'cart', 'rf', 'bart', 'xgboost', 'lightgbm', 'catboost')
type. <- c('LR', 'CART', 'RF', 'BART', 'XGBoost', 'LightGBM', 'CatBoost')

ROC <- list()
for(k in 1:7) {
    ROC[[k]] <- readRDS(paste0('RDS/valid-', type[k], '.rds'))
}

CVD <- matrix(nrow = 7, ncol = 7)
HF  <- matrix(nrow = 7, ncol = 7)
Afib <- matrix(nrow = 7, ncol = 7)
CAD <- matrix(nrow = 7, ncol = 7)

for(k in 1:4) {
    for(i in 1:7) {
        for(j in 1:7) 
        if(i != j){
            C = roc.test(roc1 = ROC[[i]][[k]], roc2 = ROC[[j]][[k]])$p.value 
            if(k == 1) CVD[i, j] = C
            if(k == 2) HF[i, j] = C
            if(k == 3) Afib[i, j] = C
            if(k == 4) CAD[i, j] = C
        }
    }
}

print('CVD')
dimnames(CVD) <- list(type., type.)
print(substr(format(CVD, digits = 4, scientific = FALSE), 0, 6))
print('HF')
dimnames(HF) <- list(type., type.)
print(substr(format(HF, digits = 4, scientific = FALSE), 0, 6))
print('Afib')
dimnames(Afib) <- list(type., type.)
print(substr(format(Afib, digits = 4, scientific = FALSE), 0, 6))
print('CAD')
dimnames(CAD) <- list(type., type.)
print(substr(format(CAD, digits = 4, scientific = FALSE), 0, 6))
