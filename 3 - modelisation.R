library(randomForest)
library(rpart)
library(rpart.plot)
library(xts)
library(magrittr)

base = read.table("../data/base.csv",sep=",",dec='.',header = T)
base = xts(base[,-1], as.Date(base[,1]) )
base = base["2000/"] %>% na.omit()


dim_out = 0.3
# Methode 1 : selection aléatoire des individus
ind_split = sample(c(TRUE, FALSE), nrow(base), replace=TRUE, prob=c(1-dim_out,dim_out))
base_in  = base[ind_split,]
base_out = base[!ind_split,]

sum(base_in$target)/nrow(base_in)
sum(base_out$target)/nrow(base_out)

# Methode 2 : selection fin de periode
ind_split = round( nrow(base) * (1 - dim_out),0 )
base_in  = base[1:ind_split,]
base_out = base[(ind_split+1):nrow(base),]

sum(base_in$target)/nrow(base_in)
sum(base_out$target)/nrow(base_out)

# --- Recursive Partitioning sur la selection de variables ---
form_rp = as.formula("as.factor(target)~.")
fit.rp  = rpart(form_rp, minsplit=round(0.01*nrow(base_in)),data=base_in, method="class")
rpart.plot(fit.rp)

y_pred_in  = predict(fit.rp,newdata = base_in,type="class")
y_pred_out = predict(fit.rp,newdata = base_out,type="class")

perf_custom <- function(y_obs,y_pred,printcm = F) {  
  # confusion matrix
  cm = table(y_obs,y_pred)
  
  # Ajout d'une ligne ou d'une colone si x ou y ne contient qu'une seule classe (que des 0 ou que des 1)
  if(ncol(cm)==1) {
    if(colnames(cm)[1]=="0") cm = cbind(cm,"1"=0)
    if(colnames(cm)[1]=="1") cm = cbind("0"=0,cm)
  }
  if(nrow(cm)==1) {
    if(rownames(cm)[1]=="0") cm = rbind(cm,"1"=0)
    if(rownames(cm)[1]=="1") cm = rbind("0"=0,cm)
  }
  
  if( printcm) print(cm)
  
  TP  = cm[2,2]
  TN  = cm[1,1]
  FP  = cm[1,2]
  FN  = cm[2,1]

  # indices standards
  prv   = (TP+FN)/(TP+TN+FP+FN) # Prévalence
  acc   = (TP+TN)/(TP+TN+FP+FN) # Accuracy
  se    = TP/(TP+FN)            # Sensibilité
  sp    = TN/(TN+FP)            # Spécificité
  ppv   = TP/(TP+FP)            # PPV
  npv   = TN/(TN+FN)            # NPV

  xx = c(accuracy=acc,se=se,sp=sp,ppv=ppv,npv=npv,prv=prv)
  xx
}

perf_custom(base_in$target,y_pred_in,T)
perf_custom(base_out$target,y_pred_out,T)


# --- Random Forest ---
formula_rf = as.formula("as.factor(target)~.")
fit.rf  = randomForest(formula_rf,ntree=400,nodesize=round(0.01*nrow(base_in)),mtry=floor(sqrt(ncol(base_in))), data = base_in, importance=T)
varimp  = varImpPlot(fit.rf,main="importance")
varimp  = varimp[order(varimp[,2], decreasing=T),2]

y_pred_in  = predict(fit.rf,newdata = base_in,type="prob")
y_pred_out = predict(fit.rf,newdata = base_out,type="prob")

thres = 0.2
perf_custom(base_in$target,( y_pred_in[,2] >= thres) * 1,T)
perf_custom(base_out$target,( y_pred_out[,2] >= thres) * 1,T)

table_cm = NULL
for(thres in seq(0,1,0.1)){
  xx = perf_custom(base_out$target,( y_pred_out[,2] >= thres) * 1,F)
  table_cm = rbind(table_cm,xx)
}
table_cm = as.data.frame(table_cm)
rownames(table_cm) =seq(0,1,0.1)
table_cm$opt = table_cm$se + table_cm$sp


plot(1-table_cm$sp, table_cm$se, type = 'l', col = "red")
abline(0,1)


##############################################################################
###                     smote
##############################################################################
library(performanceEstimation)
library(DMwR)

base_in_smote = as.data.frame(base_in)
base_in_smote$target = factor(base_in_smote$target)

base_in_smote <- smote(target ~ ., base_in_smote, perc.over = 1, perc.under = 3)
table(base_in$target)
table(base_in_smote$target)


fit.rf  = randomForest(target ~ .,ntree=400,nodesize=round(0.01*nrow(base_in_smote)),mtry=floor(sqrt(ncol(base_in_smote))), data = base_in_smote, importance=T)

y_pred_out = predict(fit.rf,newdata = base_out,type="prob")
table_cm = NULL
for(thres in seq(0,1,0.1)){
  xx = perf_custom(base_out$target,( y_pred_out[,2] >= thres) * 1,F)
  table_cm = rbind(table_cm,xx)
}
table_cm = as.data.frame(table_cm)

plot(1-table_cm$sp, table_cm$se, type = 'l', col = "red")
abline(0,1)





################################################################
###                           LSTM
################################################################

library(keras)
library(tensorflow)

train = (base$sp500 - mean(base$sp500))/sd(base$sp500) 
train = as.matrix(train)

nb_lag     = 12
prediction = 12

x_train <- t(sapply(
  1:(length(train) - nb_lag - prediction + 1),
  function(x) train[x:(x + nb_lag - 1), 1]
))

# now we transform it into 3D form
x_train <- array(
  data = as.numeric(unlist(x_train)),
  dim = c(
    nrow(x_train),
    nb_lag,
    1
  )
)

y_train <- t(sapply(
  (1 + nb_lag):(length(train) - prediction + 1),
  function(x) train[x:(x + prediction - 1)]
))

y_train <- array(
  data = as.numeric(unlist(y_train)),
  dim = c(
    nrow(y_train),
    prediction,
    1
  )
)

lstm_model <- keras_model_sequential()
lstm_model %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, 12, 1), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_lstm(units = 50,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'accuracy')

summary(lstm_model)


lstm_model %>% fit(
  x = x_train,
  y = y_train,
  batch_size = 1,
  epochs = 20,
  verbose = 0,
  shuffle = FALSE
)

lstm_forecast <- lstm_model %>%
  predict(x_train, batch_size = 1) %>%
  .[, , 1]

lstm_forecast <- lstm_forecast * sd(base$sp500) + mean(base$sp500)


x_test <- base$sp500[(nrow(train) - prediction + 1):nrow(train)]
x_test <- (x_test - mean(base$sp500)) / sd(base$sp500)

# this time our array just has one sample, as we intend to perform one 12-months prediction
x_pred <- array(
  data = x_test,
  dim = c(
    1,
    nb_lag,
    1
  )
)

lstm_forecast <- lstm_model %>%
  predict(x_pred, batch_size = 1) %>%
  .[, , 1]

# we need to rescale the data to restore the original values
lstm_forecast <- lstm_forecast * sd(base$sp500) + mean(base$sp500)
