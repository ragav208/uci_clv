library(dplyr)
library(keras)
library(ggplot2)
#Read the data 
ucl_data = read.csv("~/Downloads/Online_Retail.csv",stringsAsFactors = F)

#Fix the date 
ucl_data = ucl_data %>% mutate(InvoiceDate = as.Date(as.POSIXct(InvoiceDate,format = "%m/%d/%Y %H:%M")))

#Filter out Qty < 0 
neg = ucl_data %>% filter(Quantity<0)
ucl_data = ucl_data %>% filter(Quantity>=0) %>% filter(!is.na(CustomerID))


#Get Recency, First purchase and last purchase made by customers
max_InvoiceDate = max(ucl_data$InvoiceDate)
threshold_date = unique(ucl_data$InvoiceDate)[round(length(unique(ucl_data$InvoiceDate)) * 0.5)]


cal = ucl_data %>%  filter(InvoiceDate<=threshold_date) %>% group_by(CustomerID) %>% 
  mutate(Recency = as.numeric(threshold_date-max(InvoiceDate)),First_purchase = as.numeric(threshold_date - min(InvoiceDate)) , 
         Last_purchase =as.numeric(threshold_date - max(InvoiceDate)),            
         time_between = as.numeric(round(mean(diff(InvoiceDate)))), first_last = abs(Last_purchase-First_purchase),
        Value = Quantity*UnitPrice, Frequency = n_distinct(InvoiceNo),
        repeat_customers = ifelse(n_distinct(InvoiceNo)>1,n_distinct(InvoiceNo),0))  %>% ungroup()%>%
        group_by(CustomerID,InvoiceNo) %>% mutate(mean_val = mean(Value)) %>% ungroup()%>%arrange(InvoiceDate)

cal = cal %>% mutate(time_between = ifelse(is.na(time_between),0,time_between))

unique(cal$Frequency)
unique(cal$repeat_customers)

unique(cal$Recency)

check = cal %>% filter(is.na(time_between))



library(keras)
train_data = cal %>% select(Quantity,CustomerID,Country,Recency,First_purchase,Last_purchase,time_between,Frequency,first_last,repeat_customers) 
str(train_data)

library(caret)
dmy = dummyVars("~Country",data = train_data)
trsf <- data.frame(predict(dmy, newdata = train_data))

train_data2 = cbind(train_data%>% select(-Country),trsf) 


mean <- apply(train_data2, 2, mean)
std <- apply(train_data2, 2, sd)
train_data2 <- scale(train_data2, center = mean, scale = std)
# test_data <- scale(test_data, center = mean, scale = std)

# train_data2 = as.matrix(train_data2)
target = cal$Value



# k <- 3
# indices <- sample(1:nrow(train_data2))
# folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
# num_epochs <- 50 # 100  
# all_scores <- c()


L1_REGULARIZATION = 0.0216647
L2_REGULARIZATION = 0.0673949
DROPOUT = 0.899732


model_cam = keras_model_sequential()
input <-  layer_input(shape = ncol(train_data2))
preds <-  input %>%
  layer_dense(units = 64,activation = 'relu')%>% #,kernel_regularizer = regularizer_l1_l2(L1_REGULARIZATION,L2_REGULARIZATION)) %>%
#  layer_dropout(0.1) %>%
  layer_dense(units = 64,activation = 'relu')%>% #,kernel_regularizer = regularizer_l1_l2(L1_REGULARIZATION,L2_REGULARIZATION)) %>%
  layer_dense(units = 32,activation = 'relu')%>% #,kernel_regularizer = regularizer_l1_l2(L1_REGULARIZATION,L2_REGULARIZATION)) %>%
  #layer_dropout(0.899) %>%
  layer_dense(units = 16,activation = 'relu') %>%
  #layer_dropout(0.1) %>%
  
  layer_dense(units=1,activation = 'relu')


model_cam = keras_model(input,preds) %>% compile(
  loss = 'mse',
  optimizer = optimizer_adam(lr =  0.05,decay = 0.7),
  #lr =  0.00135,
  metrics = list('mae')
)


early_stopping <- callback_early_stopping(patience = 10)
#   epochs = 30, 

set.seed(1246)
# hist = model_cam %>% fit(
#   x = train_data2, 
#   y = target, 
#   batch_size = 100,
# #  validation_data = list(x_test, x_test), 
#   callbacks = list(checkpoint, early_stopping)
# )

checkpoint <- callback_model_checkpoint(
  filepath = "model.hdf5",
  save_best_only = TRUE,
  period = 1,
  verbose = 1
)

history <- model_cam %>% fit(
  train_data2,
  target,
  epochs = 20,
  validation_split = 0.2,
  batch_size =  5,
  verbose = 1,
    
  callbacks = list(checkpoint,early_stopping)
#  callbacks = list(print_dot_callback)
)


# save_model_hdf5()
# save(model_cam,file="model.RDA")
# mod2=load("model.hdf5")
# loss <- evaluate(model_cam, x = x_test, y = x_test)
# loss

pred_train <- data.frame(predict(model_cam, train_data2))
mse_train <- sum((target-pred_train)^2)
#save(mse_train,file="mse_train.RData")


training_data = cbind(train_data,pred_train,target)
training_data$InvoiceDate = as.Date(cal$InvoiceDate)
str(training_data)

ggplot(data = training_data,aes(x=InvoiceDate,y=target))+geom_line()+geom_line(aes(y=pred_train$predict.model_cam..train_data2.),color="red")+ylim(0,50)


library(dplyr)
holdout = ucl_data %>%  filter(InvoiceDate>threshold_date) %>% group_by(CustomerID) %>% 
  mutate(Recency = as.numeric(max_InvoiceDate-max(InvoiceDate)),First_purchase = as.numeric(max_InvoiceDate - min(InvoiceDate)) , 
         Last_purchase =as.numeric(max_InvoiceDate - max(InvoiceDate)),            
         time_between = as.numeric(round(mean(diff(InvoiceDate)))), first_last = abs(Last_purchase-First_purchase),
         Value = Quantity*UnitPrice, Frequency = n_distinct(InvoiceNo),
         repeat_customers = ifelse(n_distinct(InvoiceNo)>1,n_distinct(InvoiceNo),0))  %>% ungroup()%>%
  group_by(CustomerID,InvoiceNo) %>% mutate(mean_val = mean(Value)) %>% ungroup()%>%arrange(InvoiceDate)

holdout = holdout %>% mutate(time_between = ifelse(is.na(time_between),0,time_between))


library(keras)
test_data = holdout %>% select(Quantity,CustomerID,Country,Recency,First_purchase,Last_purchase,time_between,Frequency,first_last,repeat_customers) 
str(test_data)

library(caret)
# dmy = dummyVars("~Country",data = train_data)
test_country <- data.frame(predict(dmy, newdata = test_data))
Missing <- setdiff(colnames(trsf), colnames(test_country))
test_country = data.frame(test_country)
test_country[Missing] <- 0
test_country= test_country[ , order(names(test_country))]
test_country$CountryRSA=NULL


test_data2 = cbind(test_data%>% select(-Country),test_country) 
nrow(test_data2)

 # mean <- apply(test_data2, 2, mean)
 # std <- apply(test_data2, 2, sd)
test_data2 <- scale(test_data2, center = mean, scale = std)
test_data2[is.na(test_data2)] <- 0 
# test_data <- scale(test_data, center = mean, scale = std)

pred_test <- data.frame(predict(model_cam, test_data2))
mse_test <- sum((holdout$Value-pred_test)^2)
#save(mse_train,file="mse_train.RData")


test_data = cbind(holdout,pred_test)

ggplot(data = training_data,aes(x=InvoiceDate,y=target))+geom_line()+geom_line(aes(y=pred_train$predict.model_cam..train_data2.),color="red")+ylim(0,50)

ggplot(data = test_data,aes(x=InvoiceDate,y=Value,group=(InvoiceDate)))+geom_line()+geom_line(aes(y=pred_test$predict.model_cam..test_data2.),color="red")+ylim(0,20)+geom_smooth()

plot_data = test_data %>% group_by(InvoiceDate) %>% mutate(actual_sale = mean(Value),pred_sale = ifelse((10+predict.model_cam..test_data2.)>60,quantile((10+predict.model_cam..test_data2.),p=0.80),(10+predict.model_cam..test_data2.)))
write.csv(plot_data,"test_results.csv")
ggplot(data = plot_data,aes(x=InvoiceDate,y=actual_sale))+geom_line()+geom_line(aes(y=pred_sale),color="red")+ylim(0,100)

# build_model <- function() {
#   
#   model <- keras_model_sequential() %>%
#     layer_dense(units = 64, activation = "relu",
#                 input_shape = dim(train_data2)[2]) %>%
#     layer_dense(units = 64, activation = "relu") %>%
#     layer_dense(units = 1)
#   
#   model %>% compile(
#     loss = "mse",
#     optimizer = optimizer_rmsprop(),
#     metrics = list("mean_absolute_error")
#   )
#   
#   model
# }
# 
# model <- build_model()
# model %>% summary()
# 
# print_dot_callback <- callback_lambda(
#   on_epoch_end = function(epoch, logs) {
#     if (epoch %% 80 == 0) cat("\n")
#     cat(".")
#   }
# )    
# 
# epochs <- 50
# 
# # Fit the model and store training stats
# history <- model %>% fit(
#   train_data2,
#   target,
#   epochs = epochs,
#   validation_split = 0.2,
#   verbose = 0,
#   callbacks = list(print_dot_callback)
# )
# 
# library(ggplot2)
# 
# plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
#   coord_cartesian(ylim = c(0, 5))
# 
# 




