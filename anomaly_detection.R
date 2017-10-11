
# install.packages("h2o")
# install.packages('randomForest')
library(randomForest)
# install.packages('pROC')
library(pROC)
library(h2o)
library(RMySQL)

# This is the original sequence. Uncomment this if the newt dataset is not working
#prosPath = system.file("extdata", "prostate.csv", package = "h2o")
#prostate_df <- read.csv(prosPath)
print("************************************This is my comment******************************************")
print("************************************Importing prostate data part from MySQL DB******************************************")
print("First few rows of data from the Database")
con <- dbConnect(MySQL(),user='root',password='root',host='52.207.0.161',dbname='anomaly_db')
rs = dbSendQuery(con, "select * from Prostate_Data")
prostate_data_db = fetch(rs, n=-1)
head(prostate_data_db)

print("*************************************Handling prostate data part in Binary format from CSV file**********************************")
print("First few rows of binary data from CSV file")
pros_csv<-read.csv("prostate_binary.csv",header = T)
head(pros_csv)
print("*********************Converting the prostate data part having binary columns to decimal columns**********************")
print("First few rows of data after conversion")
pros_csv$ID<-strtoi(pros_csv$ID,base=2L)
pros_csv$CAPSULE<-strtoi(pros_csv$CAPSULE,base=2L)
pros_csv$DPROS<-strtoi(pros_csv$DPROS,base=2L)
pros_csv$DCAPS<-strtoi(pros_csv$DCAPS,base=2L)
head(pros_csv)
print("**********************Handling another prostate data part in Hexadecimal format from TXT file***********************")
print("First few rows of hexadeximal data from TXT file")
pros_txt<-read.table("prostate_hex.txt",header = T)
head(pros_txt)
print("*********************Converting the prostate data part having hexadecimal columns to decimal columns**********************")
print("First few rows of data after conversion")
pros_txt$ID<-strtoi(pros_txt$ID,base=16L)
pros_txt$AGE<-strtoi(pros_txt$AGE,base=16L)
pros_txt$RACE<-strtoi(pros_txt$RACE,base=16L)
head(pros_txt)
prostate<-merge(prostate_data_db,pros_csv,by="ID",all = TRUE)
prostate_df<-merge(prostate,pros_txt,by="ID",all = T)
prostate_df<-prostate_df[c('ID','CAPSULE','AGE','RACE','DPROS','DCAPS','PSA','VOL','GLEASON')]

print("**************************************First Few Rows of Prostate Cancer Data***********************************************")
head(prostate_df)

prostate_df <- prostate_df[,-1]
set.seed(1234)
random_splits <- runif(nrow(prostate_df))
print("****************************The original dataset is split into Training set and Testing/Validating set*********************************")
train_df <- prostate_df[random_splits < .5,]
print("******************************************Rows and Columns in the training dataset***************************************************")
dim(train_df)
print("*******************************************Rows and Columns in the testing dataset***************************************************")
validate_df <- prostate_df[random_splits >=.5,]
dim(validate_df)

# Get benchmark score

outcome_name <- 'CAPSULE'
feature_names <- setdiff(names(prostate_df), outcome_name)
set.seed(1234)
print("**************************Random Forest Prediction Model is built with Capsule column as the output/predictor************************")
rf_model <- randomForest(x=train_df[,feature_names],
                         y=as.factor(train_df[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)

validate_predictions <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")

auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions[,2])
print("****************************Receiver Operating Curve(ROC) is plotted with the model for checking Model Fit**************************")
plot(auc_rf, print.thres = "best", main=paste('Random Forest AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')
print(paste0("******************************Area Under Curve is found to be :",round(auc_rf$auc[[1]],3),"********************************"))
# build autoencoder model
print("*******************Since AUC is not close to 1, H2O Deep Learning with Autoencoder is used to detect Anomalies**********************")
localH2O = h2o.init()
prostate.hex<-as.h2o(train_df, destination_frame="train.hex")
prostate.dl = h2o.deeplearning(x = feature_names, training_frame = prostate.hex,
                               autoencoder = TRUE,
                               reproducible = T,
                               seed = 1234,
                               hidden = c(6,5,6), epochs = 50)

# interesting per feature error scores
# prostate.anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=TRUE)
# head(prostate.anon)

prostate.anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=FALSE)
print("**************************************First few rows of Error values from H2O Deep Learning ******************************************")
head(prostate.anon)
err <- as.data.frame(prostate.anon)

print("**************************************Plot of Pattern and Anomaly(Deviation) is built from H2O******************************************")
plot(sort(err$Reconstruction.MSE))
print("***********************************From the plot, we have deviation significant from Error value = 0.1**********************************")
print("*************With the H2O output, the original dataset is split into two sets - Below 0.1 error and Above/Equal to 0.1 error************")
# use the easy portion and model with random forest using same settings
print("***************************With the dataset having Below 0.1 error, Random Forest Prediction Model is built****************************")
train_df_auto <- train_df[err$Reconstruction.MSE < 0.1,]

set.seed(1234)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)

validate_predictions_known <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")

auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_known[,2])
print(paste0("******************ROC curve is plotted based on new predictions and AUC is found to be ",round(auc_rf$auc[[1]],3),"**********************"))
plot(auc_rf, print.thres = "best", main=paste('AUC curve after applying H2O:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# use the hard portion and model with random forest using same settings
print("******************With the remaining dataset having Above/Equal to 0.1 error, Random Forest Prediction Model is built******************")
train_df_auto <- train_df[err$Reconstruction.MSE >= 0.1,]

set.seed(1234)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)

validate_predictions_unknown <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_unknown[,2])
print(paste0("******************ROC curve is plotted based on new predictions and AUC is found to be ",round(auc_rf$auc[[1]],3),"**********************"))
plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# bag both results set and measure final AUC score
print("*****************************These two separate prediction values are now combined for Boosting****************************************")
valid_all <- (validate_predictions_known[,2] + validate_predictions_unknown[,2]) / 2
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=valid_all)
print(paste0("******************ROC curve is plotted using predictions from Boosting and AUC is found to be ",round(auc_rf$auc[[1]],3),"**********************"))
plot(auc_rf, print.thres = "best", main=paste('AUC after Bagging:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')
