library(RMySQL)

print("#########################MySQL DATA#######################")
con <- dbConnect(MySQL(),user='root',password='root',host='52.207.0.161',dbname='anomaly_db')

rs = dbSendQuery(con, "select * from Prostate_Data")
prostate_data = fetch(rs, n=-1)


print("###################CSV Data###################################")
pros_csv<-read.csv("E:\\Confidential\\Case Studies\\Predictive Analytics\\POC on Anomaly Detection\\Anomaly Detection using unstructured\\prostate_new.csv",header = T)
print("########################Text Data#################################")
pros_txt<-read.table("E:\\Confidential\\Case Studies\\Predictive Analytics\\POC on Anomaly Detection\\Anomaly Detection using unstructured\\prostate.txt",header = T)
print("##################Convert IDHexa to decimal in text data before merging####################### ")
pros_txt$ID<-strtoi(pros_txt$ID,base=16L)
pros_txt$AGE<-strtoi(pros_txt$AGE,base=16L)
pros_txt$RACE<-strtoi(pros_txt$RACE,base=16L)
prostate<-merge(prostate_data,pros_csv,by="ID",all = TRUE)
prostate_full<-merge(prostate,pros_txt,by="ID",all = T)
prostate_full<-prostate_full[c('ID','CAPSULE','AGE','RACE','DPROS','DCAPS','PSA','VOL','GLEASON')]