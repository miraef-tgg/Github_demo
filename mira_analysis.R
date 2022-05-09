#packages and data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,RColorBrewer, randomForest,data.table,class,install = TRUE, visreg, update = getOption("pac_update"), character.only = FALSE)

current_path <-getActiveDocumentContext()$path
dat_orig<-read.csv(paste0(dirname(current_path), "/TGG_Data_Takehome.csv"))
dat<-dat_orig

# how to push users to subscription?
# trips < 2 minutes; single use = more profit
# trips > 2 minutes; want as subscription member.


#quick look
str(dat)
head(dat)
length(unique(dat$trip_id))
(unique(dat$variable))

#put in wide format
wide<-as.data.frame(pivot_wider(dat, id_cols="trip_id", names_from = "variable"))

#reformat time vars
wide$trip_length_min<-lubridate::as_datetime(wide$end_date)-lubridate::as_datetime(wide$start_date)
wide$doy_start<-lubridate::yday(substr(wide$start_date,1,10))
wide$doy_end<-lubridate::yday(substr(wide$end_date,1,10))

#quick look at trip lengths and customers vs subscribers
par(mfrow=c(1,1))
hist(as.numeric(subset(wide, wide$subscriber_type=="Subscriber")$trip_length_min), xlim=c(0,100), 
     breaks=500, prob=T, ylim=c(0,.1), col=rgb(0,0,1,.3),
     main="Customers and Subscribers:",xlab="trip time (minutes)", ylab = "density")
lines(density(as.numeric(subset(wide, wide$subscriber_type=="Subscriber")$trip_length_min), adjust=6),col="blue", lwd=2)        
hist(as.numeric(subset(wide, wide$subscriber_type=="Customer")$trip_length_min), xlim=c(0,100), 
     breaks=500, prob=T, ylim=c(0,.1), col=rgb(1,0,0,.3), add=T)
lines(density(as.numeric(subset(wide, wide$subscriber_type=="Customer")$trip_length_min), adjust=4),col="red", lwd=2)        
legend("topright", legend=c("Subscribers", "Customers"), col = c("blue", "red"), lty=1, lwd=2)

sum(wide$trip_length_min>2)/nrow(wide)*100 #short rides
sum(wide$trip_length_min>2&wide$subscriber_type=="Customer")/nrow(wide)*100 #short, customer rides



#who are customers vs subscribers?
cust_sub_summary<- wide %>%
  dplyr::group_by(subscriber_type) %>%
  dplyr::mutate(avg_trip_length=mean(trip_length_min),
                sd_trip_length=sd(trip_length_min),
                perc_male=sum(member_gender=="Male")/sum(member_gender!="a")*100,
                perc_female=sum(member_gender=="Female")/sum(member_gender!="a")*100,
                perc_other_unknown=sum(member_gender=="" | member_gender =="Other")/sum(member_gender!="a")*100,
                mean_birth_year=mean(as.numeric(member_birth_year), na.rm=T),
                mean_doy=mean(doy_start),
                mean_doy_end=mean(doy_end))



keep<-c("subscriber_type", "avg_trip_length","sd_trip_length", "perc_male" ,"perc_female","perc_other_unknown","avg_trip_length", "mean_birth_year","mean_doy", "mean_doy_end")
(look<-(subset(cust_sub_summary, duplicated(cust_sub_summary$subscriber_type)==F)[,keep]))

bar<-transpose(look); bar$var<-keep
par(mfrow=c(1,1))
barplot(height = c(look$perc_male, look$perc_female, look$perc_other_unknown), 
        col=rep(c(rgb(0,0,1,.5), rgb(1,0,0,.5)),3),
        ylim=c(0,100), main = "Customers and Subscribers",
        names.arg=c("%male",NA, "% female",NA, "% other/unknown",NA))
legend("topright", legend=c("Subscribers", "Customers"), col = c("blue", "red"), pch=15)


#customers are more female, take longer trips, slightly older, and seem to ride later in the year
table(wide$member_gender)

#check number of categories in categorical predictor data
for ( c in 1:ncol(wide)) (print(paste0(names(wide)[c], " : ", length(as.vector(unique(wide[,c]))))))

#reformatting, selecting predictor vars, and splitting to train/test to run some models 

cols<-c("start_station_name","end_station_name","subscriber_type","zip_code", 
        "member_gender", "bike_share_for_all_trip", "trip_length_min", "member_birth_year", "doy_start")

mod_dat<-as.data.frame(subset(wide,select=cols ))
mod_dat<-mod_dat[ !is.na(mod_dat$subscriber_type),]

for (c in 1:ncol(mod_dat)) (mod_dat[,c]<-factor(mod_dat[,c]))
str(mod_dat)
mod_dat$trip_length_min<-as.numeric(mod_dat$trip_length_min)
mod_dat$member_birth_year<-as.numeric(mod_dat$member_birth_year)
mod_dat$doy_start<-as.numeric(mod_dat$doy_start)
mod_dat$y<-ifelse(mod_dat$subscriber_type=="Subscriber", 1, 0)

train_rows<-sample(1:nrow(mod_dat), .9*nrow(mod_dat))

##-----------  glm -------------------
glm1<-glm(y~member_gender+ trip_length_min +member_birth_year +doy_start, data=mod_dat[train_rows,], family="binomial")
summary(glm1)

#useful helper fnc
plt.mod <- function(model, ...){
  old.par <- par(mfrow=c(2,2))
  plot(model, ...)
  par(old.par)
}

summary(glm1)$null.deviance
summary(glm1)$deviance

#check--doesn't look great
plt.mod(glm1)

#testing data
predictions<-mod_dat[-train_rows,]
predictions$predictions_glm <-  predict.glm(glm1, newdata=mod_dat[-train_rows,])
predictions$binary_pred <- ifelse(predictions$predictions_glm >.5,1,0)
cor(predictions$binary_pred, predictions$y)

par(mfrow=c(1,3))
plot(y=jitter(predictions$binary_pred),x= predictions$trip_length_min, col=factor(predictions$member_gender),
     pch=19, main="predictions", xlab="trip length", ylab="predictions (1=Subscriber)")
plot(y=jitter(predictions$y),x= predictions$trip_length_min, col=factor(predictions$member_gender), 
     pch=19, main="real data", xlab="trip length", ylab="real data (1=Subscriber)")
plot(y=jitter(predictions$y),x= jitter(predictions$binary_pred),col=factor(predictions$member_gender), 
     pch=19, main="How did my model do?", xlab="predictions", ylab="actual data")

#results: mostly right, more often wrongly predicts customer when they're a subscriber
table(predictions$y==predictions$binary_pred & predictions$binary_pred==1)[2]/nrow(predictions)*100
table(predictions$y==predictions$binary_pred & predictions$binary_pred==0)[2]/nrow(predictions)*100
table(predictions$y!=predictions$binary_pred & predictions$binary_pred==0)[2]/nrow(predictions)*100
table(predictions$y!=predictions$binary_pred & predictions$binary_pred==1)[2]/nrow(predictions)*100


##-----------  Random Forest -------------------

covariates<-c("member_gender","bike_share_for_all_trip", "trip_length_min",
"member_birth_year","doy_start")

y<-mod_dat[train_rows,]$y
preds<-subset(mod_dat, select=covariates)[train_rows,]
ytest<-mod_dat[-(train_rows),]$y
predstest<-subset(mod_dat, select=c(covariates,"y"))[-(train_rows),]

rf<-randomForest(x=preds,y=y, importance=T)
(importance(rf))

#testing data
predictions_rf<-mod_dat[-train_rows,]
predictions_rf$predictions_rf <-  predict(rf, newdata=mod_dat[-train_rows,])
predictions_rf$binary_pred <- ifelse(predictions_rf$predictions_rf >.5,1,0)

cor(predictions_rf$binary_pred, predictions_rf$y)

par(mfrow=c(1,3))
plot(y=jitter(predictions_rf$binary_pred),x= predictions_rf$trip_length_min, col=factor(predictions_rf$member_gender),
     pch=19, main="predictions_rf", xlab="trip length", ylab="predictions_rf (1=Subscriber)")
plot(y=jitter(predictions_rf$y),x= predictions_rf$trip_length_min, col=factor(predictions_rf$member_gender), 
     pch=19, main="real data", xlab="trip length", ylab="real data (1=Subscriber)")
plot(y=jitter(predictions_rf$y),x= jitter(predictions_rf$binary_pred),col=factor(predictions_rf$member_gender), 
     pch=19, main="How'd my model do?", xlab="predictions_rf", ylab="actual data")

#results: mostly right, also  often wrongly predicts customer when they're a subscriber
table(predictions_rf$y==predictions_rf$binary_pred & predictions_rf$binary_pred==1)[2]/nrow(predictions_rf)*100
table(predictions_rf$y==predictions_rf$binary_pred & predictions_rf$binary_pred==0)[2]/nrow(predictions_rf)*100
table(predictions_rf$y!=predictions_rf$binary_pred & predictions_rf$binary_pred==0)[2]/nrow(predictions_rf)*100
table(predictions_rf$y!=predictions_rf$binary_pred & predictions_rf$binary_pred==1)[2]/nrow(predictions_rf)*100


#summary thoughts:
# randomForest performs a little better (at least in this random draw of train/test data)
# both models indicate that trip length and gender are indeed important parts of predicting who is a subscriber vs customer
# all covariates I looked at are statistically significant (unsurprising given amount of data); these two have clearest implications


