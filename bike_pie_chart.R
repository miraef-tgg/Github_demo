#packages 
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,RColorBrewer, randomForest,data.table,class,install = TRUE, visreg, update = getOption("pac_update"), character.only = FALSE)


#data
current_path <-getActiveDocumentContext()$path
dat_orig<-read.csv(paste0(dirname(current_path), "/TGG_Data_Takehome.csv"))
dat<-dat_orig[duplicated(paste0(dat_orig$trip_id , dat_orig$variable))==FALSE,]

#put in wide format
wide<-as.data.frame(pivot_wider(dat, names_from = "variable"))

#save new formatted data
write.csv(wide,paste0(dirname(current_path), "/bike_data_wide.csv"))

#make a pie chart!

#this pie chart includes 'other category'

slices <- c(sum(wide$member_gender=="Male"), sum(wide$member_gender=="Female"), sum (wide$member_gender != "Female" & wide$member_gender != "Male"  & wide$member_gender != "" ) )
labels <- c("male", "female", "other")
pie(slices, labels = labels, main="Bike riders", col= c("darkblue", "lightblue", "darkorange"))

#
