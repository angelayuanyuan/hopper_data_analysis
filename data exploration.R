library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(purrr)
library(ggplot2)
library(h2o)
library(whereport)
library(lime)
library(gridExtra)
library(ggcorrplot)
library(factoextra)
library(corrplot)


# read data
watches_raw <- fread("../watches table.csv")
head(watches_raw)

# missing data
sort((colSums(is.na(watches_raw))/nrow(watches_raw))*100,decreasing = TRUE)


# distinct values
var_value <- data.frame(apply(watches_raw,2, n_distinct),row.names = colnames(watches_raw))
colnames(var_value) <- "count"

# save Rdata
saveRDS(watches_raw,"watches.rds")


# data type conversion
watches <- readRDS("watches.rds")
summary(watches)

## factors
cols <- c("weekend","filter_no_lcc","filter_non_stop","filter_short_layover")
setDT(watches)[, (cols):= lapply(.SD, factor), .SDcols=cols]

watches <- watches %>% mutate_if(is.character,funs(factor(.)))
watches$status_latest <- factor(watches$status_latest,levels = c("expired","inactive","active","booked","shopped"))
watches$filter_name <- factor(watches$filter_name,levels = c("And(ShortLayover,NoLCC)","NoLCC","And(NonStop,NoLCC)","ShortLayover","NonStop","NoFilter"))

# Date& time
watches$departure_date <- mdy(watches$departure_date)
watches$return_date <- mdy(watches$return_date)

watches$first_search_dt <- mdy_hm(watches$first_search_dt)
watches$watch_added_dt <- mdy_hm(watches$watch_added_dt)
watches$latest_status_change_dt <- mdy_hm(watches$latest_status_change_dt)

watches$first_buy_dt <- mdy_hm(watches$first_buy_dt)
watches$last_notif_dt <- mdy_hm(watches$last_notif_dt)


# create new variables
## departure& return
watches$departure_month <- months(watches$departure_date)
watches$departure_month <- factor(watches$departure_month, levels = c("January", "February", "March","April","May","June","July","August","September","October","November","December"))
watches$return_month <- months(watches$return_date)
watches$return_month <- factor(watches$return_month, levels = c("January", "February", "March","April","May","June","July","August","September","October","November","December"))

watches$departure_weekday <- weekdays(watches$departure_date)
watches$departure_weekday <- factor(watches$departure_weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
watches$return_weekday <- weekdays(watches$return_date)
watches$return_weekday <- factor(watches$return_weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

watches$weekend <- as.factor(ifelse(watches$departure_weekday=="Sunday"|watches$departure_weekday=="Saturday",1,0))


watches$first_search_weekday <- factor(watches$first_search_weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# origin & destinations
watches <- watches %>%
  separate(origin, c("airport_og", "og"), "/") # specify certain airport or not

watches <- watches %>%
  separate(destination, c("airport_des", "des"), "/")

watches$og <- as.factor(watches$og)
watches$des <- as.factor(watches$des)
watches$airport_og <- as.factor(watches$airport_og)
watches$airport_des <- as.factor(watches$airport_des)

## first search
watches$first_search_month <- as.factor(as.character(months(watches$first_search_dt)))
watches$first_search_weekday <- as.factor(as.character(weekdays(watches$first_search_dt)))

watches$first_search_dt <- as.POSIXct(watches$first_search_dt)

## time of first search
hour_of_search_weekend <- watches%>%
  filter(weekend==1)%>%
  select(first_search_dt)
  
hour_of_search_weekend$hour_of_search <- as.numeric(format(ymd_hms(hour_of_search_weekend$first_search_dt),"%H"))

hour_of_search_workday <- watches%>%
  filter(weekend==0)%>%
  select(first_search_dt)

hour_of_search_workday$hour_of_search <- as.numeric(format(ymd_hms(hour_of_search_workday$first_search_dt),"%H"))

hour_of_search_workday$business <- hour_of_search_workday$hour_of_search %in% seq(9, 16)


grid.arrange(
ggplot(hour_of_search_weekend,aes(x = hour_of_search)) + stat_bin(breaks = (0:24-1), colour = "white",fill="pink") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Searching by Time of day (Weekend)") +
  scale_x_continuous(breaks = 0:23, labels = c(1:23,0)),


ggplot(hour_of_search_workday,aes(x = hour_of_search,fill=business)) + stat_bin(breaks = (0:24-1),colour = "white") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Searching by Time of day (Weekday)") +
  scale_x_continuous(breaks = 0:23, labels = c(1:23,0))+
  theme(legend.position="none"),
ncol=2)


# search times
grid.arrange(
ggplot(watches)+
  geom_bar(aes(first_search_weekday,fill=first_search_weekday)),
ggplot(watches)+
  geom_bar(aes(first_search_weekday,fill=watch),position = "dodge"),
ncol=2)
  

## till first buy
watches$first_buy_days <- round((difftime(watches$first_buy_dt, watches$first_search_dt, units = "days")))
watches$first_buy_days <- as.numeric(watches$first_buy_days)

# watch the flight or not
watches$watch <- as.factor(ifelse(is.na(watches$watch_advance),0,1))

# notification
watches$notification <- as.factor(ifelse(is.na(watches$total_notifs),0,1))

saveRDS(watches,"watches_add.rds")

watches <- readRDS("watches_add.rds")

###############################################
############## Data Visualization #############
###############################################
# origin & destination specification
grid.arrange(
ggplot(watches)+
  geom_bar(aes(airport_og,fill=airport_og))+
  ggtitle("Origin Specification")+
  scale_y_continuous(labels = scales::comma),  # more likely to specify the airport 
ggplot(watches)+
  geom_bar(aes(airport_des,fill=airport_des))+
  ggtitle("Destination Specification")+
  scale_y_continuous(labels = scales::comma),
ncol=2)

# filters?
ggplot(watches)+
  geom_bar(aes(filter_name,fill=filter_name)) # majority with no filter, not business?


# status?
ggplot(watches)+
  geom_bar(aes(status_latest,fill=status_latest))+
  scale_y_continuous(labels = scales::comma)# most shopped


# trip type?
ggplot(watches)+
  geom_bar(aes(trip_type,fill=trip_type))+
  scale_y_continuous(labels = scales::comma)# mostly round trip

# How early do people start searching for flights?
ggplot(watches)+
  geom_density(aes(first_advance,fill=trip_type),alpha=0.7)
 

median(watches$first_advance) # 65 days

one_way <- watches%>%filter(trip_type=="one_way")%>%
         select(first_advance)
median(one_way$first_advance)  # 51 days


round_trip <- watches%>%filter(trip_type=="round_trip")%>%
  select(first_advance)
median(round_trip$first_advance)  # 68 days



# weekend?
grid.arrange(
ggplot(watches)+
  geom_bar(aes(weekend,fill=trip_type),position = "dodge")+
  scale_y_continuous(labels = scales::comma),# mostly weekday

ggplot(watches)+
  geom_bar(aes(departure_weekday,fill=trip_type),position = "dodge"),

ggplot(watches[!is.na(watches$return_weekday),])+
  geom_bar(aes(return_weekday,fill=return_weekday)),


ggplot(watches[watches$trip_type=="round_trip"&watches$stay<=50,])+
  geom_bar(aes(stay,fill="#FA6866"))+
  geom_vline(xintercept = 5,linetype=2)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position="none"),
ncol=2)


# travel dates
ggplot(watches)+
  geom_bar(aes(departure_month,fill=departure_month)) # spring break? (might need user demo), biased

ggplot(watches)+
  geom_bar(aes(departure_weekday,fill=trip_type),position = "dodge")

ggplot(watches[!is.na(watches$return_month),])+
  geom_bar(aes(return_month,fill=return_month))

ggplot(watches[!is.na(watches$return_weekday),])+
  geom_bar(aes(return_weekday,fill=return_weekday))


# number of stays for round trip
ggplot(watches[watches$trip_type=="round_trip",])+
  geom_bar(aes(stay))
summary(watches$stay)

ggplot(watches[watches$trip_type=="round_trip"&watches$stay<=50,])+
  geom_bar(aes(stay,fill="#FA6866"))+
  geom_vline(xintercept = 5,linetype=2)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position="none")


# returned users (by user id)
user_freq <- watches%>%
  group_by(user_id)%>%
  summarise(n=n_distinct(trip_id))

ggplot(user_freq)+
  geom_bar(aes(n))

quantile(user_freq$n,0.99)
median(user_freq$n)

ggplot(user_freq[user_freq$n<=30,])+
  geom_bar(aes(n))




## returned user example
user_231 <- watches%>%
  filter(user_id=="bace1d2481c4596372f1f987bd86734b4a1a27203a80fd577e0e43a224becaf2")

user_231_uniq <- user_231%>%
  group_by(trip_id)%>%
  summarise(n=n())     # times of search

ggplot(user_231)+
  geom_bar(aes(status_latest))

ggplot(user_231)+
  geom_histogram(aes(first_advance))

# status and first advance
grid.arrange(
ggplot(watches)+
  geom_density(aes(first_advance,color=status_latest)),

ggplot(watches)+
  geom_boxplot(aes(status_latest,first_advance,fill=status_latest)),  #"last minute searches tend to expire"

ncol=2)

# watch and first advance
ggplot(watches)+
  geom_boxplot(aes(watch,first_advance,fill=watch))

# watch and status
ggplot(watches)+
  geom_bar(aes(status_latest,fill=watch)) 

# first_rec and advance
ggplot(watches)+
  geom_boxplot(aes(first_rec,first_advance,fill=first_rec))  # "": notification turned off?

# notification
ggplot(watches)+
  geom_bar(aes(notification,fill=notification))
sum(as.numeric(as.character(watches$notification)))/nrow(watches) # 6% of users turned off notification(?)

# status and notification
ggplot(watches)+
  geom_bar(aes(status_latest,fill=notification),position = "dodge")

ggplot(watches)+
  geom_col(aes(status_latest,total_notifs, fill=status_latest))

ggplot(watches)+
  geom_col(aes(status_latest,total_buy_notifs, fill=status_latest))

# first advance and travel dates (doesn't mean anything since samples are biased)
ggplot(watches)+
  geom_boxplot(aes(departure_month,first_advance,fill=departure_month)) 
ggplot(watches)+
  geom_boxplot(aes(departure_weekday,first_advance,fill=departure_weekday)) 

ggplot(watches[!is.na(watches$return_month),])+
  geom_boxplot(aes(return_month,first_advance,fill=return_month))


# watching option
ggplot(watches)+
  geom_bar(aes(watch,fill=notification))


# first rec and status
ggplot(watches)+
  geom_bar(aes(status_latest,fill=first_rec),position = "dodge")

# first rec and first advance
ggplot(watches)+
  geom_density(aes(first_advance,color=first_rec))

# forecast price and status
ggplot(watches[watches$forecast_first_good_price<2500,])+
  geom_density(aes(forecast_first_good_price,color=watch))

ggplot(watches[watches$forecast_last_good_price<2500,])+
  geom_density(aes(forecast_last_good_price,color=watch))


ggplot(watches[watches$forecast_min_target_price<2500,])+
  geom_density(aes(forecast_min_target_price,color=watch))

# first rec and watch
ggplot(watches[watches$first_rec=="buy"|watches$first_rec=="wait",])+
  geom_bar(aes(first_rec,fill=watch),position = "dodge")+
  scale_y_continuous(labels = scales::comma)



###############################################
#################### Models ###################
###############################################

h2o.init(
  nthreads=-1,          
  max_mem_size = "12G")

h2o.removeAll() 

# random forest  (outcome:status)

rf.dta <- watches[,-c(1,2,8,9,10,15,16,17,18,19,27,28,30,35,36,41,43,45,47,48,50,51)] # delete factor(watch) due to linear seperation

col.type <- sapply(rf.dta,class)

## split
set.seed(2018)
tr <- sample(nrow(watches),700000)
rf.dta_tr <- rf.dta[tr,]
rf.dta_te <-rf.dta[-tr,]

rf.dta_tr <- as.h2o(rf.dta_tr,col.types=col.types)
rf.dta_te <- as.h2o(rf.dta_te,col.types=col.types)


rf1 <- h2o.randomForest(         
  training_frame = rf.dta_tr, 
  validation_frame = rf.dta_te,
  y=10,                          
  ntrees = 200,                  
  stopping_rounds = 2,           
  seed = 2018)                

summary(rf1) 
h2o.varimp_plot(rf1, num_of_features = 10)  
rf1@model$validation_metrics

explainer_rf1 <- lime(as.data.frame(rf.dta_tr[,-10]), rf1, n_bins = 5)

explanation_rf1 <- explain(
  as.data.frame(rf.dta_te[33,-10]), 
  explainer    = explainer_rf1, 
  n_labels     = 5,
  n_features   = 5)



plot_features(explanation_rf1)


explanation_rf1_shopped <- explain(
  as.data.frame(rf.dta_te[26,-10]), 
  explainer    = explainer_rf1, 
  labels = "shopped",
  n_features   = 5)

plot_features(explanation_rf1_shopped)

explanation_rf1_active <- explain(
  as.data.frame(rf.dta_te[246,-10]), 
  explainer    = explainer_rf1, 
  labels = "active",
  n_features   = 5)

plot_features(explanation_rf1_active)

explanation_rf1_expired <- explain(
  as.data.frame(rf.dta_te[258,-10]), 
  explainer    = explainer_rf1, 
  labels = "expired",
  n_features   = 5)

plot_features(explanation_rf1_expired)


explanation_rf1_inactive <- explain(
  as.data.frame(rf.dta_te[74,-10]), 
  explainer    = explainer_rf1, 
  labels = "inactive",
  n_features   = 5)

plot_features(explanation_rf1_inactive)




# a step back

rf.dta_watch <- watches%>%
  filter(status_latest!="shopped")


rf.dta_watch <- watches[,-c(1,2,8,9,10,15,16,17,18,19,27,28,30,35,36,41,43,45,47,48,50)]



col.type1 <- sapply(rf.dta_watch, class)

rf.dta_watch_tr <- rf.dta_watch[tr,]
rf.dta_watch_te <- rf.dta_watch[-tr,]

rf.dta_watch_tr <- as.h2o(rf.dta_watch_tr,col.types=col.type1)
rf.dta_watch_te <- as.h2o(rf.dta_watch_te,col.types=col.type1)


rf2 <- h2o.randomForest(         
  training_frame = rf.dta_watch_tr, 
  validation_frame = rf.dta_watch_te,
  y=10,                          
  ntrees = 200,                  
  stopping_rounds = 2,           
  seed = 2018)  


summary(rf2) 
h2o.varimp_plot(rf2, num_of_features = 10)  
rf2@model$validation_metrics



explainer_rf2 <- lime(as.data.frame(rf.dta_watch_tr[,-10]), rf2, n_bins = 5)

explanation_rf2 <- explain(
  as.data.frame(rf.dta_watch_te[c(1:3),-10]), 
  explainer    = explainer_rf2, 
  n_labels     = 4,
  n_features   = 5)


plot_features(explanation_rf2, ncol = 2)



explanation_rf2_active <- explain(
  as.data.frame(rf.dta_watch_te[6,-10]), 
  explainer    = explainer_rf2, 
  labels = "active",
  n_features   = 5)

plot_features(explanation_rf2_active)

explanation_rf2_expired <- explain(
  as.data.frame(rf.dta_watch_te[808,-10]), 
  explainer    = explainer_rf2, 
  labels = "expired",
  n_features   = 5)     # 787

plot_features(explanation_rf2_expired)


explanation_rf2_inactive <- explain(
  as.data.frame(rf.dta_watch_te[2229,-10]), 
  explainer    = explainer_rf2, 
  labels = "inactive",
  n_features   = 5)

plot_features(explanation_rf2_inactive)


# watch or not?
watch.ex <- watches[,c(3,4,5,6,7,11,12,13,14,23,24,29,31,32,37,38,39,40,42,46,49,51)]
col.type2 <- sapply(watch.ex,class)

w.dta_tr <- watch.ex[tr,]
w.dta_te <- watch.ex[-tr,]

w.dta_tr <- as.h2o(w.dta_tr,col.types=col.type2)
w.dta_te <- as.h2o(w.dta_te,col.types=col.type2)


rf3 <- h2o.randomForest(         
  training_frame = w.dta_tr, 
  validation_frame = w.dta_te,
  y=22,                          
  ntrees = 200,                  
  stopping_rounds = 2,           
  seed = 2018)                

summary(rf3) 
h2o.varimp_plot(rf3, num_of_features = 10)  
rf3@model$validation_metrics


explainer_rf3 <- lime(as.data.frame(w.dta_tr[,-22]), rf3, n_bins = 5)

explanation_rf3 <- explain(
  as.data.frame(w.dta_te[97,-22]), 
  explainer    = explainer_rf3, 
  n_labels     = 2,
  n_features   = 5)    

plot_features(explanation_rf3)

# correlation 
corr <- round(cor(watches[,c(24,29,31,32,37,38,39,40,42)], use = "complete.obs"), 2)
ggcorrplot(corr)


# pca
pc.dta <- watches[,c(24,29,31,32,37,38,39,40,42)]

tr.pca <- prcomp(na.omit(pc.dta),
                 center = TRUE,
                 scale. = TRUE)


eig.val <- get_eigenvalue(tr.pca)
fviz_eig(tr.pca, addlabels = TRUE, ylim = c(0, 65))

var <- get_pca_var(tr.pca)
fviz_pca_var(tr.pca, alpha.var="contrib")

corrplot(var$contrib, is.corr=FALSE)  

fviz_contrib(tr.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(tr.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(tr.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(tr.pca, choice = "var", axes = 4, top = 10)

# shop or not
sh.dta <- rf.dta
sh.dta$shop <- as.factor(ifelse(sh.dta$status_latest=="shopped",1,0))
sh.dta <- sh.dta[,-c(10,11,12)]

sh.dta_tr <- sh.dta[tr,]
sh.dta_te <- sh.dta[-tr,]

col.type3 <- sapply(sh.dta, class)


sh.dta_tr <- as.h2o(sh.dta_tr,col.types=col.type3)
sh.dta_te <- as.h2o(sh.dta_te,col.types=col.type3)


rf4 <- h2o.randomForest(         
  training_frame = sh.dta_tr, 
  validation_frame = sh.dta_te,
  y=28,                          
  ntrees = 200,                  
  stopping_rounds = 2,           
  seed = 2018)                

summary(rf4) 
h2o.varimp_plot(rf4, num_of_features = 10)  
rf4@model$validation_metrics

explainer_rf4 <- lime(as.data.frame(sh.dta_tr[,-28]), rf4, n_bins = 5)
explanation_rf4 <- explain(
  as.data.frame(sh.dta_te[186,-28]), 
  explainer    = explainer_rf4, 
  n_labels     = 2,
  n_features   = 5)  

plot_features(explanation_rf4)


ggplot(sh.dta[sh.dta$notification==1,])+
  geom_bar(aes(shop,fill=shop))+
  scale_y_continuous(labels = scales::comma)

ggplot(sh.dta[sh.dta$notification==1&sh.dta$last_total<2000,])+
  geom_density(aes(lowest_total,fill=shop),alpha=0.7)
