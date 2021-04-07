library(tidyr)
library(tidyverse)
library(MASS)
library(reshape2)
library(ggplot2)
library(data.table)
library(zoo)
data = read.csv("/Users/addytan/Desktop/IUB-19SP/EDA/hour.csv", header = TRUE)
data = as.data.table(data)
#data = data[yr==1,]

cb_palette <- c("#0072B2", "#D55E00","#F0E442","#009E73","#999999", "#E69F00", "#56B4E9", "#0072B2", "#D55E00", "#CC79A7")
## format data
data[,season:= factor(season, labels = c('Winter','Spring','Summer','Fall'))]
data$weekday <- factor(data$weekday, levels=c(1,2,3,4,5,6,7), labels=c('Mon','Tue','Wed','Thur','Fri','Sat',"Sun"), ordered=TRUE)
data$weekday[is.na(data$weekday)]="Sun"
data[,workingday:= factor(workingday, labels = c('Non Workday','Workday'))]
data[,date := as.Date(dteday)]
data[,year:= factor(yr,labels=c(2011,2012))]
data[,weather:= factor(weathersit, labels = c('Clear or partly Cloudy',
                                              'Light Snow or Rain',
                                              'Misty and Cloudy','Heavly Snow or Rain'))]
data[,temp2 := temp*47-8]
data[,b_temp2 := atemp*66-16]
data[,humidity := hum*100]
data[,windspeed := windspeed*67]
data[,temp_diff := temp2-b_temp2]
df$weathersit<-factor(df$weather,levels = c("normal","foggy","light_prec","heavy_prec"),labels = c('Clear or partly Cloudy',
                                                                    'Light Snow or Rain',
                                                                    'Misty and Cloudy','Heavly Snow or Rain'),ordered = TRUE)
data$holiday <- factor(data$holiday, levels=c(0,1), labels=c('Not Holiday',"Holiday"), ordered=TRUE)
## definition of Dew Point
data$humidity[(data$humidity==0.00)]=0.01
N<-(log(data$humidity/100,base = exp(1))+((17.27*data$temp2)/(237.3+data$temp2)))/17.27
data$dp<-237.3*N/(1-N)
data$Dew <- cut(data$dp, c(-80,0,10,15, 19, 22, 26), 
                labels=c("Extreme Dry","Dry", "Comfortable","Alright", "Uncomfortable", "Extreme Unconfortable"), include.lowest=T)

s<-data%>% group_by(year,Seasons,hr,Dew,holiday,weather,humidity,mnth,workingday,weekday) %>% summarise(Casual=sum(casual),Registered=sum(registered))


data_long <- gather(s, condition, measurement, Casual:Registered, factor_key=TRUE)
data_long

## definition of season

yq <- as.yearqtr(as.yearmon(data$dteday, "%Y-%m-%d") + 1/12)
data$Seasons <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))

data$tempdiff_factor <- cut(data$temp_diff, breaks = c(-13, -5, 0, 5, 10, 15,20,25,35))
df_temp <- data %>% group_by(tempdiff_factor, temp2) %>% summarise(avg_cnt = (sum(cnt)/n()))
ggplot(data=df_temp, aes(x=temp2, y=avg_cnt, color=tempdiff_factor)) + 
  geom_line() + 
  scale_color_manual(values = cb_palette)
 
ggplot(data, aes(temp2))+
  geom_histogram(position="dodge")+scale_color_manual(values = cb_palette)


ggplot(tempdata, aes(x=temp2, y=n,color=year))+
  geom_line(size=1.5)

ggplot(btempdata, aes(x=n, y=temp_diff,color=season))+
  facet_wrap(~season)+
  geom_line(size=1.5)

ggplot(data, aes(x=temp2, y=cnt,color=hr))+
  geom_jitter(alpha=0.3)+
  geom_point(size=1.5)+
  facet_wrap(~year)+
labs(title = 'bicyle rental count per hour',
     x='hour',y='bicyle rental count')


ggplot(data, aes(weather,fill=year))+
  geom_histogram(stat="count",position="dodge")

ggplot(subset(data_long,year==2012),aes(x=hr,y=measurement,fill=condition))+
  geom_col()+
  facet_wrap(~holiday)+
  scale_fill_manual(values = cb_palette,name="User Type")+
  theme_bw()+
  labs(title = 'Bike Usage Count in ',
       subtitle="Holiday and Non Holiday",
       x='Hour',y='Count')

#####datadfajh####
s<-df%>% group_by(yr,Seasons,hr,holiday,weather,mnth,workingday,weekday) %>% summarise(Casual=mean(casual),Registered=mean(registered))
data_long <- gather(s, condition, measurement, Casual:Registered, factor_key=TRUE)

data_plot<-data_long%>%group_by(hr,workingday,condition,yr)%>%summarise(avg_cnt = (mean(measurement)))

ggplot(subset(data_plot,yr="2012"),aes(x=hr,y=avg_cnt,fill=condition))+
  geom_col()+
  facet_wrap(~workingday)+
  scale_fill_manual(values = cb_palette,name="User Type")+
  theme_bw()+
  guides(colour = guide_legend(nrow=1,byrow=TRUE)) + 
  theme(legend.position="bottom",legend.text = element_text(size='10'))+
  labs(title = 'Bike Usage Average Count',
       subtitle="Working and Non Working Days",
       x='Hour',y='Count')

ggplot(data_long,aes(x=season,y=measurement,fill=condition))+geom_col()

data_plot<-data_long%>%group_by(Seasons,weekday,condition,year)%>%summarise(avg_cnt = (sum(measurement)/n()))

ggplot(subset(data_plot,year==2012),aes(x=weekday,y=avg_cnt,fill=condition))+
  geom_col()+
  coord_flip() +
  facet_wrap(~Seasons)+scale_fill_manual(values = cb_palette,name="User Type")+
  theme_bw()+
  labs(title = 'Casual and Registered User Count in Different Season',
                  x='Days of Week',y='Count')


ggplot(data_long,aes(x=mnth,y=measurement,fill=condition))+
  geom_col()+
  facet_wrap(~year)+
  scale_fill_manual(values = cb_palette)+
  theme_bw()
data_plot<-data_long%>%group_by(hr,weather,workingday,condition,year)%>%summarise(avg_cnt = (sum(measurement)/n()))

ggplot(subset(data_plot,year==2012),aes(x=hr,y=avg_cnt,fill=condition))+
  geom_col()+
  facet_wrap(~workingday+weather)+
  scale_fill_manual(values = cb_palette,name="User Type")+
  theme_bw()+
  labs(title = 'User Count in Workday and Non Workday',
       x='Hour',y='Count')

install.packages("ggmosaic")
library(ggmosaic)
cb_palette=c("#56B4E9","#999999",'#E69F00','#009E73')
ggplot(data) + geom_mosaic(aes(product(weather,Seasons), weight = cnt, fill = weather)) + 
  xlab("Seasons") +
  ylab("Proportion of bikes in each season") + 
  scale_fill_manual(values =cb_palette)+theme_bw()+
  ggtitle('Conditional distribution of bike usage or \njoint relative frequency of bike usage \nin each Season/Weather combination')


data$dteday<-NULL
data$weathersit<-NULL

ggplot(subset(data_long,year==2012),aes(x=Seasons,fill=weather))+
  geom_bar()+
  scale_fill_manual(values = cb_palette,name="Weather Condition")+
  theme_bw()+
  labs(title = 'Type of Weather Coundition in Each Season',
       x='Season',y='Weather Count')


data_plot<-data_long%>%group_by(hr,weather,workingday,condition,year)%>%summarise(avg_cnt = (sum(measurement)/n()))
ggplot(subset(data_plot,weather!='Heavly Snow or Rain'& weather!='Misty and Cloudy'& year==2012),aes(x=hr,y=avg_cnt,fill=condition))+
  geom_col()+
  facet_wrap(~weather+workingday)+
  scale_fill_manual(values = cb_palette,name="User Type")+
  theme_bw()+
  labs(title = 'Bike Usage Count in Workday and Non Workday',
       subtitle="By Different Weather Conditions",
       x='Hour',y='Count')



ggplot(subset(data_long,weather=='Misty and Cloudy'& year==2012),aes(x=hr,y=measurement,fill=condition))+
  geom_col()+
  facet_wrap(~weather+workingday)+
  scale_fill_manual(values = cb_palette,name="User Type")+
  theme_bw()+
  scale_y_continuous(limits=c(0, 10000))+
  labs(title = 'Bike Usage Count in Workday and Non Workday By Weather Conditions',
       x='Hour',y='Count')



ggplot(data, aes(x=temp2, y=cnt,color=hr))+
  geom_jitter(alpha=0.3)+
  geom_smooth(size=1.5)+
  facet_wrap(~year)
  labs(title = 'bicyle rental count per hour',
       x='Temp',y='bicyle rental count')
  
  

  
ggplot(data, aes(x=temp2, y=cnt,color=hr))+
    geom_jitter(alpha=0.3)+
    geom_smooth(size=1.5)+
    facet_wrap(~year)
  labs(title = 'bicyle rental count per hour',
       x='hour',y='bicyle rental count')
  
####humidity#######
  
ggplot(subset(data_long, year==2012 ),aes(x=hr,y=measurement,fill=condition))+
   geom_col()+
   facet_wrap(~Dew)+
   scale_fill_manual(values = cb_palette,name="User Type")+
   theme_bw()+
   labs(title = 'Bike Usage by Different Humidity Conditions', x='Hours',y='Count')
  
count.loess = loess(cnt ~ temp2 * hr , data = data,degree = 1)
count.grid = expand.grid(temp2 = c(-5, 0, 10,20,30), hr = c(0,5,10,15,18,23))
count.predict = predict(count.loess, newdata = count.grid)  

ggplot(data.frame(count.grid, fit = as.vector(count.predict)), aes(x = temp2, y = fit,color = factor(hr))) + 
  geom_line()+scale_color_manual(values = cb_palette)

summary(data$humidity)
plot(density(data$humidity))
countwind.loess = loess(cnt ~ temp2 + windspeed, data = data,degree = 1)
countwind.grid = expand.grid(temp2 = c(-5, 0, 10,20,30), windspeed = c(0,10,20,30,40,50))
countwind.predict = predict(countwind.loess, newdata = countwind.grid)  

ggplot(data.frame(countwind.grid, fit = as.vector(countwind.predict)), 
       aes(x = windspeed, y = fit,color = factor(temp2))) + 
  geom_line()+scale_color_manual(values = cb_palette)



countwind.loess = loess(cnt ~ b_temp2 * windspeed, data = data,degree = 1)
countwind.grid = expand.grid(b_temp2 = c(-5, 0, 10,20,30), windspeed = c(0,10,20,30,40,50))
countwind.predict = predict(countwind.loess, newdata = countwind.grid)  

ggplot(data.frame(countwind.grid, fit = as.vector(countwind.predict)), 
       aes(x = windspeed, y = fit,color = factor(b_temp2))) + 
  geom_line()+scale_color_manual(values = cb_palette)


counthum.loess = loess(cnt ~ temp2 +humidity, data = data,degree = 1)
counthum.grid = expand.grid(temp2 = c(-5, 0, 10,20,30), humidity = c(0,40,60,80,100))
counthum.predict = predict(counthum.loess, newdata = counthum.grid)  

ggplot(data.frame(counthum.grid, fit = as.vector(counthum.predict)), 
       aes(x = humidity, y = fit,color = factor(temp2))) + 
  geom_line()+scale_color_manual(values = cb_palette)


#######line graphs#########


hum_palette <- c("#999999", "#0072B2", "#56B4E9","#009E73","#E69F00","#D55E00")
data$hum_factor <- cut(data$dp, breaks = c(-21,0,10,15, 19, 22, 26))
levels(data$hum_factor) <- c( c("Extreme Dry","Dry", "Comfortable","Alright", "Uncomfortable", "Extreme Unconfortable"))
df_hum <- data %>% group_by(Dew, hr,year) %>% summarise(avg_cnt = (sum(cnt)/n()))
p_hum <-ggplot(subset(df_hum,year==2012), aes(x=hr, y=avg_cnt, color=Dew)) +
  geom_line(size=1.1) + 
  scale_color_manual(name = "Comfort Level", values = hum_palette) + xlab('Hour') + ylab('Average Usage') + 
  ggtitle("Average Usage at Different Comfort Level") + 
  guides(colour = guide_legend(nrow=2,byrow=TRUE)) + 
  scale_x_continuous(breaks=seq(0,23)) + theme_bw()+
  theme(legend.position="bottom",
        legend.text = element_text(size='10'),
        plot.title=element_text(size='15'))
p_hum
 summary(df_hum)

 temp_palette <- c("#0072B2", "#56B4E9", "#009E73", "#E69F00", "#D55E00")
 temp_palette<-c()
 df$temp_factor <- cut(df$temp, breaks = c(40, 30, 20, 10, 0, -10))
 levels(df$temp_factor) <- c( "-10 ~   0°C","  0 ~  10°C"," 10 ~  20°C"," 20 ~  30°C"," 30 ~  40°C" )
 df_temp <- df %>% group_by(temp_factor, hr) %>% summarise(avg_cnt = (sum(cnt)/n())) 
 #%>% spread(key=temp_factor, value=avg_cnt)
 p_temp <-ggplot(data=df_temp, aes(x=hr, y=avg_cnt, color=temp_factor)) +
   geom_line(size=1.1) + 
   scale_color_manual(name = "Temperature (°C)", values = temp_palette) + 
   xlab('Hour') + ylab('Average Usage') + 
   ggtitle("Average Usage at Different Temperature Levels") + 
   guides(colour = guide_legend(reverse=T,nrow=2,byrow=TRUE)) + 
   scale_x_continuous(breaks=seq(0,23)) + theme_bw()+
   theme(legend.position="bottom",legend.text = element_text(size='10'),
         plot.title=element_text(size='13'))
 p_temp
 
 hum_palette <- c("#D55E00", "#E69F00", "#009E73", "#56B4E9", "#0072B2")
 df$hum_factor <- cut(df$hum, breaks = c(50, 60, 70, 80, 90, 100))
 levels(df$hum_factor) <- c( c("50 ~  60 %", "60 ~  70 %", "70 ~  80 %", "80 ~  90 %", "90 ~ 100 %"))
 df_hum <- df %>% group_by(hum_factor, hr) %>% summarise(avg_cnt = (sum(cnt)/n()))
 p_hum <-ggplot(data=na.omit(df_hum), aes(x=hr, y=avg_cnt, color=hum_factor)) + 
   geom_line(size=1.1) + 
   scale_color_manual(name = "Humidity (%)", values = temp_palette) + 
   xlab('Hour') + ylab('Average Usage') +
   ggtitle("Average Usage at Different Humidity Levels") +
   guides(colour = guide_legend(nrow=2,byrow=TRUE)) + 
   scale_x_continuous(breaks=seq(0,23)) + theme_bw()+
   theme(legend.position="bottom",legend.text = element_text(size='10'))
 p_hum
 
 yq <- as.yearqtr(as.yearmon(df$dteday, "%Y-%m-%d") + 1/12)
df$Seasons <- factor(format(yq, "%q"), levels = 1:4, 
                        labels = c("winter", "spring", "summer", "fall"))
 
 
season_palette <- c("#56B4E9","#009E73","#D55E00", "#E69F00",  "#0072B2")
df_season <- df %>% group_by(Seasons, hr) %>% summarise(avg_cnt = (sum(cnt)/n()))
ggplot(data=na.omit(df_season), aes(x=hr, y=avg_cnt, color=Seasons)) + 
   geom_line(size=1.1) + 
   scale_color_manual(name = "Seasons", values = season_palette) + 
   xlab('Hour') + ylab('Average Usage') +
   ggtitle("Average Usage in Different Seasons") +
   scale_x_continuous(breaks=seq(0,23)) + theme_bw()+
   theme(legend.text = element_text(size='10'))


ggplot(subset(data,year="2012"),aes(x=hr,y=cnt))+geom_point(alpha = 0.5,colour = "#56B4E9")+geom_smooth(colour="#D55E00")+
  facet_wrap(~weekday,nrow=4,ncol=2)+
  ylab('Count of bike used')+xlab('Hour of the day')+
  labs(title = 'Variation of Bike Usage in Weekdays') +theme_bw()


#####start prediction ######
data2 = read.csv("/Users/addytan/Desktop/IUB-19SP/EDA/hour.csv", header = TRUE)


work.loess = loess(cnt ~ workingday + weathersit*hr, data = data2,degree = 2)
work.grid = expand.grid(weathersit = c(1,2,3,4), hr = seq(0,23,1),workingday = c(0,1))
work.predict = predict(work.loess, newdata = work.grid)  

ggplot(data.frame(work.grid, fit = as.vector(work.predict)), 
       aes(x = hr, y = fit,color = factor(workingday))) + 
  facet_wrap(~weathersit)+
  geom_point()+scale_color_manual(values = cb_palette)
######poisson#########

glm_wwh = glm(casual ~ workingday * factor(hr)+weather, family = poisson, data = df) 
display(glm_wwh)
print('normal + workday')
pred_nor_wd_un = predict(glm_wwh, newdata = data.frame( weather="normal",workingday = 'workday', hr =seq(0, 23)), type = "response")
print('normal + non_workday')
pred_nor_nd_un = predict(glm_wwh, newdata = data.frame(weather="normal", workingday = 'non_workday', hr =seq(0, 23)), type = "response")


data_two<-cbind(c(1:24),pred_nor_wd_un, pred_nor_nd_un)
data_two<-as.data.frame(data_two) 
names(data_two)<-c('Hours','workday','non_workday')

longdata<-gather(data_two,Type,measurement,workday:non_workday,factor_key = TRUE)

ggplot(data=longdata, aes(x=Hours, y=measurement)) +
  geom_bar(stat="identity", position = "dodge",fill="#0072B2") + 
  facet_wrap(~Type, ncol = 7) + xlab('Hour') + 
  ylab('Usage')  + theme_bw()+
  scale_y_continuous(limits=c(0,500)) +
  labs(title = 'Predicted Bike Usage by Unregistered Users \nIn Working Days and Non Working Days')



####
glm_wwh = glm(casual ~ Seasons * factor(hr), family = poisson, data = noramdata) 
library(arm)
display(glm_wwh)
head(df$weathersit)

pred_nor_winter = predict(glm_wwh, newdata = data.frame(weathersit = 'Clear or partly Cloudy', Seasons = 'winter', hr =seq(0, 23)), type = "response")
pred_nor_fall = predict(glm_wwh, newdata = data.frame(weathersit = 'Clear or partly Cloudy', Seasons = 'fall', hr =seq(0, 23)), type = "response")
pred_nor_summer = predict(glm_wwh, newdata = data.frame(weathersit = 'Clear or partly Cloudy', Seasons = 'summer', hr =seq(0, 23)), type = "response")
pred_nor_spring = predict(glm_wwh, newdata = data.frame(weathersit = 'Clear or partly Cloudy', Seasons = 'spring', hr =seq(0, 23)), type = "response")


pred_li_winter = predict(glm_wwh, newdata = data.frame(weathersit = 'Light Snow or Rain', Seasons = 'winter', hr =seq(0, 23)), type = "response")
pred_li_fall = predict(glm_wwh, newdata = data.frame(weathersit = 'Light Snow or Rain', Seasons = 'fall', hr =seq(0, 23)), type = "response")
pred_li_summer = predict(glm_wwh, newdata = data.frame(weathersit = 'Light Snow or Rain', Seasons = 'summer', hr =seq(0, 23)), type = "response")
pred_li_spring = predict(glm_wwh, newdata = data.frame(weathersit = 'Light Snow or Rain', Seasons = 'spring', hr =seq(0, 23)), type = "response")

pred_mi_winter = predict(glm_wwh, newdata = data.frame(weathersit = 'Misty and Cloudy', Seasons = 'winter', hr =seq(0, 23)), type = "response")
pred_mi_fall = predict(glm_wwh, newdata = data.frame(weathersit = 'Misty and Cloudy', Seasons = 'fall', hr =seq(0, 23)), type = "response")
pred_mi_summer = predict(glm_wwh, newdata = data.frame(weathersit = 'Misty and Cloudy', Seasons = 'summer', hr =seq(0, 23)), type = "response")
pred_mi_spring = predict(glm_wwh, newdata = data.frame(weathersit = 'Misty and Cloudy', Seasons = 'spring', hr =seq(0, 23)), type = "response")

data_one<-cbind(c(1:24),pred_nor_winter, pred_nor_fall,pred_nor_summer,pred_nor_spring,
                       pred_li_winter, pred_li_fall,pred_li_summer,pred_li_spring,
                     pred_mi_winter, pred_mi_fall,pred_mi_summer,pred_mi_spring)
data_one<-as.data.frame(data_one) 
names(data_one)<-c('Hours','Noraml_Winter','Noraml_Fall',"Noraml_Summer",'Noraml_Spring',
                   'Light_Winter','Light_Fall',"Light_Summer",'Light_Spring',
                   'Misty_Winter','Misty_Fall',"Misty_Summer",'Misty_Spring')

longdata<-gather(data_one,Type,measurement,Noraml_Winter:Misty_Spring,factor_key = TRUE)

ggplot(data=longdata, aes(x=Hours, y=measurement)) +
  geom_bar(stat="identity", position = "dodge",fill="#0072B2") + 
  facet_wrap(~Type, ncol = 4) + xlab('Hour') + 
  ylab('Usage')  + theme_bw()+
  labs(title = 'Predicted Bike Usage by Unregistered Users \nIn Working Days and Non Working Days')


########??????##########
glm_wwh = glm(casual ~ workingday * factor(hr), family = poisson, data = noramdata) 
display(glm_wwh)
coefficients(summary(glm_wwh))
precinct.resid = residuals(glm_wwh, type = "response")
precinct.fitted = fitted.values(glm_wwh)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(noramdata, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_wwh)
overdispersion


print('normal + workday')
pred_nor_wd = predict(glm_wwh, newdata = data.frame(weather = 'normal', workingday = 'workday', hr =seq(0, 23)), type = "response")
print('normal + non_workday')
pred_nor_nd = predict(glm_wwh, newdata = data.frame(weather = 'normal', workingday = 'non_workday', hr =seq(0, 23)), type = "response")

data_one<-cbind(c(1:24),pred_nor_wd, pred_nor_nd)
data_one<-as.data.frame(data_one) 
names(data_one)<-c('Hours','Workday','Non_Workday')

longdata<-gather(data_one,Type,measurement,Workday:Non_Workday,factor_key = TRUE)

ggplot(data=longdata, aes(x=Hours, y=measurement)) +
  geom_bar(stat="identity", position = "dodge",fill="#D55E00") + 
  facet_wrap(~Type, ncol = 7) + xlab('Hour') + 
  ylab('Usage')  + theme_bw()+
  labs(title = 'Predicted Bike Usage by Registered Users \nIn Working Days and Non Working Days')




####
yq <- as.yearqtr(as.yearmon(df$dteday, "%Y-%m-%d") + 1/12)
df$Seasons <- factor(format(yq, "%q"), levels = 1:4, 
                       labels = c("winter", "spring", "summer", "fall"))


glm_wwh = glm(cnt ~ Seasons*factor(hr), family = poisson, data = noramdata) 
glm_wwh = glm(log(cnt) ~ Seasons*factor(hr), family =quasipoisson(link="log"), data = df) 

glm_wwh = glm(cnt ~ 1, family = poisson, data = noramdata) 

precinct.resid = residuals(glm_wwh, type = "response")
precinct.fitted = fitted.values(glm_wwh)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(noramdata, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_wwh)
overdispersion

ggplot(precinct.glm.df, aes(x = log(.fitted), y = .std.resid)) + geom_point() +
  geom_smooth(span = 1, method.args = list(degree = 1))

coefficients(summary(glm_wwh))[1:6, 1:2]

precinct.resid = residuals(glm_wwh_winter, type = "response")
precinct.fitted = fitted.values(glm_wwh_winter)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(noramdata, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_wwh_winter)
overdispersion

ggplot(precinct.glm.df, aes(x = log(.fitted), y = .std.resid)) + geom_point() +
  geom_smooth(span = 1, method.args = list(degree = 1))





print('normal + workday')
pred_winter = predict(glm_wwh, newdata = data.frame(Seasons = 'winter', hr =seq(0, 23)), type = "response")
print('normal + non_workday')
pred_fall = predict(glm_wwh, newdata = data.frame(Seasons = 'fall', hr =seq(0, 23)), type = "response")
pred_summer=predict(glm_wwh, newdata = data.frame(Seasons = 'summer', hr =seq(0, 23)), type = "response")
pred_spring=predict(glm_wwh, newdata = data.frame(Seasons = 'spring', hr =seq(0, 23)), type = "response")

data_season<-cbind(c(1:24),pred_winter, pred_spring,pred_summer,pred_fall)
data_season<-as.data.frame(data_season) 
names(data_season)<-c('Hours','winter','spring','summer','fall')


seasondata<-gather(data_season,Type,measurement,winter:fall,factor_key = TRUE)

season_palette <- c("#56B4E9","#009E73","#D55E00", "#E69F00",  "#0072B2")
ggplot(data=seasondata, aes(x=Hours, y=measurement,color=Type)) +
  geom_line(size=1.1) + 
  xlab('Hour') + 
  ylab('Usage')  + theme_bw()+
  scale_color_manual(values = season_palette) + 
  labs(title = 'Predicted Bike Usage \nIn Each Season by Hours')

data_season$Fall_Summer = data_season$"fall"-data_season$"summer"
data_season$Spring_Fall = data_season$"spring"-data_season$"fall"
data_season$Winter_Fall = data_season$"winter"-data_season$"fall"
data_season$Winter_Summer = data_season$"winter"-data_season$"summer"


diffdata<-gather(data_season,Type,measurement,Spring_Fall:Winter_Summer,factor_key = TRUE)

ggplot(data=diffdata, aes(x=Hours, y=measurement,color=Type)) +
  geom_line(size=1.1) + 
  xlab('Hour') + 
  ylab('Usage')  + theme_bw()+
  scale_color_manual(name = "Different Seasons",values = season_palette) + 
  labs(title = 'Predicted Bike Usage Difference \nIn Hours by Season')


pchisq(glm_wwh$deviance, df=glm_wwh$df.residual, lower.tail=FALSE)

observed<-seasondata
expected<-df%>%group_by(Seasons,hr)%>% summarise(avg_cnt = (sum(cnt)/n()))
chisq.test(expected$avg_cnt, observed$measurement)
head(expected)
head(observed)

chisq.test(observed$measurement)

glm_wwh = glm(registered ~ weather + workingday * factor(hr), family = poisson, data = df) 

chisq.test(observed$measurement)




glm_wwh = glm(casual ~ weather + workingday * factor(hr), family = poisson, data = df) 
anova(glm_wwh, test="Chisq")
####noraml####
glm_gest=glm(formula = cnt ~  workingday * factor(hr), family = quasipoisson, 
            data = noramdata)
precinct.resid = residuals(glm_gest, type = "response")
precinct.fitted = fitted.values(glm_gest)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(noramdata, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_gest)
overdispersion

#####log#####
workdata<-subset(df,workingday=="workday")
glm_wwh_work=glm(formula = cnt ~  Seasons* factor(hr), family = poisson, 
             data = workdata)

glm_wwh_winter=glm(formula = log(cnt) ~ Seasons * factor(hr), family = quasipoisson(link = "log"), 
    data = winterdata)


precinct.resid = residuals(glm_wwh_work, type = "response")
precinct.fitted = fitted.values(glm_wwh_work)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(workdata, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_wwh_work)
overdispersion


wwinterdata<-subset(df,Seasons=="winter")
glm_wwh_work=glm(formula = cnt ~  Seasons* factor(hr), family = poisson, 
                 data = noramdata)

noramdata<-subset(df, weather=="normal")

glm_wwh_winter=glm(formula = sqrt(cnt) ~ Seasons * factor(hr), family = quasipoisson(link = "log"), 
                   data = noramdata)


precinct.resid = residuals(glm_wwh_winter, type = "response")
precinct.fitted = fitted.values(glm_wwh_winter)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(noramdata, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_wwh_winter)
overdispersion


pred_winter = predict(glm_wwh_winter, newdata = data.frame(Seasons = 'winter', hr =seq(0, 23)), type = "response")
pred_fall = predict(glm_wwh_winter, newdata = data.frame(Seasons = 'fall', hr =seq(0, 23)), type = "response")
pred_summer=predict(glm_wwh_winter, newdata = data.frame(Seasons = 'summer', hr =seq(0, 23)), type = "response")
pred_spring=predict(glm_wwh_winter, newdata = data.frame(Seasons = 'spring', hr =seq(0, 23)), type = "response")

data_season<-cbind(c(0:23),pred_winter, pred_spring,pred_summer,pred_fall)
data_season<-as.data.frame(data_season) 
names(data_season)<-c('Hours','winter','spring','summer','fall')


seasondata<-gather(data_season,Type,measurement,winter:fall,factor_key = TRUE)

season_palette <- c("#56B4E9","#009E73","#D55E00", "#E69F00",  "#0072B2")
å
seas<-noramdata%>%group_by(Seasons,hr)%>%summarise(avg_cnt = (sum(cnt)/n()))

season_palette <- c("#56B4E9","#009E73","#D55E00", "#E69F00",  "#0072B2")
ggplot(data=seas, aes(x=hr, y=log(avg_cnt),color=Seasons)) +
  geom_line(size=1.1) + 
  xlab('Hour') + 
  ylab('Usage')  + theme_bw()+
  scale_color_manual(values = season_palette) + 
  labs(title = 'Expected Bike Log(Usage) \nIn Each Season by Hours')

seasondata$error<-seasondata$measurement-log(seas$avg_cnt)

ggplot(data=seasondata, aes(x=Hours, y=exp(error))) +
  geom_line(size=1.1) + 
  facet_wrap(~Type)
  xlab('Hour') + 
  ylab('Usage')  + theme_bw()+
  scale_color_manual(values = season_palette) + 
  labs(title = 'Expected Bike Log(Usage) \nIn Each Season by Hours')

chisq.test(seasondata$measurement)


####log 2#####
glm_wwh_winter=glm(formula = log(cnt) ~ Seasons * factor(hr), family = quasipoisson(link = "log"), 
                   data = df)

precinct.resid = residuals(glm_wwh_winter, type = "response")
precinct.fitted = fitted.values(glm_wwh_winter)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(noramdata, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_wwh_winter)
overdispersion

ggplot(precinct.glm.df, aes(x = log(.fitted), y = .std.resid)) + geom_point() +
  geom_smooth(span = 1, method.args = list(degree = 1))



glm_wwh_winter=glm(formula = log(cnt) ~ Seasons*factor(hr)*weather, family = quasipoisson(link = "log"), 
    data = noramdata)

precinct.resid = residuals(glm_wwh_winter, type = "response")
precinct.fitted = fitted.values(glm_wwh_winter)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(noramdata, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_wwh_winter)
overdispersion

#####last model log(cnt) ~ workingday*factor(hr)*factor(temp_factor)####

df$weathersit<-factor(df$weather,levels = c("normal","foggy","light_prec","heavy_prec"),labels = c('Clear or partly Cloudy',
                                                                                                   'Light Snow or Rain',
                                                                                                   'Misty and Cloudy','Misty and Cloudy'),ordered = TRUE)


df$temp_factor <- cut(df$temp, breaks = c(40, 30, 20, 10, 0, -10))
glm_wwh_winter=glm(formula = log(cnt) ~ workingday*factor(hr)*factor(temp_factor), family = quasipoisson(link = "log"), 
                   data = df)


precinct.resid = residuals(glm_wwh_winter, type = "response")
precinct.fitted = fitted.values(glm_wwh_winter)
precinct.std.resid = precinct.resid/sqrt(precinct.fitted)

precinct.glm.df = data.frame(df, .fitted = precinct.fitted, .resid = precinct.resid)
precinct.glm.df$.std.resid = precinct.std.resid
overdispersion = sum(precinct.std.resid^2)/df.residual(glm_wwh_winter)
overdispersion
ggplot(precinct.glm.df, aes(x = log(.fitted), y = .std.resid)) + geom_point() +
  geom_smooth(span = 1, method.args = list(degree = 1))


n1 = predict(glm_wwh_winter, newdata = data.frame(workingday = 'non_workday', hr =seq(0, 23),temp_factor="(-10,0]"), type = "response")
print('normal + non_workday')
n2 = predict(glm_wwh_winter, newdata = data.frame(workingday = 'workday', hr =seq(0, 23),temp_factor="(-10,0]"), type = "response")

n3=predict(glm_wwh_winter, newdata = data.frame(workingday = 'non_workday', hr =seq(0, 23), temp_factor="(0,10]"),type = "response")
n4=predict(glm_wwh_winter, newdata = data.frame(workingday = 'workday', hr =seq(0, 23), temp_factor="(0,10]"),type = "response")

l1 = predict(glm_wwh_winter, newdata = data.frame(workingday = 'non_workday', hr =seq(0, 23),temp_factor= "(10,20]"), type = "response")
l2 = predict(glm_wwh_winter, newdata = data.frame(workingday = 'workday', hr =seq(0, 23),temp_factor="(10,20]"), type = "response")

l3=predict(glm_wwh_winter, newdata = data.frame(workingday = 'non_workday', hr =seq(0, 23), temp_factor="(20,30]"),type = "response")
l4=predict(glm_wwh_winter, newdata = data.frame(workingday = 'workday', hr =seq(0, 23), temp_factor="(20,30]"),type = "response")

m1= predict(glm_wwh_winter, newdata = data.frame(workingday = 'non_workday', hr =seq(0, 23),temp_factor="(30,40]"), type = "response")
m2 = predict(glm_wwh_winter, newdata = data.frame(workingday = 'workday', hr =seq(0, 23),temp_factor="(30,40]"), type = "response")


data_level1<-cbind(c(1:24),c("-10 ~   0°C"),n1,n2)
data_level1<-as.data.frame(data_level1) 
names(data_level1)<-c('Hours','TempLevel','non_workday','workday')

data_level1 <- gather(data_level1, condition, measurement, non_workday:workday, factor_key=TRUE)
names(data_level1)<-c( "Hours", "TempLevel","condition"  , "measurement")

data_level2<-cbind(c(1:24),c("0 ~  10°C"),n3,n4)
data_level2<-as.data.frame(data_level2) 
names(data_level2)<-c('Hours','TempLevel','non_workday','workday')
data_level2 <- gather(data_level2, condition, measurement, non_workday:workday, factor_key=TRUE)
names(data_level2)<-names(data_level1)

data_level3<-cbind(c(1:24),c("10 ~  20°C"),l1,l2)
data_level3<-as.data.frame(data_level3) 
names(data_level3)<-c('Hours','TempLevel','non_workday','workday')
data_level3 <- gather(data_level3, condition, measurement, non_workday:workday, factor_key=TRUE)
names(data_level3)<-names(data_level1)

data_level4<-cbind(c(1:24),c("20 ~  30°C"),l3,l4)
data_level4<-as.data.frame(data_level4) 
names(data_level4)<-c('Hours','TempLevel','non_workday','workday')
data_level4 <- gather(data_level4, condition, measurement, non_workday:workday, factor_key=TRUE)
names(data_level4)<-names(data_level1)

data_level5<-cbind(c(1:24),c("30 ~  40°C"),m1,m2)
data_level5<-as.data.frame(data_level5) 
names(data_level5)<-c('Hours','TempLevel','non_workday','workday')
data_level5 <- gather(data_level5, condition, measurement, non_workday:workday, factor_key=TRUE)
names(data_level5)<-names(data_level1)

data_season<-rbind(data_level1,data_level2,data_level3,data_level4,data_level5)
data_season<-as.data.frame(data_season) 


data_season$Hours <- factor(data_season$Hours, levels=seq(1,24), ordered=TRUE)

data_season2<-cbind(data_season$Hours,
                    (data_season$TempLevel),
                    (data_season$condition),
                    as.numeric(data_season$measurement))
data_season2<-as.data.frame(data_season2)
names(data_season2)<-names(data_level1)
data_season2$TempLevel <- factor(data_season2$TempLevel, levels=c(1,2,3,4,5), labels= c( "-10 ~   0°C","  0 ~  10°C"," 10 ~  20°C"," 20 ~  30°C"," 30 ~  40°C" )
, ordered=TRUE)

data_season2$condition <- factor(data_season2$condition, levels=c(1,2), labels= c( "non_workday","workday" )
                                 , ordered=TRUE)


temp_palette <- c("#0072B2", "#56B4E9", "#009E73", "#E69F00", "#D55E00")
ggplot(data=data_season2, aes(x=Hours, y=exp(measurement),color=TempLevel)) +
  geom_line(size=1.1) + 
  facet_wrap(~condition)+
  xlab('Hour') + 
  ylab("Usage")  + theme_bw()+
  scale_color_manual(values = temp_palette,name = "Temperature (°C)") + 
  labs(title = 'Quasipoisson Predicted Bike Usage \nIn Each Temperature Level by Hours',
       subtitle="In Working and Non-working Day")




summary(df[which(df$hr==7),])

data_season2[which(data_season2$Hours==7&data_season2$TempLevel==" 30 ~  40°C"&data_season2$condition=="workday"),]$measurement<-NA

data_season2[which(data_season2$Hours==8&data_season2$TempLevel==" 30 ~  40°C"&data_season2$condition=="workday"),]$measurement<-NA
