# === script preparation === #
setwd("~/Documents/5 - ReproducibleResearch/project_2/")
rm(list=ls())
library(ggplot2)-

# === read file === #
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
              ,"./repdata%2Fdata%2FStormData.csv.bz2",method = "wget")
#unzip(zipfile = "./repdata%2Fdata%2FStormData.csv.bz2",overwrite = T,exdir = "./")
sd <- read.csv("./repdata%2Fdata%2FStormData.csv.bz2",T,",")

# === question 1 ===#
ev_pop <- na.omit(aggregate(list(sd$FATALITIES,sd$INJURIES),by=list(sd$EVTYPE),FUN=sum,na.rm=T))
names(ev_pop) <- c("EVTYPE","FATALITIES","INJURIES")
ev_pop <- ev_pop[order(-ev_pop$FATALITIES),]
#ev_pop$percentage <- (1:nrow(ev_pop))/nrow(ev_pop)
ev_pop$prcnt <- ev_pop$FATALITIES/sum(ev_pop$FATALITIES)*100
ev_pop$incr_prcnt <- cumsum(ev_pop$FATALITIES)/sum(ev_pop$FATALITIES)*100
plot_ev_pop <- ev_pop[ev_pop$incr_prcnt<=80,]
plot(plot_ev_pop$incr_prcnt)
plot(plot_ev_pop$prcnt)

# === question 2 ===#
sd$damage <- sd$PROPDMG * switch(sd$PROPDMGEXP,"K"=1000,"M"=1000000,"B"=1000000000,1)
sd$damage <- sd$PROPDMG * ifelse(sd$PROPDMGEXP=="K",1000,
                                 ifelse(sd$PROPDMGEXP=="M",1000000,ifelse(sd$PROPDMGEXP=="B",1000000000,1)))

ev_propdmg <- na.omit(aggregate(list(sd$damage),by=list(sd$EVTYPE),FUN=sum,na.rm=T))
names(ev_propdmg) <- c("EVTYPE","damage")
ev_propdmg <- ev_propdmg[order(-ev_propdmg$damage),]
ev_propdmg$prcnt <- ev_propdmg$damage/sum(ev_propdmg$damage)*100
ev_propdmg$incr_prcnt <- cumsum(ev_propdmg$damage)/sum(ev_propdmg$damage)*100
plot_ev_propdmg <- ev_propdmg[ev_propdmg$incr_prcnt<=80,]
plot(plot_ev_propdmg$incr_prcnt)
plot(plot_ev_propdmg$prcnt)


barplot(mean_steps$x,names.arg=mean_steps$Group.1,main="Mean and median total number of steps taken per day")
lines(median_steps$Group.1,median_steps$x,col="red")

qplot(interval,steps,data = wday_adj_amd,facets = wday ~ .,geom = "line")
qplot(interval,steps,data = wday_adj_amd,facets = . ~ wday,geom = "line")

