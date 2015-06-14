# === script preparation === #
setwd("~/Documents/5 - ReproducibleResearch/project_2/")
rm(list=ls())
library(ggplot2)

# === read file === #
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#              ,"./repdata%2Fdata%2FStormData.csv.bz2",method = "wget")
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


par(mai=c(1,2,1,1))
barplot(plot_ev_pop$FATALITIES,names.arg=plot_ev_pop$EVTYPE,horiz = T,las=1)

par(mai=c(1,2,1,1))
barplot(data$PV,main="Page Views", horiz=TRUE,names.arg=names,las=1)

barplot(plot_ev_pop$FATALITIES,names.arg = plot_ev_pop$EVTYPE)


ggplot(plot_ev_pop,aes(x=plot_ev_pop$EVTYPE,y=plot_ev_pop$prcnt)) + geom_bar()



last_plot() + geom_bar()
aes(x=plot_ev_pop$EVTYPE,y=plot_ev_pop$prcnt)

ggplot(df, aes(x=reorder(Seller, Num), y=Avg_Cost)) +
    geom_bar(stat='identity') +
    coord_flip()


plot(plot_ev_pop$incr_prcnt)
plot(plot_ev_pop$prcnt)

# === question 2 ===#
#sd$damage <- sd$PROPDMG * switch(sd$PROPDMGEXP,"K"=1000,"M"=1000000,"B"=1000000000,1)
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


qplot(ev_pop$EVTYPE,ev_pop$prcnt,main = "Event type by Fatalities Share",xlab="Event",ylab="Fatalities")


