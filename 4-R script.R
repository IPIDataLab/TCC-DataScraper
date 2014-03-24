########################################################
##This R script is the final step in updating the IPI
##Peacekeeping database. It reads in the last month scraped,
##binds it to the full data, reaggregates all summary data
##frames, generates country level csvs and graphics as well 
##as the csvs for download on the site. These files are
##written into the Documents folder and should be uploaded
##to the appropriate director on the server.
##
##Before running this script remember to conver contribution
##0's to NA's.
##
##Note- you have to export plot.current.region, plot.current.continent and plot.current.continent manually
########################################################

library(reshape2)
library(ggplot2)
library(plyr)
library(scales)
library(RColorBrewer)
library(geosphere)
library(reldist)
source("http://peterhaschke.com/Code/multiplot.R")

#set appropriate working directory
setwd("~/Dropbox/Projects/active/TCC/TCCDataScraper")

###################################
##PROCESS IMPORTED DATA
###################################
#read in old data
data.full <- read.csv("tcc_files/Data.Full.csv")
gender.full <- read.csv("tcc_files/Gender.Full.csv")

#import data exported by sql query as temp add files. must be in the 'Documents' folder and titled 'contributsions.csv' and 'gender.csv' respectively
new.data.full <- read.csv('tcc_files/contributions.csv')
new.gender.full <- read.csv('tcc_files/gender.csv')

#calculate distance between capital and mission hq 
new.data.full$distance <- distHaversine(p1=new.data.full[,5:6],p2=new.data.full[,38:39]) / 1609.34

#Change col names to correct format
data.full.col.names <- c('date','tcc','tcc.iso3.alpha','tcc.cap','tcc.cap.long','tcc.cap.lat','tcc.continent','tcc.unregion','tcc.unbloc','tcc.p5g4a3','tcc.nam','tcc.g77','tcc.au','tcc.arab.league','tcc.oic','tcc.cis','tcc.g20','tcc.eu','tcc.nato','tcc.g8','tcc.oecd','tcc.asean','tcc.oas','tcc.shanghai','tcc.gcc','tcc.uma','tcc.comesa','tcc.censad','tcc.eac','tcc.eccas','tcc.ecowas','tcc.igad','tcc.sadc','mission','mission.country','mission.iso3.alpha','mission.hq','mission.hq.long','mission.hq.lat','mission.continent','mission.un.region','mission.un.bloc','mission.p5g4a3','mission.nam','mission.g77','mission.au','mission.arab.league','mission.oic','mission.cis','mission.g20','mission.eu','mission.nato','mission.g8','mission.oecd','mission.asean','mission.oas','mission.shanghai','mission.gcc','mission.uma','mission.comesa','mission.censad','mission.eac','mission.eccas','mission.ecowas','mission.igad','mission.sadc','eom','milobvs','civpol','fpu','ip','troops','police','observers','total','distance')
gender.full.col.names <- c('date','tcc','tcc.iso3.alpha','mission','ip.M','ip.F','ip.T','fpu.M','fpu.F','fpu.T','eom.M','eom.F','eom.T','troops.M','troops.F','troops.T','total.M','total.F','total.T')
colnames(new.data.full) <- data.full.col.names
colnames(new.gender.full) <- gender.full.col.names
colnames(data.full) <- data.full.col.names
colnames(gender.full) <- gender.full.col.names
rm(data.full.col.names)
rm(gender.full.col.names)

#format date column as date data type
new.data.full$date <- as.Date(new.data.full$date,"%m/%d/%y")
new.gender.full$date <- as.Date(new.gender.full$date,"%m/%d/%y")
data.full$date <- as.Date(data.full$date)
gender.full$date <- as.Date(gender.full$date)

#set dummy variables as factors
new.data.full$tcc.nam <- as.factor(new.data.full$tcc.nam)
new.data.full$tcc.g77 <- as.factor(new.data.full$tcc.g77)
new.data.full$tcc.au <- as.factor(new.data.full$tcc.au)
new.data.full$tcc.arab.league <- as.factor(new.data.full$tcc.arab.league)
new.data.full$tcc.oic <- as.factor(new.data.full$tcc.oic)
new.data.full$tcc.cis <- as.factor(new.data.full$tcc.cis)
new.data.full$tcc.g20 <- as.factor(new.data.full$tcc.g20)
new.data.full$tcc.eu <- as.factor(new.data.full$tcc.eu)
new.data.full$tcc.nato <- as.factor(new.data.full$tcc.nato)
new.data.full$tcc.oecd <- as.factor(new.data.full$tcc.oecd)
new.data.full$tcc.asean <- as.factor(new.data.full$tcc.asean)
new.data.full$tcc.oas <- as.factor(new.data.full$tcc.oas)
new.data.full$tcc.shanghai <- as.factor(new.data.full$tcc.shanghai)
new.data.full$tcc.gcc <- as.factor(new.data.full$tcc.gcc)
new.data.full$tcc.uma <- as.factor(new.data.full$tcc.uma)
new.data.full$tcc.comesa <- as.factor(new.data.full$tcc.comesa)
new.data.full$tcc.censad <- as.factor(new.data.full$tcc.censad)
new.data.full$tcc.eac <- as.factor(new.data.full$tcc.eac)
new.data.full$tcc.eccas <- as.factor(new.data.full$tcc.eccas)
new.data.full$tcc.ecowas <- as.factor(new.data.full$tcc.ecowas)
new.data.full$tcc.igad <- as.factor(new.data.full$tcc.igad)
new.data.full$tcc.sadc <- as.factor(new.data.full$tcc.sadc)
new.data.full$mission.nam <- as.factor(new.data.full$mission.nam)
new.data.full$mission.g77 <- as.factor(new.data.full$mission.g77)
new.data.full$mission.au <- as.factor(new.data.full$mission.au)
new.data.full$mission.arab.league <- as.factor(new.data.full$mission.arab.league)
new.data.full$mission.oic <- as.factor(new.data.full$mission.oic)
new.data.full$mission.cis <- as.factor(new.data.full$mission.cis)
new.data.full$mission.g20 <- as.factor(new.data.full$mission.g20)
new.data.full$mission.eu <- as.factor(new.data.full$mission.eu)
new.data.full$mission.nato <- as.factor(new.data.full$mission.nato)
new.data.full$mission.oecd <- as.factor(new.data.full$mission.oecd)
new.data.full$mission.asean <- as.factor(new.data.full$mission.asean)
new.data.full$mission.oas <- as.factor(new.data.full$mission.oas)
new.data.full$mission.shanghai <- as.factor(new.data.full$mission.shanghai)
new.data.full$mission.gcc <- as.factor(new.data.full$mission.gcc)
new.data.full$mission.uma <- as.factor(new.data.full$mission.uma)
new.data.full$mission.comesa <- as.factor(new.data.full$mission.comesa)
new.data.full$mission.censad <- as.factor(new.data.full$mission.censad)
new.data.full$mission.eac <- as.factor(new.data.full$mission.eac)
new.data.full$mission.eccas <- as.factor(new.data.full$mission.eccas)
new.data.full$mission.ecowas <- as.factor(new.data.full$mission.ecowas)
new.data.full$mission.igad <- as.factor(new.data.full$mission.igad)
new.data.full$mission.sadc <- as.factor(new.data.full$mission.sadc)

data.full$tcc.nam <- as.factor(data.full$tcc.nam)
data.full$tcc.g77 <- as.factor(data.full$tcc.g77)
data.full$tcc.au <- as.factor(data.full$tcc.au)
data.full$tcc.arab.league <- as.factor(data.full$tcc.arab.league)
data.full$tcc.oic <- as.factor(data.full$tcc.oic)
data.full$tcc.cis <- as.factor(data.full$tcc.cis)
data.full$tcc.g20 <- as.factor(data.full$tcc.g20)
data.full$tcc.eu <- as.factor(data.full$tcc.eu)
data.full$tcc.nato <- as.factor(data.full$tcc.nato)
data.full$tcc.oecd <- as.factor(data.full$tcc.oecd)
data.full$tcc.asean <- as.factor(data.full$tcc.asean)
data.full$tcc.oas <- as.factor(data.full$tcc.oas)
data.full$tcc.shanghai <- as.factor(data.full$tcc.shanghai)
data.full$tcc.gcc <- as.factor(data.full$tcc.gcc)
data.full$tcc.uma <- as.factor(data.full$tcc.uma)
data.full$tcc.comesa <- as.factor(data.full$tcc.comesa)
data.full$tcc.censad <- as.factor(data.full$tcc.censad)
data.full$tcc.eac <- as.factor(data.full$tcc.eac)
data.full$tcc.eccas <- as.factor(data.full$tcc.eccas)
data.full$tcc.ecowas <- as.factor(data.full$tcc.ecowas)
data.full$tcc.igad <- as.factor(data.full$tcc.igad)
data.full$tcc.sadc <- as.factor(data.full$tcc.sadc)
data.full$mission.nam <- as.factor(data.full$mission.nam)
data.full$mission.g77 <- as.factor(data.full$mission.g77)
data.full$mission.au <- as.factor(data.full$mission.au)
data.full$mission.arab.league <- as.factor(data.full$mission.arab.league)
data.full$mission.oic <- as.factor(data.full$mission.oic)
data.full$mission.cis <- as.factor(data.full$mission.cis)
data.full$mission.g20 <- as.factor(data.full$mission.g20)
data.full$mission.eu <- as.factor(data.full$mission.eu)
data.full$mission.nato <- as.factor(data.full$mission.nato)
data.full$mission.oecd <- as.factor(data.full$mission.oecd)
data.full$mission.asean <- as.factor(data.full$mission.asean)
data.full$mission.oas <- as.factor(data.full$mission.oas)
data.full$mission.shanghai <- as.factor(data.full$mission.shanghai)
data.full$mission.gcc <- as.factor(data.full$mission.gcc)
data.full$mission.uma <- as.factor(data.full$mission.uma)
data.full$mission.comesa <- as.factor(data.full$mission.comesa)
data.full$mission.censad <- as.factor(data.full$mission.censad)
data.full$mission.eac <- as.factor(data.full$mission.eac)
data.full$mission.eccas <- as.factor(data.full$mission.eccas)
data.full$mission.ecowas <- as.factor(data.full$mission.ecowas)
data.full$mission.igad <- as.factor(data.full$mission.igad)
data.full$mission.sadc <- as.factor(data.full$mission.sadc)

#bind new data to old data
data.full <- rbind(data.full, new.data.full)
gender.full <- rbind(gender.full, new.gender.full)

###################################
##MONTHLY ANALYSIS
###################################
#Set current date
current.date <- format(new.data.full[1,1], format="%B %Y")

#set pie options
pie.options <- theme_bw() + theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
                     axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border=element_blank())

##Plot pie of current deployments

#Regional deployments
#subset data
current.region.deploy <- ddply(new.data.full, .(mission.continent,mission.un.region),summarise,
                               n.missions = length(unique(mission)),
                               troops = sum(troops,na.rm=TRUE),
                               police = sum(police,na.rm=TRUE),
                               observers = sum(observers,na.rm=TRUE),
                               total = sum(total,na.rm=TRUE))
current.region.deploy$mission.perc <- current.region.deploy$n.missions / sum(current.region.deploy$n.missions)
current.region.deploy$troops.perc <- current.region.deploy$troops / sum(current.region.deploy$troops)
current.region.deploy$pol.perc <- current.region.deploy$police / sum(current.region.deploy$police)
current.region.deploy$obsv.perc <- current.region.deploy$observers / sum(current.region.deploy$observers)
current.region.deploy$tot.perc <- current.region.deploy$total / sum(current.region.deploy$total)

#set color palatte
paired <- brewer.pal(name="Paired", n=nlevels(current.region.deploy$mission.un.region))
names(paired) <- rev(levels(current.region.deploy$mission.un.region))

#plot
plot.troops.region <- ggplot(current.region.deploy,aes(x=factor(1),y=troops.perc,fill = factor(mission.un.region))) 
plot.troops.region <- plot.troops.region + geom_bar() + coord_polar(theta="y") + scale_y_continuous(labels = percent)
plot.troops.region <- plot.troops.region + labs(title = paste('Troop Deployments', current.date),fill='Region',color='Region')
plot.troops.region <- plot.troops.region + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.police.region <- ggplot(current.region.deploy,aes(x=factor(1),y=pol.perc,fill = factor(mission.un.region))) 
plot.police.region <- plot.police.region + geom_bar() + coord_polar(theta="y") + scale_y_continuous(labels = percent)
plot.police.region <- plot.police.region + labs(title = paste('Police Deployments', current.date),fill='Region',color='Region')
plot.police.region <- plot.police.region + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.observers.region <- ggplot(current.region.deploy,aes(x=factor(1),y=obsv.perc,fill = factor(mission.un.region))) 
plot.observers.region <- plot.observers.region + geom_bar() + coord_polar(theta="y") + scale_y_continuous(labels = percent)
plot.observers.region <- plot.observers.region + labs(title = paste('Experts on Mission Deployments', current.date),fill='Region',color='Region')
plot.observers.region <- plot.observers.region + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.total.region <- ggplot(current.region.deploy,aes(x=factor(1),y=tot.perc,fill = factor(mission.un.region))) 
plot.total.region <- plot.total.region + geom_bar() + coord_polar(theta="y") + scale_y_continuous(labels = percent)
plot.total.region <- plot.total.region + labs(title = paste('Total Deployments', current.date),fill='Region',color='Region')
plot.total.region <- plot.total.region + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

#set plots on single page
plot.current.region <- multiplot(plot.troops.region, plot.police.region, plot.observers.region, plot.total.region, cols=2)

#Regional deployments
#subset data
current.continent.deploy <- ddply(new.data.full, .(mission.continent),summarise,
                               n.missions = length(unique(mission)),
                               troops = sum(troops,na.rm=TRUE),
                               police = sum(police,na.rm=TRUE),
                               observers = sum(observers,na.rm=TRUE),
                               total = sum(total,na.rm=TRUE))
current.continent.deploy$mission.perc <- current.continent.deploy$n.missions / sum(current.continent.deploy$n.missions)
current.continent.deploy$troops.perc <- current.continent.deploy$troops / sum(current.continent.deploy$troops)
current.continent.deploy$pol.perc <- current.continent.deploy$police / sum(current.continent.deploy$police)
current.continent.deploy$obsv.perc <- current.continent.deploy$observers / sum(current.continent.deploy$observers)
current.continent.deploy$tot.perc <- current.continent.deploy$total / sum(current.continent.deploy$total)

#reset color palatte
paired <- brewer.pal(name="Paired", n=nlevels(current.continent.deploy$mission.continent))
names(paired) <- rev(levels(current.continent.deploy$mission.continent))

#plot
plot.troops.continent <- ggplot(current.continent.deploy,aes(x=factor(1),y=troops.perc,fill = factor(mission.continent))) 
plot.troops.continent <- plot.troops.continent + geom_bar() + coord_polar(theta="y") + theme_bw() + scale_y_continuous(labels = percent)
plot.troops.continent <- plot.troops.continent + labs(title = paste('Total Deployments', current.date),fill='Continent',color='Continent')
plot.troops.continent <- plot.troops.continent + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.police.continent <- ggplot(current.continent.deploy,aes(x=factor(1),y=pol.perc,fill = factor(mission.continent))) 
plot.police.continent <- plot.police.continent + geom_bar() + coord_polar(theta="y") + theme_bw() + scale_y_continuous(labels = percent)
plot.police.continent <- plot.police.continent + labs(title = paste('Police Deployments', current.date),fill='Continent',color='Continent')
plot.police.continent <- plot.police.continent + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.observers.continent <- ggplot(current.continent.deploy,aes(x=factor(1),y=obsv.perc,fill = factor(mission.continent))) 
plot.observers.continent <- plot.observers.continent + geom_bar() + coord_polar(theta="y") + theme_bw() + scale_y_continuous(labels = percent)
plot.observers.continent <- plot.observers.continent + labs(title = paste('Experts on Mission Deployments', current.date),fill='Continent',color='Continent')
plot.observers.continent <- plot.observers.continent + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.total.continent <- ggplot(current.continent.deploy,aes(x=factor(1),y=tot.perc,fill = factor(mission.continent))) 
plot.total.continent <- plot.total.continent + geom_bar() + coord_polar(theta="y") + theme_bw() + scale_y_continuous(labels = percent)
plot.total.continent <- plot.total.continent + labs(title = paste('Police Deployments', current.date),fill='Continent',color='Continent')
plot.total.continent <- plot.total.continent + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

#set plots on single page
plot.current.continent <- multiplot(plot.troops.continent, plot.police.continent, plot.observers.continent, plot.total.continent, cols=2)

##Plot pie of current contributions
#Regional deployments
current.region.cont <- ddply(new.data.full, .(tcc.continent,tcc.unregion),summarise,
                               troops = sum(troops,na.rm=TRUE),
                               police = sum(police,na.rm=TRUE),
                               observers = sum(observers,na.rm=TRUE),
                               total = sum(total,na.rm=TRUE))
current.region.cont$troops.perc <- current.region.cont$troops / sum(current.region.cont$troops)
current.region.cont$pol.perc <- current.region.cont$police / sum(current.region.cont$police)
current.region.cont$obsv.perc <- current.region.cont$observers / sum(current.region.cont$observers)
current.region.cont$tot.perc <- current.region.cont$total / sum(current.region.cont$total)

#Create component plots
plot.troopcont.region <- ggplot(current.region.cont,aes(x=factor(tcc.unregion),y=troops.perc)) 
plot.troopcont.region <- plot.troopcont.region + geom_bar() + scale_y_continuous(labels = percent)
plot.troopcont.region <- plot.troopcont.region + labs(title = paste('Troop Contributions', current.date),x='Region',y='Percentage of total contributions')
plot.troopcont.region <- plot.troopcont.region + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot.policecont.region <- ggplot(current.region.cont,aes(x=factor(tcc.unregion),y=pol.perc)) 
plot.policecont.region <- plot.policecont.region + geom_bar() + scale_y_continuous(labels = percent)
plot.policecont.region <- plot.policecont.region + labs(title = paste('Police Contributions', current.date),x='Region',y='Percentage of total contributions')
plot.policecont.region <- plot.policecont.region + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot.observerscont.region <- ggplot(current.region.cont,aes(x=factor(tcc.unregion),y=obsv.perc)) 
plot.observerscont.region <- plot.observerscont.region + geom_bar() + scale_y_continuous(labels = percent)
plot.observerscont.region <- plot.observerscont.region + labs(title = paste('Experts on Mission Contributions', current.date),x='Region',y='Percentage of total contributions')
plot.observerscont.region <- plot.observerscont.region + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot.totalcont.region <- ggplot(current.region.cont,aes(x=factor(tcc.unregion),y=tot.perc)) 
plot.totalcont.region <- plot.totalcont.region + geom_bar() + scale_y_continuous(labels = percent)
plot.totalcont.region <- plot.totalcont.region + labs(title = paste('Total Contributions', current.date),x='Region',y='Percentage of total contributions')
plot.totalcont.region <- plot.totalcont.region + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(plot.troopcont.region,file='tcc_files/current_month/current_continental_troop_contributions.png',height=5,width=12)
ggsave(plot.policecont.region,file='tcc_files/current_month/current_continental_police_contributions.png',height=5,width=12)
ggsave(plot.observerscont.region,file='tcc_files/current_month/current_continentall_observer_contributions.png',height=5,width=12)
ggsave(plot.totalcont.region,file='tcc_files/current_month/current_continental_contributions.png',height=5,width=12)

#continent contributions
#Subset data
current.continent.cont <- ddply(new.data.full, .(tcc.continent),summarise,
                                  troops = sum(troops,na.rm=TRUE),
                                  police = sum(police,na.rm=TRUE),
                                  observers = sum(observers,na.rm=TRUE),
                                  total = sum(total,na.rm=TRUE))
current.continent.cont$troops.perc <- current.continent.cont$troops / sum(current.continent.cont$troops)
current.continent.cont$pol.perc <- current.continent.cont$police / sum(current.continent.cont$police)
current.continent.cont$obsv.perc <- current.continent.cont$observers / sum(current.continent.cont$observers)
current.continent.cont$tot.perc <- current.continent.cont$total / sum(current.continent.cont$total)

#reset color palatte
paired <- brewer.pal(name="Paired", n=nlevels(current.continent.cont$tcc.continent))
names(paired) <- rev(levels(current.continent.cont$tcc.continent))

#Create Plots
plot.troopcont.continent <- ggplot(current.continent.cont,aes(x=factor(1),y=troops.perc,fill = factor(tcc.continent))) 
plot.troopcont.continent <- plot.troopcont.continent + geom_bar() + coord_polar(theta="y") + theme_bw() + scale_y_continuous(labels = percent)
plot.troopcont.continent <- plot.troopcont.continent + labs(title = paste('Troop Contributions', current.date),fill='Continent',color='Continent')
plot.troopcont.continent <- plot.troopcont.continent + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.policecont.continent <- ggplot(current.continent.cont,aes(x=factor(1),y=pol.perc,fill = factor(tcc.continent))) 
plot.policecont.continent <- plot.policecont.continent + geom_bar() + coord_polar(theta="y") + theme_bw() + scale_y_continuous(labels = percent)
plot.policecont.continent <- plot.policecont.continent + labs(title = paste('Police Contributions', current.date),fill='Continent',color='Continent')
plot.policecont.continent <- plot.policecont.continent + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.observerscont.continent <- ggplot(current.continent.cont,aes(x=factor(1),y=obsv.perc,fill = factor(tcc.continent))) 
plot.observerscont.continent <- plot.observerscont.continent + geom_bar() + coord_polar(theta="y") + theme_bw() + scale_y_continuous(labels = percent)
plot.observerscont.continent <- plot.observerscont.continent + labs(title = paste('Experts on Mission Contributions', current.date),fill='Continent',color='Continent')
plot.observerscont.continent <- plot.observerscont.continent + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.totalcont.continent <- ggplot(current.continent.cont,aes(x=factor(1),y=tot.perc,fill = factor(tcc.continent))) 
plot.totalcont.continent <- plot.totalcont.continent + geom_bar() + coord_polar(theta="y") + theme_bw() + scale_y_continuous(labels = percent)
plot.totalcont.continent <- plot.totalcont.continent + labs(title = paste('Total Contributions', current.date),fill='Continent',color='Continent')
plot.totalcont.continent <- plot.troopcont.continent + pie.options + scale_fill_manual(values=paired) + scale_color_manual(values=paired)

plot.currentcont.continent <- multiplot(plot.troopcont.continent, plot.policecont.continent, plot.observerscont.continent, plot.totalcont.continent, cols=2)

######Things to add
#list of top 10 contributors total and by type
#regional contributions and deployments% - donuts one for each type
#regional contributions and deployments-totals - stacked bar
#organizational contributions - one stacked bar for each 3 charts one for each depoloyment type
###g8
###g77
###NAM
###AU
###EU
#financial contriubutions


rm(new.data.full)
rm(new.gender.full)
rm(paired)
rm(pie.options)
rm(plot.current.continent)
rm(plot.current.region)
rm(plot.observers.continent)
rm(plot.currentcont.continent)
rm(plot.observers.region)
rm(plot.observerscont.continent)
rm(plot.observerscont.region)
rm(plot.police.continent)
rm(plot.police.region)
rm(plot.policecont.continent)
rm(plot.policecont.region)
rm(plot.total.continent)
rm(plot.total.region)
rm(plot.totalcont.continent)
rm(plot.totalcont.region)
rm(plot.troopcont.continent)
rm(plot.troopcont.region)
rm(plot.troops.continent)
rm(plot.troops.region)
rm(current.continent.cont)
rm(current.continent.deploy)
rm(current.region.cont)
rm(current.region.deploy)
rm(current.date)

###################################
##REGIONAL AGGREGATION
###################################
###replace 0's with NA's and fix data.full$total and convert to int
data.full$eom[data.full$eom == 0] <- NA
data.full$milobvs[data.full$milobvs == 0] <- NA
data.full$civpol[data.full$civpol == 0] <- NA
data.full$fpu[data.full$fpu == 0] <- NA
data.full$ip[data.full$ip == 0] <- NA
data.full$troops[data.full$troops == 0] <- NA
data.full$police[data.full$police == 0] <- NA
data.full$observers[data.full$observers == 0] <- NA
data.full$total[data.full$total == 0] <- NA

#region aggregation from full
data.full.region.tcc <- ddply(data.full, .(date,tcc.unregion,tcc.continent), summarise,
                              n.contributors = length(unique(tcc)),
                              troops = sum(troops,na.rm=TRUE),
                              police = sum(police,na.rm=TRUE),
                              observers = sum(observers,na.rm=TRUE),
                              total = sum(total,na.rm=TRUE))
#replace 0's with NA's
data.full.region.tcc[] <- lapply(data.full.region.tcc, function(x){replace(x, x == 0, NA)})

###create continent subsets and subset plots
#Africa
data.full.region.tcc.africa <- data.full.region.tcc[which(data.full.region.tcc$tcc.continent=='Africa'),]
data.full.region.tcc.africa$tcc.unregion <- as.character(data.full.region.tcc.africa$tcc.unregion)
data.full.region.tcc.africa$tcc.unregion = factor(data.full.region.tcc.africa$tcc.unregion,
                                                  levels=c('Western Africa','Eastern Africa','Northern Africa','Southern Africa','Middle Africa'),
                                                  ordered=TRUE)
data.full.region.tcc.africa$tcc.continent <- as.character(data.full.region.tcc.africa$tcc.continent)

plot.region.africa <- ggplot(data.full.region.tcc.africa, aes(date,total))
plot.region.africa <- plot.region.africa + geom_area(aes(color=tcc.unregion,fill=tcc.unregion,order=desc(tcc.unregion)), position="stack")
plot.region.africa <- plot.region.africa + labs(title = 'Africa',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.africa <- plot.region.africa + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.region.africa <- plot.region.africa + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
reds <- brewer.pal(name="Reds", n=nlevels(data.full.region.tcc.africa$tcc.unregion))
names(reds) <- rev(levels(data.full.region.tcc.africa$tcc.unregion))
plot.region.africa <- plot.region.africa + scale_fill_manual(values=reds) + scale_color_manual(values=reds)

#Americas
data.full.region.tcc.americas <- data.full.region.tcc[which(data.full.region.tcc$tcc.continent=='South America' | data.full.region.tcc$tcc.continent=='North America'),]
data.full.region.tcc.americas$tcc.unregion <- as.character(data.full.region.tcc.americas$tcc.unregion)
data.full.region.tcc.americas$tcc.unregion = factor(data.full.region.tcc.americas$tcc.unregion,
                                                    levels=c('South America','Central America','Northern America','Caribbean'),
                                                    ordered=TRUE)
data.full.region.tcc.americas$tcc.continent <- as.character(data.full.region.tcc.americas$tcc.continent)
data.full.region.tcc.americas$tcc.continent <- as.factor(data.full.region.tcc.americas$tcc.continent)

plot.region.americas <- ggplot(data.full.region.tcc.americas, aes(date,total))
plot.region.americas <- plot.region.americas + geom_area(aes(color=tcc.unregion, fill=tcc.unregion,order=desc(tcc.unregion)), position="stack")
plot.region.americas <- plot.region.americas + labs(title = 'Americas',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.americas <- plot.region.americas + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.region.americas <- plot.region.americas + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
oranges <- brewer.pal(name="Oranges", n=nlevels(data.full.region.tcc.americas$tcc.unregion))
names(oranges) <- rev(levels(data.full.region.tcc.americas$tcc.unregion))
plot.region.americas <- plot.region.americas + scale_fill_manual(values=oranges) + scale_color_manual(values=oranges)

#Asia
data.full.region.tcc.asia <- data.full.region.tcc[which(data.full.region.tcc$tcc.continent=='Asia'),]
data.full.region.tcc.asia$tcc.unregion <- as.character(data.full.region.tcc.asia$tcc.unregion)
data.full.region.tcc.asia$tcc.unregion = factor(data.full.region.tcc.asia$tcc.unregion,
                                                levels=c('Southern Asia','Western Asia','South-Eastern Asia','Eastern Asia','Central Asia'),
                                                ordered=TRUE)
data.full.region.tcc.asia$tcc.continent <- as.character(data.full.region.tcc.asia$tcc.continent)

plot.region.asia <- ggplot(data.full.region.tcc.asia, aes(date,total))
plot.region.asia <- plot.region.asia + geom_area(aes(fill=tcc.unregion,color=tcc.unregion, fill=tcc.unregion,order=desc(tcc.unregion)), position="stack")
plot.region.asia <- plot.region.asia + labs(title = 'Asia',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.asia <- plot.region.asia + theme_bw() + scale_x_date(labels = date_format('%Y'), breaks = date_breaks('year'))
plot.region.asia <- plot.region.asia + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
greens <- brewer.pal(name="Greens", n=nlevels(data.full.region.tcc.asia$tcc.unregion))
names(greens) <- rev(levels(data.full.region.tcc.asia$tcc.unregion))
plot.region.asia <- plot.region.asia + scale_fill_manual(values=greens) + scale_color_manual(values=greens)

#Europe
data.full.region.tcc.europe <- data.full.region.tcc[which(data.full.region.tcc$tcc.continent=='Europe'),]
data.full.region.tcc.europe$tcc.unregion <- as.character(data.full.region.tcc.europe$tcc.unregion)
data.full.region.tcc.europe$tcc.unregion = factor(data.full.region.tcc.europe$tcc.unregion,
                                                  levels=c('Southern Europe','Western Europe','Eastern Europe','Northern Europe'),
                                                  ordered=TRUE)
data.full.region.tcc.europe$tcc.continent <- as.character(data.full.region.tcc.europe$tcc.continent)

plot.region.europe <- ggplot(data.full.region.tcc.europe, aes(date,total))
plot.region.europe <- plot.region.europe + geom_area(aes(color=tcc.unregion, fill=tcc.unregion,order=desc(tcc.unregion)), position="stack")
plot.region.europe <- plot.region.europe + labs(title = 'Europe',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.europe <- plot.region.europe + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.region.europe <- plot.region.europe + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
blues <- brewer.pal(name="Blues", n=nlevels(data.full.region.tcc.europe$tcc.unregion))
names(blues) <- rev(levels(data.full.region.tcc.europe$tcc.unregion))
plot.region.europe <- plot.region.europe + scale_fill_manual(values=blues) + scale_color_manual(values=blues)

#Oceania
data.full.region.tcc.oceania <- data.full.region.tcc[which(data.full.region.tcc$tcc.continent=='Oceania'),]
data.full.region.tcc.oceania$tcc.unregion <- as.character(data.full.region.tcc.oceania$tcc.unregion)
data.full.region.tcc.oceania$tcc.unregion <- as.factor(data.full.region.tcc.oceania$tcc.unregion)
data.full.region.tcc.oceania$tcc.unregion = factor(data.full.region.tcc.oceania$tcc.unregion,
                                                   levels=c('Melanesia','Australia and New Zealand','Polynesia','Micronesia'),
                                                   ordered=TRUE)
data.full.region.tcc.oceania$tcc.continent <- as.character(data.full.region.tcc.oceania$tcc.continent)

plot.region.oceania <- ggplot(data.full.region.tcc.oceania, aes(date,total))
plot.region.oceania <- plot.region.oceania + geom_area(aes(color=tcc.unregion, fill=tcc.unregion,order=desc(tcc.unregion)), position="stack")
plot.region.oceania <- plot.region.oceania + labs(title = 'Oceania',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.oceania <- plot.region.oceania + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.region.oceania <- plot.region.oceania + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
purples <- brewer.pal(name="Purples", n=nlevels(data.full.region.tcc.oceania$tcc.unregion))
names(purples) <- rev(levels(data.full.region.tcc.oceania$tcc.unregion))
plot.region.oceania <- plot.region.oceania + scale_fill_manual(values=purples) + scale_color_manual(values=purples)

#write subsets to region dir 
write.csv(data.full.region.tcc.africa,'tcc_files/regions/Africa.csv',row.names=FALSE)
write.csv(data.full.region.tcc.americas,'tcc_files/regions/Americas.csv',row.names=FALSE)
write.csv(data.full.region.tcc.asia,'tcc_files/regions/Asia.csv',row.names=FALSE)
write.csv(data.full.region.tcc.europe,'tcc_files/regions/Europe.csv',row.names=FALSE)
write.csv(data.full.region.tcc.oceania,'tcc_files/regions/Oceania.csv',row.names=FALSE)
write.csv(data.full.region.tcc,'tcc_files/Data.Regions.csv',row.names=FALSE)
ggsave(plot.region.africa,file='tcc_files/regions/Africa.png',height=8.5,width=11)
ggsave(plot.region.americas,file='tcc_files/regions/Americas.png',height=8.5,width=11)
ggsave(plot.region.asia,file='tcc_files/regions/Asia.png',height=8.5,width=11)
ggsave(plot.region.europe,file='tcc_files/regions/Europe.png',height=8.5,width=11)
ggsave(plot.region.oceania,file='tcc_files/regions/Oceania.png',height=8.5,width=11)

rm(data.full.region.tcc)
rm(data.full.region.tcc.africa)
rm(data.full.region.tcc.asia)
rm(data.full.region.tcc.americas)
rm(data.full.region.tcc.europe)
rm(data.full.region.tcc.oceania)
rm(blues)
rm(greens)
rm(oranges)
rm(purples)
rm(reds)
rm(plot.region.africa)
rm(plot.region.asia)
rm(plot.region.americas)
rm(plot.region.europe)
rm(plot.region.oceania)

###################################
##CONTINENT AGGREGATION
###################################
#continent aggregation from full
data.full.continent <- ddply(data.full, .(date,tcc.continent), summarise,
                             n.contributors = length(unique(tcc)),
                             troops = sum(troops,na.rm=TRUE),
                             police = sum(police,na.rm=TRUE),
                             observers = sum(observers,na.rm=TRUE),
                             total = sum(total,na.rm=TRUE))

###plot continent graphs
#total contributions
plot.continent.total <- ggplot(data.full.continent, aes(date,total))
plot.continent.total <- plot.continent.total + geom_line(aes(color=tcc.continent))
plot.continent.total <- plot.continent.total + labs(title = 'Total Contributions by Continent',x='Year',y='Total UN Peacekeeping Contributions',color='Continent')
plot.continent.total <- plot.continent.total + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.total <- plot.continent.total + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.total <- plot.continent.total + scale_color_brewer(palette='Paired')

#police contributions
plot.continent.police <- ggplot(data.full.continent, aes(date,police))
plot.continent.police <- plot.continent.police + geom_line(aes(color=tcc.continent))
plot.continent.police <- plot.continent.police + labs(title = 'Police Contributions by Continent',x='Year',y='Total UN Police Contributions',color='Continent')
plot.continent.police <- plot.continent.police + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.police <- plot.continent.police + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.police <- plot.continent.police + scale_color_brewer(palette='Paired')

#observer contributions
plot.continent.observers <- ggplot(data.full.continent, aes(date,observers))
plot.continent.observers <- plot.continent.observers + geom_line(aes(color=tcc.continent))
plot.continent.observers <- plot.continent.observers + labs(title = 'Observer and EoM Contributions by Continent',x='Year',y='Total UN Observer and Eom Contributions',color='Continent')
plot.continent.observers <- plot.continent.observers + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.observers <- plot.continent.observers + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.observers <- plot.continent.observers + scale_color_brewer(palette='Paired')

#troop contributions
plot.continent.troops <- ggplot(data.full.continent, aes(date,troops))
plot.continent.troops <- plot.continent.troops + geom_line(aes(color=tcc.continent))
plot.continent.troops <- plot.continent.troops + labs(title = 'Troop Contributions by Continent',x='Year',y='Total UN Troop Contributions',color='Continent')
plot.continent.troops <- plot.continent.troops + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.troops <- plot.continent.troops + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.troops <- plot.continent.troops + scale_color_brewer(palette='Paired')

#number of contributors
plot.continent.contributors <- ggplot(data.full.continent, aes(date,n.contributors))
plot.continent.contributors <- plot.continent.contributors + geom_line(aes(color=tcc.continent))
plot.continent.contributors <- plot.continent.contributors + labs(title = 'Contributors by Continent',x='Year',y='Number of Troop Contributing Countries',color='Continent')
plot.continent.contributors <- plot.continent.contributors + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.contributors <- plot.continent.contributors + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.contributors <- plot.continent.contributors + scale_color_brewer(palette='Paired')

#write subsets to continent dir 
write.csv(data.full.continent,'tcc_files/Data.Continent.csv',row.names=FALSE)
ggsave(plot.continent.total,file='tcc_files/continent/Total.png',height=8.5,width=11)
ggsave(plot.continent.police,file='tcc_files/continent/Police.png',height=8.5,width=11)
ggsave(plot.continent.observers,file='tcc_files/continent/Observer.png',height=8.5,width=11)
ggsave(plot.continent.troops,file='tcc_files/continent/Troops.png',height=8.5,width=11)
ggsave(plot.continent.contributors,file='tcc_files/continent/Contributors.png',height=8.5,width=11)

rm(data.full.continent)
rm(plot.continent.contributors)
rm(plot.continent.observers)
rm(plot.continent.police)
rm(plot.continent.total)
rm(plot.continent.troops)

###################################
##COUNTRY AGGREGATION
###################################
#Create a list of countries
tcc.vector<- as.vector(unique(data.full$tcc))

#country aggregation from full
data.full.tcc <- ddply(data.full, .(date,tcc,tcc.iso3.alpha,tcc.cap.long,tcc.cap.lat,tcc.continent,tcc.unregion,tcc.unbloc,tcc.p5g4a3,tcc.nam,tcc.g77,tcc.au,tcc.arab.league,tcc.oic,tcc.cis,tcc.g20,tcc.eu),
                       summarise,
                       n.missions = length(unique(mission)),
                       troops.sum = sum(troops,na.rm=TRUE),
                       troops.mean = mean(troops,na.rm=TRUE),
                       troops.med = median(troops,na.rm=TRUE),
                       troops.sd = sd(troops,na.rm=TRUE),
                       police.sum = sum(police,na.rm=TRUE),
                       police.mean = mean(police,na.rm=TRUE),
                       police.med = median(police,na.rm=TRUE),
                       police.sd = sd(police,na.rm=TRUE),
                       observers.sum = sum(observers,na.rm=TRUE),
                       observers.mean = mean(observers,na.rm=TRUE),
                       observers.med = median(observers,na.rm=TRUE),
                       observers.sd = sd(observers,na.rm=TRUE),
                       total.sum = sum(total,na.rm=TRUE),
                       total.mean = mean(total,na.rm=TRUE),
                       total.med = median(total,na.rm=TRUE),
                       total.sd = sd(total,na.rm=TRUE),
                       dist.mean =  mean(distance,na.rm=TRUE))

#remove 0's un troop, police, observers, and total columns
tmp <- data.full.tcc[,c(19,23,27,31)]
tmp[] <- lapply(tmp, function(x){replace(x, x == 0, NA)})
data.full.tcc[,c(19,23,27,31)] <- tmp
rm(tmp)

#Loop through country list to create and save individual csv's and plots
lapply(tcc.vector, function(c){
  tmp <- data.full.tcc[which(data.full.tcc$tcc == c),c(1,19,23,27)]
  tmp.cols <- c('Date','Troops','Police','Experts')
  colnames(tmp) <- tmp.cols
  write.csv(tmp,paste0('tcc_files/countries/',c,'.csv'),row.names=FALSE)
  tmp <- melt(tmp,id.vars='Date')
  tmp.cols <- c('Date','Type','Value')
  colnames(tmp) <- tmp.cols
  tmp.p <- ggplot(tmp, aes(Date,Value))
  tmp.p <- tmp.p + geom_line(aes(color=Type,), size=1.5, alpha=.7)  
  tmp.p <- tmp.p + labs(title=paste('Figure 1:', c,'Uniformed Personnel Contributions to UN Peacekeeping Operations, 1990-2014', sep=" "),x='Year',y='', color='')
  tmp.p <- tmp.p + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
  tmp.p <- tmp.p + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(size = 7))
  ggsave(tmp.p,file=paste0('tcc_files/countries/',c,'.png'),height=4,width=6)
})

write.csv(data.full.tcc,'tcc_files/Data.TCC.csv',row.names=FALSE)
rm(tcc.vector)

###################################n
##MISSION AGGREGATION
###################################
#mission aggregation from full
data.full.mission <- ddply(data.full, .(date,mission,mission.country,mission.iso3.alpha,mission.hq,mission.hq.long,mission.hq.lat,mission.continent,mission.un.region,mission.nam,mission.g77,mission.au,mission.arab.league,mission.oic,mission.cis,mission.g20,mission.eu),
                           summarise,
                           n.contributors = length(unique(tcc)),
                           troops = sum(troops,na.rm=TRUE),
                           police = sum(police,na.rm=TRUE),
                           observers = sum(observers,na.rm=TRUE),
                           total = sum(total,na.rm=TRUE))

#replace 0's with NA's
data.full.mission[] <- lapply(data.full.mission, function(x){replace(x, x == 0, NA)})

#regional deployments
data.full.mission.region <- ddply(data.full.mission, .(date,mission.continent,mission.un.region),summarise,
                                  n.missions = length(unique(mission)),
                                  troops = sum(troops,na.rm=TRUE),
                                  police = sum(police,na.rm=TRUE),
                                  observers = sum(observers,na.rm=TRUE),
                                  total = sum(total,na.rm=TRUE))

#Africa
data.full.mission.region.africa <- data.full.mission.region[which(data.full.mission.region$mission.continent=='Africa'),]
data.full.mission.region.africa$mission.un.region <- as.character(data.full.mission.region.africa$mission.un.region)
data.full.mission.region.africa$mission.un.region = factor(data.full.mission.region.africa$mission.un.region,
                                                levels=c('Northern Africa','Middle Africa','Western Africa','Eastern Africa'),
                                                ordered=TRUE)
data.full.mission.region.africa$mission.continent <- as.character(data.full.mission.region.africa$mission.continent)

plot.region.deployments.africa <- ggplot(data.full.mission.region.africa, aes(date,total))
plot.region.deployments.africa <- plot.region.deployments.africa + geom_area(aes(color=mission.un.region, fill=mission.un.region, order=desc(mission.un.region)), position="stack")
plot.region.deployments.africa <- plot.region.deployments.africa + labs(title = 'African Regional Deployments',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.deployments.africa <- plot.region.deployments.africa + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.region.deployments.africa <- plot.region.deployments.africa + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.region.deployments.africa <- plot.region.deployments.africa + scale_fill_brewer(palette='Reds') + scale_color_brewer(palette='Reds')

#Asia
data.full.mission.region.asia <- data.full.mission.region[which(data.full.mission.region$mission.continent=='Asia'),]
data.full.mission.region.asia$mission.un.region <- as.character(data.full.mission.region.asia$mission.un.region)
data.full.mission.region.asia$mission.un.region = factor(data.full.mission.region.asia$mission.un.region,
                                                           levels=c('Central Asia','Western Asia','Southern Asia','South-Eastern Asia'),
                                                           ordered=TRUE)
data.full.mission.region.asia$mission.continent <- as.character(data.full.mission.region.asia$mission.continent)

plot.region.deployments.asia <- ggplot(data.full.mission.region.asia, aes(date,total))
plot.region.deployments.asia <- plot.region.deployments.asia + geom_area(aes(color=mission.un.region, fill=mission.un.region, order=desc(mission.un.region)), position="stack")
plot.region.deployments.asia <- plot.region.deployments.asia + labs(title = 'Asian Regional Deployments',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.deployments.asia <- plot.region.deployments.asia + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.region.deployments.asia <- plot.region.deployments.asia + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.region.deployments.asia <- plot.region.deployments.asia + scale_fill_brewer(palette='Greens') + scale_color_brewer(palette='Greens')

#Americas
data.full.mission.region.americas <- data.full.mission.region[which(data.full.mission.region$mission.continent=='South America' | data.full.mission.region$mission.continent=='North America'),]
data.full.mission.region.americas$mission.un.region <- as.character(data.full.mission.region.americas$mission.un.region)
data.full.mission.region.americas$mission.un.region <- as.factor(data.full.mission.region.americas$mission.un.region)
data.full.mission.region.americas$mission.continent <- as.character(data.full.mission.region.americas$mission.continent)

plot.region.deployments.americas <- ggplot(data.full.mission.region.americas, aes(date,total))
plot.region.deployments.americas <- plot.region.deployments.americas + geom_area(aes(color=mission.un.region, fill=mission.un.region, order=desc(mission.un.region)), position="stack")
plot.region.deployments.americas <- plot.region.deployments.americas + labs(title = 'Americas Regional Deployments',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.deployments.americas <- plot.region.deployments.americas + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.region.deployments.americas <- plot.region.deployments.americas + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.region.deployments.americas <- plot.region.deployments.americas + scale_fill_brewer(palette='Oranges') + scale_color_brewer(palette='Oranges')

#Europe
data.full.mission.region.europe <- data.full.mission.region[which(data.full.mission.region$mission.continent=='Europe'),]
data.full.mission.region.europe$mission.un.region <- as.character(data.full.mission.region.europe$mission.un.region)
data.full.mission.region.europe$mission.un.region <- as.factor(data.full.mission.region.europe$mission.un.region)
data.full.mission.region.europe$mission.continent <- as.character(data.full.mission.region.europe$mission.continent)

plot.region.deployments.europe <- ggplot(data.full.mission.region.europe, aes(date,total))
plot.region.deployments.europe <- plot.region.deployments.europe + geom_area(aes(color=mission.un.region, fill=mission.un.region, order=desc(mission.un.region)), position="stack")
plot.region.deployments.europe <- plot.region.deployments.europe + labs(title = 'European Regional Deployments',x='Year',y='Total UN Peacekeeping Contributions',fill='Region',color='Region')
plot.region.deployments.europe <- plot.region.deployments.europe + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.region.deployments.europe <- plot.region.deployments.europe + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.region.deployments.europe <- plot.region.deployments.europe + scale_fill_brewer(palette='Blues') + scale_color_brewer(palette='Blues')

#continent deployments
data.full.mission.continent <- ddply(data.full.mission, .(date,mission.continent),summarise,
                                     n.missions = length(unique(mission)),
                                     troops = sum(troops,na.rm=TRUE),
                                     police = sum(police,na.rm=TRUE),
                                     observers = sum(observers,na.rm=TRUE),
                                     total = sum(total,na.rm=TRUE))
#total deployments plot
plot.continent.deployments.total <- ggplot(data.full.mission.continent, aes(date,total))
plot.continent.deployments.total <- plot.continent.deployments.total + geom_line(aes(color=mission.continent))
plot.continent.deployments.total <- plot.continent.deployments.total + labs(title = 'Total Deployments by Continent',x='Year',y='Total UN Peacekeeping Deployments',color='Continent')
plot.continent.deployments.total <- plot.continent.deployments.total + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.deployments.total <- plot.continent.deployments.total + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.deployments.total <- plot.continent.deployments.total + scale_color_brewer(palette='Paired')

#number of missions plot
plot.continent.mission <- ggplot(data.full.mission.continent, aes(date,n.missions))
plot.continent.mission <- plot.continent.mission + geom_line(aes(color=mission.continent))
plot.continent.mission <- plot.continent.mission + labs(title = 'Missions by Continent',x='Year',y='Number of Ongoing Missions by Continent',color='Continent')
plot.continent.mission <- plot.continent.mission + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.mission <- plot.continent.mission + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.mission <- plot.continent.mission + scale_color_brewer(palette='Paired')


#write subsets to mission dir 
write.csv(data.full.mission,'tcc_files/Data.Mission.csv',row.names=FALSE)
write.csv(data.full.mission.region,'tcc_files/mission/regiol_deployments.csv',row.names=FALSE)
write.csv(data.full.mission.continent,'tcc_files/mission/continent_deployments.csv',row.names=FALSE)

ggsave(plot.region.deployments.africa,file='tcc_files/mission/african_regional_deployments.png',height=8.5,width=11)
ggsave(plot.region.deployments.asia,file='tcc_files/mission/asian_regional_deployments.png',height=8.5,width=11)
ggsave(plot.region.deployments.americas,file='tcc_files/mission/americas_regional_deployments.png',height=8.5,width=11)
ggsave(plot.region.deployments.europe,file='tcc_files/mission/european_regional_deployments.png',height=8.5,width=11)
ggsave(plot.continent.deployments.total,file='tcc_files/mission/total_deployments_by_continent.png',height=8.5,width=11)
ggsave(plot.continent.mission,file='tcc_files/mission/missions_by_continent.png',height=8.5,width=11)

rm(data.full.mission)
rm(data.full.mission.continent)
rm(data.full.mission.region)
rm(data.full.mission.region.africa)
rm(data.full.mission.region.asia)
rm(data.full.mission.region.europe)
rm(data.full.mission.region.americas)
rm(plot.continent.deployments.total)
rm(plot.continent.mission)
rm(plot.region.deployments.africa)
rm(plot.region.deployments.americas)
rm(plot.region.deployments.asia)
rm(plot.region.deployments.europe)

###################################
##SUMMARY AGGREGATION
###################################
#summary from full aggregation
data.summary.monthly.full <- ddply(data.full, .(date), summarise,
                                   n.conributors = length(unique(tcc)),
                                   troops.sum = sum(troops,na.rm=TRUE),
                                   troops.mean = mean(troops,na.rm=TRUE),
                                   troops.med = median(troops,na.rm=TRUE),
                                   troops.sd = sd(troops,na.rm=TRUE),
                                   police.sum = sum(police,na.rm=TRUE),
                                   police.mean = mean(police,na.rm=TRUE),
                                   police.med = median(police,na.rm=TRUE),
                                   police.sd = sd(police,na.rm=TRUE),
                                   observers.sum = sum(observers,na.rm=TRUE),
                                   observers.mean = mean(observers,na.rm=TRUE),
                                   observers.med = median(observers,na.rm=TRUE),
                                   observers.sd = sd(observers,na.rm=TRUE),
                                   total.sum = sum(total,na.rm=TRUE),
                                   total.mean = mean(total,na.rm=TRUE),
                                   total.med = median(total,na.rm=TRUE),
                                   total.sd = sd(total,na.rm=TRUE),
                                   dist.mean =  mean(distance,na.rm=TRUE))

#plot mean distance of contributors to missions
plot.monthly.mean.dist <- ggplot(data.summary.monthly.full, aes(date,dist.mean))
plot.monthly.mean.dist <- plot.monthly.mean.dist + geom_line()
plot.monthly.mean.dist <- plot.monthly.mean.dist + labs(title='Mean Distance of Contributor Capital to Mission HQ',x='Year',y='Mean Distance (Miles)')
plot.monthly.mean.dist <- plot.monthly.mean.dist + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.monthly.mean.dist <- plot.monthly.mean.dist + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))

#create aggregation of total monthly deploytments
summary.tmp <- data.summary.monthly.full[,c(1,3,7,11)]
summary.tmp.cols <- c('Date','Troops','Police','Observers')
colnames(summary.tmp) <- summary.tmp.cols
rm(summary.tmp.cols)
summary.tmp <- melt(summary.tmp, id.vars='Date')
summary.tmp$variable <- as.character(summary.tmp$variable)
summary.tmp$variable <- as.factor(summary.tmp$variable)
summary.tmp$variable <- factor(summary.tmp$variable,levels=c('Troops','Police','Observers'),ordered=TRUE)
plot.monthly.totals <- ggplot(summary.tmp, aes(Date,value))
plot.monthly.totals <- plot.monthly.totals + geom_area(aes(color=variable, fill=variable,order=desc(variable)), position="stack")
plot.monthly.totals <- plot.monthly.totals + labs(title = 'Monthly UN Peacekeeping Deployments ',x='Year',y='Total UN Peacekeepers',fill='Type',color='Type')
plot.monthly.totals <- plot.monthly.totals + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.monthly.totals <- plot.monthly.totals + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
colors <- brewer.pal(name="Blues", n=nlevels(summary.tmp$variable))
names(colors) <- rev(levels(summary.tmp$variable))
plot.monthly.totals <- plot.monthly.totals + scale_fill_manual(values=colors) + scale_color_manual(values=colors)

#summary from full aspirant aggregation
data.summary.monthly.aspirant <- ddply(data.full, .(date,tcc.p5g4a3), summarise,
                                      troops.sum = sum(troops,na.rm=TRUE),
                                      troops.mean = mean(troops,na.rm=TRUE),
                                      troops.med = median(troops,na.rm=TRUE),
                                      troops.sd = sd(troops,na.rm=TRUE),
                                      police.sum = sum(police,na.rm=TRUE),
                                      police.mean = mean(police,na.rm=TRUE),
                                      police.med = median(police,na.rm=TRUE),
                                      police.sd = sd(police,na.rm=TRUE),
                                      observers.sum = sum(observers,na.rm=TRUE),
                                      observers.mean = mean(observers,na.rm=TRUE),
                                      observers.med = median(observers,na.rm=TRUE),
                                      observers.sd = sd(observers,na.rm=TRUE),
                                      total.sum = sum(total,na.rm=TRUE),
                                      total.mean = mean(total,na.rm=TRUE),
                                      total.med = median(total,na.rm=TRUE),
                                      total.sd = sd(total,na.rm=TRUE))
aspirant.cols <- c('date','grouping','Troops','troops.mean','troops.med','troops.sd','Police','police.mean','police.median','police.sd','Obvservers','obvservers.mean','obvservers.median','obvservers.sd','Total','total.mean','total.median','total.sd')
colnames(data.summary.monthly.aspirant) <- aspirant.cols
plot.aspirants <- ggplot(data.summary.monthly.aspirant, aes(date,total.mean))
plot.aspirants <- plot.aspirants + geom_line(aes(color=grouping))
plot.aspirants <- plot.aspirants + labs(title='P5 vs Aspirational Council Member Mean Contributions',x='Year',y='Mean UN Peacekeeping Contributions', color='Aspirational Group')
plot.aspirants <- plot.aspirants + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.aspirants <- plot.aspirants + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.aspirants <- plot.aspirants + scale_color_brewer(palette='Paired')

#summary from tcc aggregation
data.summary.monthly.tcc <- ddply(data.full.tcc, .(date), summarise,
                                  n.conributors = length(unique(tcc)),
                                  troops = sum(troops.sum,na.rm=TRUE),
                                  troops.mean = mean(troops.sum,na.rm=TRUE),
                                  troops.med = median(troops.sum,na.rm=TRUE),
                                  troops.sd = sd(troops.sum,na.rm=TRUE),
                                  police = sum(police.sum,na.rm=TRUE),
                                  police.mean = mean(police.sum,na.rm=TRUE),
                                  police.med = median(police.sum,na.rm=TRUE),
                                  police.sd = sd(police.sum,na.rm=TRUE),
                                  observers = sum(observers.sum,na.rm=TRUE),
                                  observers.mean = mean(observers.sum,na.rm=TRUE),
                                  observers.med = median(observers.sum,na.rm=TRUE),
                                  observers.se = sd(observers.sum,na.rm=TRUE),
                                  total = sum(total.sum,na.rm=TRUE),
                                  total.mean = mean(total.sum,na.rm=TRUE),
                                  total.med = median(total.sum,na.rm=TRUE),
                                  total.sd = sd(total.sum,na.rm=TRUE),
                                  troops.gini = gini(!is.na(troops.sum)),
                                  total.gini = gini(total.sum),
                                  total.quint.first=sum(total.sum[total.sum <= quantile(total.sum,c(.2))]),
                                  total.quint.second=sum(total.sum[total.sum <= quantile(total.sum,c(.4))]),
                                  total.quint.third=sum(total.sum[total.sum <= quantile(total.sum,c(.6))]),
                                  total.quint.fourth=sum(total.sum[total.sum <= quantile(total.sum,c(.8))]),
                                  total.quint.fifth=sum(total.sum[total.sum <= quantile(total.sum,c(1))]))

#troop quintiles
tmp <- data.full.tcc[complete.cases(data.full.tcc$troops.sum),]
tmp <- ddply(tmp, .(date), summarise,
             troops.quint.first=sum(troops.sum[troops.sum <= quantile(troops.sum,c(.2))]),
             troops.quint.second=sum(troops.sum[troops.sum <= quantile(troops.sum,c(.4))]),
             troops.quint.third=sum(troops.sum[troops.sum <= quantile(troops.sum,c(.6))]),
             troops.quint.fourth=sum(troops.sum[troops.sum <= quantile(troops.sum,c(.8))]),
             troops.quint.fifth=sum(troops.sum[troops.sum <= quantile(troops.sum,c(1))]))

#merge
data.summary.monthly.tcc$troops.quint.first <- tmp$troops.quint.first
data.summary.monthly.tcc$troops.quint.second <- tmp$troops.quint.second
data.summary.monthly.tcc$troops.quint.third <- tmp$troops.quint.third
data.summary.monthly.tcc$troops.quint.fourth <- tmp$troops.quint.fourth
data.summary.monthly.tcc$troops.quint.fifth <- tmp$troops.quint.fifth

#Remove 0's
data.summary.monthly.tcc[] <- lapply(data.summary.monthly.tcc, function(x){replace(x, x == 0, NA)})

#plot troop quantiles
tmp <- data.summary.monthly.tcc[,c(1,3,26:30)]
tmp$Fifth <- (tmp$troops.quint.fifth - tmp$troops.quint.fourth) / tmp$troops
tmp$Fourth <- (tmp$troops.quint.fourth - tmp$troops.quint.third) / tmp$troops
tmp$Third <- (tmp$troops.quint.third - tmp$troops.quint.second) / tmp$troops
tmp$Second <- (tmp$troops.quint.second - tmp$troops.quint.first) / tmp$troops
tmp$First <- tmp$troops.quint.first / tmp$troops
tmp <- tmp[,c(1,8:12)]
tmp <- melt(tmp,id.var='date')
tmp$variable <- as.factor(tmp$variable)

plot.troops.quintiles <- ggplot(tmp,aes(date,value))
plot.troops.quintiles <- plot.troops.quintiles + geom_line(aes(color=variable))
plot.troops.quintiles <- plot.troops.quintiles + labs(title = 'Troop contributions by Quintile',x='Year',y='Percentage of Total Troop Contributions',color='Quintile')
plot.troops.quintiles <- plot.troops.quintiles + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.troops.quintiles <- plot.troops.quintiles + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.troops.quintiles <- plot.troops.quintiles + scale_color_brewer(palette='Paired')

#plot total quantiles
tmp <- data.summary.monthly.tcc[,c(1,15,21:25)]
tmp$Fifth <- (tmp$total.quint.fifth - tmp$total.quint.fourth) / tmp$total
tmp$Fourth <- (tmp$total.quint.fourth - tmp$total.quint.third) / tmp$total
tmp$Third <- (tmp$total.quint.third - tmp$total.quint.second) / tmp$total
tmp$Second <- (tmp$total.quint.second - tmp$total.quint.first) / tmp$total
tmp$First <- tmp$total.quint.first / tmp$total
tmp <- tmp[,c(1,8:12)]
tmp <- melt(tmp,id.var='date')
tmp$variable <- as.factor(tmp$variable)

plot.total.quintiles <- ggplot(tmp,aes(date,value))
plot.total.quintiles <- plot.total.quintiles + geom_line(aes(color=variable))
plot.total.quintiles <- plot.total.quintiles + labs(title = 'Total contributions by Quintile',x='Year',y='Percentage of Total Contributions',color='Quintile')
plot.total.quintiles <- plot.total.quintiles + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.total.quintiles <- plot.total.quintiles + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.total.quintiles <- plot.total.quintiles + scale_color_brewer(palette='Paired')

#write files to dir
write.csv(data.summary.monthly.full,'tcc_files/Data.Monthly.csv',row.names=FALSE)
write.csv(data.summary.monthly.aspirant,'tcc_files/summaries/aspirants.csv',row.names=FALSE)
write.csv(data.summary.monthly.tcc,'tcc_files/summaries/distribution.csv',row.names=FALSE)
ggsave(plot.aspirants,file='tcc_files/summaries/aspirants.png',height=8.5,width=11)
ggsave(plot.monthly.totals,file='tcc_files/summaries/monthly_deployments.png',height=8.5,width=11)
ggsave(plot.troops.quintiles,file='tcc_files/summaries/troop_quintiles.png',height=8.5,width=11)
ggsave(plot.total.quintiles,file='tcc_files/summaries/total_quintiles.png',height=8.5,width=11)

rm(data.full.tcc)
rm(data.summary.monthly.aspirant)
rm(data.summary.monthly.full)
rm(data.summary.monthly.tcc)
rm(summary.tmp)
rm(tmp)
rm(aspirant.cols)
rm(colors)
rm(plot.aspirants)
rm(plot.monthly.mean.dist)
rm(plot.monthly.totals)
rm(plot.total.quintiles)
rm(plot.troops.quintiles)

###################################
##GENDER AGGREGATION
###################################
###manually melt data
#pull out individual police data
tmp <- gender.full[,c(1:2,4,5:7)]
tmp.cols <- c('Date', 'Contributor', 'Mission', 'Male', 'Female', 'Total')
colnames(tmp) <- tmp.cols
tmp$Type <- 'Individual Police'
gender.mission <- tmp
gender.tcc <- tmp
gender.summary <- tmp

#pull out formed police unit data
tmp <- gender.full[,c(1:2,4,8:10)]
tmp.cols <- c('Date', 'Contributor', 'Mission', 'Male', 'Female', 'Total')
colnames(tmp) <- tmp.cols
tmp$Type <- 'Formed Police Units'
gender.mission <- rbind(gender.mission,tmp)
gender.tcc <- rbind(gender.mission,tmp)
gender.summary <- rbind(gender.summary,tmp)

#pull out eom data
tmp <- gender.full[,c(1:2,4,11:13)]
tmp.cols <- c('Date', 'Contributor', 'Mission', 'Male', 'Female', 'Total')
colnames(tmp) <- tmp.cols
tmp$Type <- 'Experts on Mission'
gender.mission <- rbind(gender.mission,tmp)
gender.tcc <- rbind(gender.mission,tmp)
gender.summary <- rbind(gender.summary,tmp)

#pull out troops data
tmp <- gender.full[,c(1:2,4,14:16)]
tmp.cols <- c('Date', 'Contributor', 'Mission', 'Male', 'Female', 'Total')
colnames(tmp) <- tmp.cols
tmp$Type <- 'Contingent Troops'
gender.mission <- rbind(gender.mission,tmp)
gender.tcc <- rbind(gender.mission,tmp)
gender.summary <- rbind(gender.summary,tmp)

#pull out total data and create new data frame
tmp <- gender.full[,c(1:2,4,17:10)]
tmp.cols <- c('Date', 'Contributor', 'Mission', 'Male', 'Female', 'Total')
colnames(tmp) <- tmp.cols
gender.mission.total <- tmp
gender.tcc.total <- tmp
gender.summary.total <- tmp

###mission aggregation
gender.mission <- ddply(gender.mission, .(Date,Mission,Type), summarise,
                             male.sum=sum(Male,na.rm=TRUE),
                             male.mean=mean(Male,na.rm=TRUE),
                             male.med=median(Male,na.rm=TRUE),
                             male.sd=sd(Male,na.rm=TRUE),
                             female.sum=sum(Female,na.rm=TRUE),
                             female.mean=mean(Female,na.rm=TRUE),
                             female.med=median(Female,na.rm=TRUE),
                             female.sd=sd(Female,na.rm=TRUE),
                             total.sum=sum(Total,na.rm=TRUE),
                             total.mean=mean(Total,na.rm=TRUE),
                             total.med=median(Total,na.rm=TRUE),
                             total.sd=sd(Total,na.rm=TRUE))
gender.mission.total$Total <- as.numeric(as.character(gender.mission.total$Total))
gender.mission.total <- ddply(gender.mission.total, .(Date,Mission), summarise,
                             male.sum=sum(Male,na.rm=TRUE),
                             male.mean=mean(Male,na.rm=TRUE),
                             male.med=median(Male,na.rm=TRUE),
                             male.sd=sd(Male,na.rm=TRUE),
                             female.sum=sum(Female,na.rm=TRUE),
                             female.mean=mean(Female,na.rm=TRUE),
                             female.med=median(Female,na.rm=TRUE),
                             female.sd=sd(Female,na.rm=TRUE),
                             total.sum=sum(Total,na.rm=TRUE),
                             total.mean=mean(Total,na.rm=TRUE),
                             total.med=median(Total,na.rm=TRUE),
                             total.sd=sd(Total,na.rm=TRUE))
tmp.cols <- c('Date','Mission','Type','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(gender.mission) <- tmp.cols
tmp.cols <- c('Date','Mission','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(gender.mission.total) <- tmp.cols
gender.mission$female.perc <- gender.mission$Female / gender.mission$Total
gender.mission.total$female.perc <- gender.mission.total$Female / gender.mission.total$Total

###tcc aggregation
gender.tcc <- ddply(gender.tcc, .(Date,Contributor,Type), summarise,
                             male.sum=sum(Male,na.rm=TRUE),
                             male.mean=mean(Male,na.rm=TRUE),
                             male.med=median(Male,na.rm=TRUE),
                             male.sd=sd(Male,na.rm=TRUE),
                             female.sum=sum(Female,na.rm=TRUE),
                             female.mean=mean(Female,na.rm=TRUE),
                             female.med=median(Female,na.rm=TRUE),
                             female.sd=sd(Female,na.rm=TRUE),
                             total.sum=sum(Total,na.rm=TRUE),
                             total.mean=mean(Total,na.rm=TRUE),
                             total.med=median(Total,na.rm=TRUE),
                             total.sd=sd(Total,na.rm=TRUE))
gender.tcc.total$Total <- as.numeric(as.character(gender.tcc.total$Total))
gender.tcc.total <- ddply(gender.tcc.total, .(Date,Contributor), summarise,
                                   male.sum=sum(Male,na.rm=TRUE),
                                   male.mean=mean(Male,na.rm=TRUE),
                                   male.med=median(Male,na.rm=TRUE),
                                   male.sd=sd(Male,na.rm=TRUE),
                                   female.sum=sum(Female,na.rm=TRUE),
                                   female.mean=mean(Female,na.rm=TRUE),
                                   female.med=median(Female,na.rm=TRUE),
                                   female.sd=sd(Female,na.rm=TRUE),
                                   total.sum=sum(Total,na.rm=TRUE),
                                   total.mean=mean(Total,na.rm=TRUE),
                                   total.med=median(Total,na.rm=TRUE),
                                   total.sd=sd(Total,na.rm=TRUE))
tmp.cols <- c('Date','Contributor','Type','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(gender.tcc) <- tmp.cols
tmp.cols <- c('Date','Contributor','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(gender.tcc.total) <- tmp.cols
gender.tcc$female.perc <- gender.tcc$Female / gender.tcc$Total
gender.tcc.total$female.perc <- gender.tcc.total$Female / gender.tcc.total$Total

###summary aggregation
gender.summary <- ddply(gender.summary, .(Date,Type), summarise,
                             male.sum=sum(Male,na.rm=TRUE),
                             male.mean=mean(Male,na.rm=TRUE),
                             male.med=median(Male,na.rm=TRUE),
                             male.sd=sd(Male,na.rm=TRUE),
                             female.sum=sum(Female,na.rm=TRUE),
                             female.mean=mean(Female,na.rm=TRUE),
                             female.med=median(Female,na.rm=TRUE),
                             female.sd=sd(Female,na.rm=TRUE),
                             total.sum=sum(Total,na.rm=TRUE),
                             total.mean=mean(Total,na.rm=TRUE),
                             total.med=median(Total,na.rm=TRUE),
                             total.sd=sd(Total,na.rm=TRUE))
gender.summary.total$Total <- as.numeric(as.character(gender.summary.total$Total))
gender.summary.total <- ddply(gender.mission.total, .(Date,Mission), summarise,
                                   male.sum=sum(Male,na.rm=TRUE),
                                   male.mean=mean(Male,na.rm=TRUE),
                                   male.med=median(Male,na.rm=TRUE),
                                   male.sd=sd(Male,na.rm=TRUE),
                                   female.sum=sum(Female,na.rm=TRUE),
                                   female.mean=mean(Female,na.rm=TRUE),
                                   female.med=median(Female,na.rm=TRUE),
                                   female.sd=sd(Female,na.rm=TRUE),
                                   total.sum=sum(Total,na.rm=TRUE),
                                   total.mean=mean(Total,na.rm=TRUE),
                                   total.med=median(Total,na.rm=TRUE),
                                   total.sd=sd(Total,na.rm=TRUE))
tmp.cols <- c('Date','Type','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(gender.summary) <- tmp.cols
tmp.cols <- c('Date','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(gender.summary.total) <- tmp.cols
gender.summary$female.perc <- gender.summary$Female / gender.summary$Total
gender.summary.total$female.perc <- gender.summary.total$Female / gender.summary.total$Total

############PLOTS?



rm(gender.mission)
rm(gender.tcc)
rm(gender.summary)
rm(gender.mission.total)
rm(gender.tcc.total)
rm(gender.summary.total)
rm(tmp)
rm(tmp.cols)

###################################
##FINAL OUTFILES
###################################
#write out full csv with correct headings
data.full.cols <- c('Date','Contributor','Contributor ISO-3','Contributor Capital','Contributor Capital Latitude','Contributor Capital Longitude','Contributor Continent','Contributor Region','Contributor UN Bloc','Contributor - P5, G4 or A3','Contributor - NAM','Contributor - G77','Contributor - AU','Contributor - Arab League','Contributor - OIC','Contributor - CIS','Contributor - G20','Contributor - EU','Contributor - NATO','Contributor - G8','Contributor - OECD','Contributor - ASEAN','Contributor - OAS','Contributor - Shanghai Coopertion Organization','Contributor - GCC','Contributor - UMA','Contributor - COMESA','Contributor - CENSAD','Contributor - EAC','Contributor - ECCAS','Contributor - ECOWAS','Contributor - IGAD','Contributor - SADC','Mission','Mission Country','Mission Country ISO-3','Mission HQ','Mission HQ Longitude','Mission HQ Latitude','Mission Continent','Mission Region','Mission UN Bloc','Mission - P5, G4, or A3','Mission - NAM','Mission - G77','Mission - AU','Mission - Arab League','Mission - OIC','Mission - CIS','Mission - G20','Mission - EU','Mission - NATO','Mission - G8','Mission - OECD','Mission - ASEAN','Mission - OAS','Mission - Shanghai Cooperation Organization','Mission - GCC','Mission - UMA','Mission - COMESA','Mission - CENSAD','Mission - EAC','Mission - ECCAS','Mission - ECOWAS','Mission - IGAD','Mission - SADC','Experts on Mission','Military Observers','Civilian Police','Formed Police Units','Inidividual Police','Troops','Police','Observers','Total','Distance')
colnames(data.full) <- data.full.cols
write.csv(data.full,'tcc_files/Data.Full.csv',row.names=FALSE)
rm(data.full)
rm(data.full.cols)

#write out gender csv with correct headings
gender.cols <- c('Date','Contributor','Contributor ISO-3','Mission','Individual Police Male','Individual Police Female','Individual Police Total','Formed Police Units Male','Formed Police Units Female','Formed Police Units Total','Experts on Mission Male','Experts on Mission Female','Experts on Mission Total','Troops Male','Troops Female','Troops Total','Total Male','Total Female','Total Total')
colnames(gender.full) <- gender.cols
write.csv(gender.full,'tcc_files/Gender.Full.csv',row.names=FALSE)
rm(gender.full)
rm(gender.cols)

rm(multiplot)