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
########################################################

library(reshape2)
library(ggplot2)
library(plyr)
library(scales)
library(RColorBrewer)
library(geosphere)
library(reldist)

backup.data.full <- data.full
backup.data.gender <- data.gender

#import data exported by sql query as temp add files. must be in the 'Documents' folder and titled 'contributsions.csv' and 'gender.csv' respectively
temp.data.full <- read.csv('~/Documents/contributions.csv')
temp.data.gender <- read.csv('~/Documents/gender.csv')

###################################
##PROCESS IMPORTED DATA
###################################
#calculate distance between capital and mission hq 
temp.data.full$distance <- distHaversine(p1=temp.data.full[,5:6],p2=temp.data.full[,23:24])

#Change col names to correct format
data.full.col.names <- c('date','tcc','tcc.iso3.alpha','tcc.cap','tcc.cap.long','tcc.cap.lat','tcc.continent','tcc.unregion','tcc.unbloc','tcc.p5g4a3','tcc.nam','tcc.g77','tcc.au','tcc.arab.league','tcc.oic','tcc.cis','tcc.g20','tcc.eu','mission','mission.country','mission.iso3.alpha','mission.hq','mission.hq.long','mission.hq.lat','mission.continent','mission.un.region','mission.un.block','mission.p5g4a3','mission.nam','mission.g77','mission.au','mission.arab.league','mission.oic','mission.cis','mission.g20','mission.eu','eom','milobvs','civpol','fpu','ip','troops','police','observers','total','distance')
data.gender.col.names <- c('date','date.string','tcc','tcc.iso3.alpha','mission','ip.M','ip.F','ip.T','fpu.M','fpu.F','fpu.T','eom.M','eom.F','eom.T','troops.M','troops.F','troops.T','total.M','total.F','total.T')
colnames(temp.data.full) <- data.full.col.names
colnames(temp.data.gender) <- data.gender.col.names
  
#format date column as date data type
temp.data.full$date <- as.Date(temp.data.full$date)
temp.data.gender$date <- as.Date(temp.data.gender$date)

#set dummy variables as factors
temp.data.full$tcc.nam <- as.factor(temp.data.full$tcc.nam)
temp.data.full$tcc.g77 <- as.factor(temp.data.full$tcc.g77)
temp.data.full$tcc.au <- as.factor(temp.data.full$tcc.au)
temp.data.full$tcc.arab.league <- as.factor(temp.data.full$tcc.arab.league)
temp.data.full$tcc.oic <- as.factor(temp.data.full$tcc.oic)
temp.data.full$tcc.cis <- as.factor(temp.data.full$tcc.cis)
temp.data.full$tcc.g20 <- as.factor(temp.data.full$tcc.g20)
temp.data.full$tcc.eu <- as.factor(temp.data.full$tcc.eu)
temp.data.full$mission.nam <- as.factor(temp.data.full$mission.nam)
temp.data.full$mission.g77 <- as.factor(temp.data.full$mission.g77)
temp.data.full$mission.au <- as.factor(temp.data.full$mission.au)
temp.data.full$mission.arab.league <- as.factor(temp.data.full$mission.arab.league)
temp.data.full$mission.oic <- as.factor(temp.data.full$mission.oic)
temp.data.full$mission.cis <- as.factor(temp.data.full$mission.cis)
temp.data.full$mission.g20 <- as.factor(temp.data.full$mission.g20)
temp.data.full$mission.eu <- as.factor(temp.data.full$mission.eu)

#bind new data to old data
data.full <- rbind(data,full, temp.data.full)
data.gender <- rbind(data.full, temp.data.gender)

###############################
##Do some analysis on new month contributions here:
##who are the biggest contributors, where are the 
##most troops deployed, average contribution...
###############################

#drop temp data
rm(temp.data.full)
rm(temp.data.gender)

write.csv(data.full,'~/Documents/tcc_files/Data.Full.csv',row.names=FALSE)
###################################
##REGIONAL AGGREGATION
###################################
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
write.csv(data.full.region.tcc.africa,'~/Documents/tcc_files/regions/africa.csv',row.names=FALSE)
write.csv(data.full.region.tcc.americas,'~/Documents/tcc_files/regions/americas.csv',row.names=FALSE)
write.csv(data.full.region.tcc.asia,'~/Documents/tcc_files/regions/asia.csv',row.names=FALSE)
write.csv(data.full.region.tcc.europe,'~/Documents/tcc_files/regions/europe.csv',row.names=FALSE)
write.csv(data.full.region.tcc.oceania,'~/Documents/tcc_files/regions/oceania.csv',row.names=FALSE)
ggsave(plot.region.africa,file='~/Documents/tcc_files/regions/africa.png',height=8.5,width=11)
ggsave(plot.region.americas,file='~/Documents/tcc_files/regions/americas.png',height=8.5,width=11)
ggsave(plot.region.asia,file='~/Documents/tcc_files/regions/asia.png',height=8.5,width=11)
ggsave(plot.region.europe,file='~/Documents/tcc_files/regions/europe.png',height=8.5,width=11)
ggsave(plot.region.oceania,file='~/Documents/tcc_files/regions/oceania.png',height=8.5,width=11)

rm(reds)
rm(oranges)
rm(greens)
rm(blues)
rm(purples)
rm(plot.region.africa)
rm(plot.region.americas)
rm(plot.region.asia)
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
plot.continent.police <- plot.continent.police + labs(title = 'Police Contributions by Continent',x='Year',y='Total UN Peacekeeping Contributions',color='Continent')
plot.continent.police <- plot.continent.police + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.police <- plot.continent.police + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.police <- plot.continent.police + scale_color_brewer(palette='Paired')

#observer contributions
plot.continent.observers <- ggplot(data.full.continent, aes(date,observers))
plot.continent.observers <- plot.continent.observers + geom_line(aes(color=tcc.continent))
plot.continent.observers <- plot.continent.observers + labs(title = 'Observer Contributions by Continent',x='Year',y='Total UN Peacekeeping Contributions',color='Continent')
plot.continent.observers <- plot.continent.observers + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.observers <- plot.continent.observers + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.observers <- plot.continent.observers + scale_color_brewer(palette='Paired')

#troop contributions
plot.continent.troops <- ggplot(data.full.continent, aes(date,troops))
plot.continent.troops <- plot.continent.troops + geom_line(aes(color=tcc.continent))
plot.continent.troops <- plot.continent.troops + labs(title = 'Troop Contributions by Continent',x='Year',y='Total UN Peacekeeping Contributions',color='Continent')
plot.continent.troops <- plot.continent.troops + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.troops <- plot.continent.troops + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.troops <- plot.continent.troops + scale_color_brewer(palette='Paired')

#number of contributors
plot.continent.contributors <- ggplot(data.full.continent, aes(date,n.contributors))
plot.continent.contributors <- plot.continent.contributors + geom_line(aes(color=tcc.continent))
plot.continent.contributors <- plot.continent.contributors + labs(title = 'Contributors by Continent',x='Year',y='Number of Contributing Countries by Continent',color='Continent')
plot.continent.contributors <- plot.continent.contributors + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.continent.contributors <- plot.continent.contributors + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
plot.continent.contributors <- plot.continent.contributors + scale_color_brewer(palette='Paired')

#write subsets to continent dir 
write.csv(data.full.continent,'~/Documents/tcc_files/continent/continent.csv',row.names=FALSE)
ggsave(plot.continent.total,file='~/Documents/tcc_files/continent/total.png',height=8.5,width=11)
ggsave(plot.continent.police,file='~/Documents/tcc_files/continent/police.png',height=8.5,width=11)
ggsave(plot.continent.observers,file='~/Documents/tcc_files/continent/observer.png',height=8.5,width=11)
ggsave(plot.continent.troops,file='~/Documents/tcc_files/continent/troops.png',height=8.5,width=11)
ggsave(plot.continent.contributors,file='~/Documents/tcc_files/continent/contributors.png',height=8.5,width=11)

rm(plot.continent.total)
rm(plot.continent.police)
rm(plot.continent.observers)
rm(plot.continent.troops)
rm(plot.continent.contributors)

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

#Loop throuh country list to create and save individual csv's and plots
lapply(tcc.vector, function(c){
  tmp <- data.full.tcc[which(data.full.tcc$tcc == c),c(1,19,23,27)]
  tmp.cols <- c('Date','Troops','Police','Observers')
  colnames(tmp) <- tmp.cols
  write.csv(tmp,paste0('~/Documents/tcc_files/countries/',c,'.csv'),row.names=FALSE)
  tmp <- melt(tmp,id.vars='Date')
  tmp.cols <- c('Date','Type','Value')
  colnames(tmp) <- tmp.cols
  tmp.p <- ggplot(tmp, aes(Date,Value))
  tmp.p <- tmp.p + geom_line(aes(color=Type))
  tmp.p <- tmp.p + labs(title=paste('Peacekeeping Contributions of', c, sep=" "),x='Year',y='UN Peacekeeping Contributions', color='Type of contribution')
  tmp.p <- tmp.p + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
  tmp.p <- tmp.p + theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))
  tmp.p <- tmp.p + scale_color_brewer(palette='Paired')
  ggsave(tmp.p,file=paste0('~/Documents/tcc_files/countries/',c,'.png'),height=4,width=6)
})

write.csv(data.full.tcc,'~/Documents/tcc_files/Data.TCC.csv',row.names=FALSE)
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
write.csv(data.full.mission,'~/Documents/tcc_files/Data.Mission.csv',row.names=FALSE)
write.csv(data.full.mission.region,'~/Documents/tcc_files/mission/regional_deployments.csv',row.names=FALSE)
write.csv(data.full.mission.continent,'~/Documents/tcc_files/mission/continent_deployments.csv',row.names=FALSE)

ggsave(plot.region.deployments.africa,file='~/Documents/tcc_files/mission/african_regional_deployments.png',height=8.5,width=11)
ggsave(plot.region.deployments.asia,file='~/Documents/tcc_files/mission/asian_regional_deployments.png',height=8.5,width=11)
ggsave(plot.region.deployments.americas,file='~/Documents/tcc_files/mission/americas_regional_deployments.png',height=8.5,width=11)
ggsave(plot.region.deployments.europe,file='~/Documents/tcc_files/mission/european_regional_deployments.png',height=8.5,width=11)
ggsave(plot.continent.deployments.total,file='~/Documents/tcc_files/mission/total_deployments_by_continent.png',height=8.5,width=11)
ggsave(plot.continent.mission,file='~/Documents/tcc_files/mission/missions_by_continent.png',height=8.5,width=11)

rm(plot.region.deployments.africa)
rm(plot.region.deployments.asia)
rm(plot.region.deployments.americas)
rm(plot.region.deployments.europe)
rm(plot.continent.deployments.total)
rm(plot.continent.mission)

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
write.csv(data.summary.monthly.full,'~/Documents/tcc_files/Data.Monthly.csv',row.names=FALSE)
write.csv(data.summary.monthly.aspirant,'~/Documents/tcc_files/summaries/aspirants.csv',row.names=FALSE)
write.csv(data.summary.monthly.tcc,'~/Documents/tcc_files/summaries/distribution.csv',row.names=FALSE)
ggsave(plot.aspirants,file='~/Documents/tcc_files/summaries/aspirants.png',height=8.5,width=11)
ggsave(plot.monthly.totals,file='~/Documents/tcc_files/summaries/monthly_deployments.png',height=8.5,width=11)
ggsave(plot.troops.quintiles,file='~/Documents/tcc_files/summaries/troop_quintiles.png',height=8.5,width=11)
ggsave(plot.total.quintiles,file='~/Documents/tcc_files/summaries/total_quintiles.png',height=8.5,width=11)

rm(tmp)
rm(summary.tmp)
rm(colors)
rm(plot.monthly.totals)
rm(plot.monthly.mean.dist)
rm(plot.aspirants)
rm(plot.troops.quintiles)
rm(plot.total.quintiles)

###################################
##GENDER AGGREGATION
###################################
#write out gender csv with correct headings
tmp <- data.gender[,c(1,3:20)]
tmp.cols <- c('Date','Contributor','Contributor ISO-3','Mission','Individual Police Male','Individual Police Female','Individual Police Total','Formed Police Units Male','Formed Police Units Female','Formed Police Units Total','Experts on Mission Male','Experts on Mission Female','Experts on Mission Total','Troops Male','Troops Female','Troops Total','Total Male','Total Female','Total Total')
colnames(tmp) <- tmp.cols
write.csv(tmp,'~/Documents/tcc_files/Data.Gender.csv')

###manually melt data
#pull out individual police data
tmp <- data.gender[,c(1,3,5,6:8)]
tmp$Date <- tmp$date
tmp$Contributor <- tmp$tcc
tmp$Mission <- tmp$mission
tmp$Male <- tmp$ip.M
tmp$Female <- tmp$ip.F
tmp$Total <- tmp$ip.T
tmp$Type <- 'Individual Police'
data.gender.mission <- tmp[,7:13]
data.gender.tcc <- tmp[,7:13]
data.gender.summary <- tmp[,7:13]

#pull out formed police unit data
tmp <- data.gender[,c(1,3,5,9:11)]
tmp$Date <- tmp$date
tmp$Contributor <- tmp$tcc
tmp$Mission <- tmp$mission
tmp$Male <- tmp$fpu.M
tmp$Female <- tmp$fpu.F
tmp$Total <- tmp$fpu.T
tmp$Type <- 'Formed Police Units'
data.gender.mission <- rbind(data.gender.mission,tmp[,7:13])
data.gender.tcc <- rbind(data.gender.mission,tmp[,7:13])
data.gender.summary <- rbind(data.gender.summary,tmp[,7:13])

#pull out eom data
tmp <- data.gender[,c(1,3,5,12:14)]
tmp$Date <- tmp$date
tmp$Contributor <- tmp$tcc
tmp$Mission <- tmp$mission
tmp$Male <- tmp$eom.M
tmp$Female <- tmp$eom.F
tmp$Total <- tmp$eom.T
tmp$Type <- 'Experts on Mission'
data.gender.mission <- rbind(data.gender.mission,tmp[,7:13])
data.gender.tcc <- rbind(data.gender.mission,tmp[,7:13])
data.gender.summary <- rbind(data.gender.summary,tmp[,7:13])

#pull out troops data
tmp <- data.gender[,c(1,3,5,15:17)]
tmp$Date <- tmp$date
tmp$Contributor <- tmp$tcc
tmp$Mission <- tmp$mission
tmp$Male <- tmp$troops.M
tmp$Female <- tmp$troops.F
tmp$Total <- tmp$troops.T
tmp$Type <- 'Contingent Troops'
data.gender.mission <- rbind(data.gender.mission,tmp[,7:13])
data.gender.tcc <- rbind(data.gender.mission,tmp[,7:13])
data.gender.summary <- rbind(data.gender.summary,tmp[,7:13])

#pull out total data and create new data frame
tmp <- data.gender[,c(1,3,5,18:20)]
tmp$Date <- tmp$date
tmp$Contributor <- tmp$tcc
tmp$Mission <- tmp$mission
tmp$Male <- tmp$total.M
tmp$Female <- tmp$total.F
tmp$Total <- tmp$total.T
data.gender.mission.total <- tmp[,7:12]
data.gender.tcc.total <- tmp[,7:12]
data.gender.summary.total <- tmp[,7:12]

###mission aggregation
data.gender.mission <- ddply(data.gender.mission, .(Date,Mission,Type), summarise,
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
data.gender.mission.total <- ddply(data.gender.mission.total, .(Date,Mission), summarise,
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
colnames(data.gender.mission) <- tmp.cols
tmp.cols <- c('Date','Mission','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(data.gender.mission.total) <- tmp.cols
data.gender.mission$female.perc <- data.gender.mission$Female / data.gender.mission$Total
data.gender.mission.total$female.perc <- data.gender.mission.total$Female / data.gender.mission.total$Total

###tcc aggregation
data.gender.tcc <- ddply(data.gender.tcc, .(Date,Contributor,Type), summarise,
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
data.gender.tcc.total <- ddply(data.gender.tcc.total, .(Date,Contributor), summarise,
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
colnames(data.gender.tcc) <- tmp.cols
tmp.cols <- c('Date','Contributor','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(data.gender.tcc.total) <- tmp.cols
data.gender.tcc$female.perc <- data.gender.tcc$Female / data.gender.tcc$Total
data.gender.tcc.total$female.perc <- data.gender.tcc.total$Female / data.gender.tcc.total$Total

###summary aggregation
data.gender.summary <- ddply(data.gender.summary, .(Date,Type), summarise,
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
data.gender.summary.total <- ddply(data.gender.mission.total, .(Date,Mission), summarise,
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
colnames(data.gender.summary) <- tmp.cols
tmp.cols <- c('Date','Male','Male.mean','Male.med','Male.sd','Female','Female.mean','Female.med','Female.sd','Total','Total.mean','Total.med','Total.sd')
colnames(data.gender.summary.total) <- tmp.cols
data.gender.summary$female.perc <- data.gender.summary$Female / data.gender.summary$Total
data.gender.summary.total$female.perc <- data.gender.summary.total$Female / data.gender.summary.total$Total

plot.gender.totals <- ggplot(data.gender.summary, aes(Date,Female))
plot.gender.totals <- plot.gender.totals + geom_line(aes(color=Type))
plot.gender.totals <- plot.gender.totals + labs(title='Female Contributions to UN Peacekeeping',x='Year',y='Total Female UN Peacekeeping Contributions', color='Type')
plot.gender.totals <- plot.gender.totals + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.gender.totals <- plot.gender.totals + theme(legend.position="bottom")
plot.gender.totals <- plot.gender.totals + scale_color_brewer(palette='Paired')

plot.gender.perc <- ggplot(data.gender.summary, aes(Date,female.perc))
plot.gender.perc <- plot.gender.perc + geom_line(aes(color=Type))
plot.gender.perc <- plot.gender.perc + labs(title='Female Contributions to UN Peacekeeping',x='Year',y='Percentage of Total Female UN Peacekeeping Contributions', color='Type')
plot.gender.perc <- plot.gender.perc + theme_bw() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))
plot.gender.perc <- plot.gender.perc + theme(legend.position="bottom")
plot.gender.perc <- plot.gender.perc + scale_color_brewer(palette='Paired')

#write csvs and plots to dir
write.csv(data.gender.mission,'~/Documents/tcc_files/gender/mission.csv')
write.csv(data.gender.mission.total,'~/Documents/tcc_files/gender/mission.total.csv')
write.csv(data.gender.tcc,'~/Documents/tcc_files/gender/tcc.csv')
write.csv(data.gender.tcc.total,'~/Documents/tcc_files/gender/tcc.total.csv')
ggsave(plot.gender.totals,file='~/Documents/tcc_files/gender/monthly_totals.png',height=8.5,width=11)
ggsave(plot.gender.perc,file='~/Documents/tcc_files/gender/monthly_percentages.png',height=8.5,width=11)

rm(tmp)
rm(tmp.cols)
rm(plot.gender.totals)
rm(plot.gender.perc)