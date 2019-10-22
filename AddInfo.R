#Ancillary info!

info.raw<-read.csv('PlotInfo_2019.csv')
count.raw<-read.csv('StandCount_2019.csv')

refl<-cbind(ref.ts[,1:4], refl.merge)

explots<-which(refl$row!=(-3) & refl$row < 161 & refl$range!=1 & refl$range!=26)

light.raw<-refl[explots,]

#Remove edge plots; conveniently, outside data does not include them
light<-light.raw #[which(light.raw$row %in% unique(count.raw$first_row)& light.raw$range %in% unique(count.raw$range)), ]


lightinfo<-merge(light, info.raw, by.x=c('row', 'range'),by.y=c('first_row','range'))
lightcountinfo<-merge(lightinfo, count.raw,by.x=c('row', 'range'),by.y=c('first_row','range'))

dat.re<-lightcountinfo[,c(1:4, 766:775, 5:765)]; dat.ts<-lightcountinfo[,1:4] #REadable dataframe, ancil info first

library(stringr)

dat.sr<-dat.re[15:ncol(dat.re)]

checkind<-str_detect(dat.re$set_id, "CHK")
dat.check<-dat.re[str_detect(dat.re$set_id, "CHK"),]

#CLEANUP
rm('light','lightcountinfo','lightinfo', 'refl', 'info.raw','light.raw')
