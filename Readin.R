#Choose your sensors. Probably should functionalize this at some point.

sensors<-data.frame(cbind(c('downward', 'upward', 'downward', 'upward'), c('short', 'short', 'long','long')))

for(q in 1:4){

ud<-sensors[q,1]
lam<-sensors[q,2]

if(ud=="downward"&lam=="short"){filename<-'SR_08_09_data/SR_08_09_1153_relative.csv';nwave<-481}else
  if(ud=="upward"&lam=="short"){filename<-'SR_08_09_data/SR_08_09_1152_relative.csv';nwave<-481}else
    if(ud=="downward"& lam=="long"){filename<-'SR_08_09_data/SR_08_09_1025_relative.csv';nwave<-466}else
      if(ud=="upward"& lam=="long"){filename<-'SR_08_09_data/SR_08_09_1024_relative.csv';nwave<-466}else (print("Did not correctly specify sensor"))

print('##########################')
print(paste("Processing", sensors[q,1],sensors[q,2]))
print('##########################')

sr.raw<-read.csv(filename, skip=2, header=FALSE, nrows=nwave, colClasses = 'numeric')
sr.ts<-(read.csv(filename, header=FALSE, nrows=1))


###Deal with Apogee's dumb file format####
#de-factorize timestamp
i<-sapply(sr.ts,is.factor)
sr.ts[i]<-lapply(sr.ts[i], as.character)
sr.ts<-substr(sr.ts,1,19); sr.ts<-sr.ts[2:length(sr.ts)]
H<-as.numeric(substr(sr.ts,1,2)); M<-as.numeric(substr(sr.ts,4,5)); S<-as.numeric(substr(sr.ts,7,8))
dectime<-H+(M/60)+(S/3600)
TS<-data.frame(cbind(dectime,H, M, S))

#de-factorize and transpose data
sr.wave<-sr.raw[,1]
sr.num<-(t(sr.raw[2:ncol(sr.raw)]));colnames(sr.num)<-paste('b',sr.wave, sep="")

# 
# par(mfrow=c(3,3)) #quick check for reasonable spectra
# for(i in sample(1:nrow(sr.num), 18)){
#   plot(sr.num[i,], type='l')
# }
#####

###Retrieve timestamps from plot splitting files###
source('Rtimes.R')
ts.split<-splitzip("SR_08_09_timestamp")

###Put everything together###
sr.dat<-as.matrix(cbind(TS, sr.num)); rownames(sr.dat)<-c();sr.pure<-sr.dat[,5:ncol(sr.dat)]
combine<-function(dat, ts, split, na.timecol="TIMESTAMP"){ #dat: stripped down data W/O timestamp info. ts: processed timestamp df. split: plot splitting df, formatted by breaktime+splitzip. na.timecol: column to use to indicate times of no data collection
  
  bigdat<-cbind(ts, dat)
  plot.combine<-merge(split, bigdat, by='dectime', all=TRUE)
  
  na.timeind<-which(colnames(plot.combine)==na.timecol)
  both.str<-max(min(which(!is.na(plot.combine$row))), min(which(!is.na(plot.combine[na.timeind]))))
  both.end<-min(max(which(!is.na(plot.combine$row))), max(which(!is.na(plot.combine[na.timeind]))))
  
  plot.cl<-plot.combine[both.str:both.end,]
  
  
  return(plot.cl)
  
}
plot.cl.sr<-combine(dat=sr.pure, ts=(TS), split=(ts.split), na.timecol="S")
#bit of cleanup
rm('sr.pure','sr.num','TS','ts.split', 'H','M','S', 'sr.ts')

###Actual plot splitting###
plotsplit=function(dat, numcol=c(1:3,5:8,11:18), out='mean', noise.tol=200, n.split=4){
  
  uniquerows<-unique(dat$row)[!is.na(unique(dat$row))]
  uniqueranges<-unique(dat$range)[!is.na(unique(dat$range))]
  
  #bigdf<-data.frame()
  meandf<-data.frame(matrix(nrow=length(uniquerows)*length(uniqueranges), ncol=length(numcol)+3)); colnames(meandf)<-c('row', 'range','count',colnames(dat[numcol]))#, 'noisy')
  
  count<-0
  
  for (o in 1:length(uniquerows)){
    for(a in 1:length(uniqueranges)){
      
      count<-count+1
      loc<-which(dat$row==uniquerows[o] & dat$range==uniqueranges[a])
      
      if(length(loc)==n.split){
        
        ind<-c(min(loc):max(loc))
        datsub<-dat[ind,]
        
        datcount<-length(which(!is.na(rowMeans(datsub[,numcol]))))
        datlab<-c(uniquerows[o],uniqueranges[a], datcount);names(datlab)<-c('row', 'range', 'reccount')
        
        
        #if(out!='mean'){bigdf<-rbind(bigdf, datsub)}
        
        meandf[count,]<-c(datlab, colMeans(datsub[,numcol], na.rm=TRUE))#, noiseflag)
        
        
        
      }
      else{
        
        if(length(loc)==8){print(paste("row", uniquerows[o], "range", uniqueranges[a], "is doubled"))}
        else if(length(loc)==0){print(paste("row",uniquerows[o], "range",  uniqueranges[a], "is missing"))}
        else{print(paste("row",uniquerows[o], "range",  uniqueranges[a], "is irregular;","There are", length(which(dat$row==uniquerows[o] & dat$range==uniqueranges[a])), "records with this label"))}
        
      }
    }
  }
  
  if(out=='mean'){return(meandf)}else{return(bigdf)}
  
}
plotmeans.sr<-plotsplit(dat=plot.cl.sr, numcol=c(1,8:(nwave+7)), n.split=2) #plotfull<-plotsplit(plot.cl, out='all')





###Plot result###
par(mfrow=c(1,1))
band<-c(5:(nwave+4))
plot(unlist(plotmeans.sr[500,band])~sr.wave, ylim=c(-100,5000), type='l', col='white')
for(i in sample(1:(nrow(plotmeans.sr)), 200)){
  lines(unlist(plotmeans.sr[i,band])~sr.wave, col=sample(1:8, 1))
}

if(ud=="upward"&lam=="short"){plotmeans.sr->ul.sw.mean;sr.wave.sw<-sr.wave} ; if(ud=="downward"&lam=="short"){plotmeans.sr->dl.sw.mean;sr.wave.sw<-sr.wave}
if(ud=="upward"&lam=="long"){plotmeans.sr->ul.lw.mean;sr.wave.lw<-sr.wave} ; if(ud=="downward"&lam=="long"){plotmeans.sr->dl.lw.mean;sr.wave.lw<-sr.wave}



#Reflectance calculations for shotwave
if(exists('ul.sw.mean')&exists('dl.sw.mean')&lam=="short"){

dl.sw.dat<-dl.sw.mean[,5:(nwave+4)]; ul.sw.dat<-ul.sw.mean[,5:(nwave+4)]
refl.sw<-dl.sw.dat/ul.sw.dat

colgr<-colorRampPalette(c('cyan','antiquewhite', 'orange')); colvec<-colgr(30)[as.numeric(cut(dl.sw.mean$dectime, breaks=30))]
#par(mfrow=c(1,1))
plot(unlist(refl.sw[1,])~sr.wave.sw, col='white', ylim=c(-0.4, 0.9))
for(i in sort(sample(1:nrow(refl.sw), 50))){
  exists<-!is.na(mean(unlist(refl.sw[i,]),na.rm=TRUE))
  if(exists==TRUE){lines(unlist(refl.sw[i,])~sr.wave.sw, col=colvec[i])}else(print(paste("row ", dl.sw.mean$row[i],',', "range ",dl.sw.mean$range[i], " does not have data", sep='')))
}

}

#Reflectance calculations for longwave
if(exists('ul.lw.mean')&exists('dl.lw.mean')&lam=="long"){
  
  dl.lw.dat<-dl.lw.mean[,5:(nwave+4)]; ul.lw.dat<-ul.lw.mean[,5:(nwave+4)]
  refl.lw<-dl.lw.dat/ul.lw.dat
  
  colgr<-colorRampPalette(c('cyan','antiquewhite', 'orange')); colvec<-colgr(30)[as.numeric(cut(dl.lw.mean$dectime, breaks=30))]
  #par(mfrow=c(1,1))
  plot(unlist(refl.lw[1,])~sr.wave.lw, col='white', ylim=c(-0.4, 0.9))
  for(i in sort(sample(1:nrow(refl.lw), 50))){
    exists<-!is.na(mean(unlist(refl.lw[i,]),na.rm=TRUE))
    if(exists==TRUE){lines(unlist(refl.lw[i,])~sr.wave.lw, col=colvec[i])}else(print(paste("row ", dl.lw.mean$row[i],',', "range ",dl.lw.mean$range[i], " does not have data", sep='')))
  }
  
}
}

#Combine the spectra
