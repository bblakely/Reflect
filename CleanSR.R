#Early analysis and correction
source("Readin.R")
source("AddInfo.R")



####Very coarse broadbanding - just averages!####

vis.400.700<-rowMeans(dat.sr[sr.wave%in%c(400:700)])
nir.700.1000<-rowMeans(dat.sr[sr.wave%in%c(700:1000)])



par(mfrow=c(1,2))
hist(vis.400.700, xlab="unweighted albedo", main="VIS reflectance (400-700nm)" );hist(nir.700.1000, xlab='unweighted albedo', main="NIR reflectance (700-1000nm)")


# #Convert to broadband; commented out####
# library("DescTools") #Calculus boo
# vis<-AUC(x=c(1:301),y=avgsolar.vis)
# 
# prop<-rep(0,300)
# for(i in 1:301){
#   prop[i]<-AUC(x=c(1:2),y=c(avgsolar.vis[i],avgsolar.vis[i+1]))
#   
# }
# 
# num<-sample(1:1075,1)
# refl.ineg<-sum(unlist(refl.merge[num,which(sr.wave%in%c(400:700))])*weights, na.rm=TRUE)
# refl.ineg
# #Next step is to wieght whole thing; this doesn't seem to make a huge differece, though!
#####

#Get the checklines
refl.exp<-dat.sr#Excludes border rows
refl.chk<-dat.check[,15:ncol(dat.check)] #Checkline plots

sw.exp<-vis.400.700;sw.chk<-vis.400.700[checkind]

refl.cat<-as.character(dat.check$set_id)


for(i in 1:length(unique(refl.cat))){
  
  nums<-sw.chk[which(refl.cat==unique(refl.cat)[i])]
  ran<-sw.exp[sample(1:length(sw.exp),16)]
  
  hist(nums, main=paste("SW ", unique(refl.cat[i])), xlim=c(0.01, 0.1), ylim=c(0,10),breaks=seq(from=min(nums), to=max(nums), length.out = 10))
  
  hist(ran, main='random', xlim=c(0.01, 0.1), ylim=c(0,10), breaks=seq(from=min(ran), to=max(ran), length.out = 10))
  
  print(paste(refl.cat[i],(t.test(nums, ran)[3])))
  
}


par(mfrow=c(1,2))

lw.exp<-nir.700.1000;lw.chk<-nir.700.1000[checkind]

refl.cat<-as.character(dat.check$set_id)

for(i in 1:length(unique(refl.cat))){
  
  nums<-lw.chk[which(refl.cat==unique(refl.cat)[i])]
  ran<-lw.exp[sample(1:length(lw.exp),16)]
  
  hist(nums, main=paste("LW ",unique(refl.cat[i])), xlim=c(0.2, 0.7), ylim=c(0,10), breaks=seq(from=min(nums), to=max(nums), length.out=10))
  hist(ran, main='random', xlim=c(0.2, 0.7), ylim=c(0,10), breaks=seq(from=min(ran), to=max(ran), length.out=10))
  
  print(paste(refl.cat[i],(t.test(nums, ran)[3])))
}

####Trying to narrow checkline variability####

#Correct for downward drift

times<-dat.re$dectime
plot(vis.400.700~times)

tr<-lm(vis.400.700~times)
coef(tr)

corrections<-(dat.re$dectime-min(dat.re$dectime, na.rm=TRUE))*coef(tr)[2]

cvis.400.700<-vis.400.700-corrections
plot(cvis.400.700~times)

#This matters


#Recheck: Same code as above sub in corrected values####
sw.exp<-cvis.400.700;sw.chk<-cvis.400.700[checkind]

refl.cat<-as.character(dat.check$set_id)


for(i in 1:length(unique(refl.cat))){
  par(mfrow=c(1,2))
  nums<-sw.chk[which(refl.cat==unique(refl.cat)[i])]
  ran<-sw.exp[sample(1:length(sw.exp),16)]
  
  hist(nums, main=paste("SW ", unique(refl.cat)[i]), xlim=c(0.01, 0.1), ylim=c(0,10),breaks=seq(from=min(nums), to=max(nums), length.out = 10))
  
  hist(ran, main='random', xlim=c(0.01, 0.1), ylim=c(0,10), breaks=seq(from=min(ran), to=max(ran), length.out = 10))
  
  
  print(paste('mean:', unique(refl.cat)[i],(t.test(nums, ran, var.equal = FALSE)[3])))
  print(paste('var:',unique(refl.cat)[i],(var.test(nums, ran, alternative = 'less')[3])))
  

  }

#####


#North and south?

s.ind<-which(dat.re$range %% 2==0) #Don't actually know that this is south, but all odds will be in the same direction (whatever it may be)
n.ind<-which(dat.re$range %% 2==1)

plot(cvis.400.700[s.ind]~times[s.ind]); points(cvis.400.700[n.ind]~times[n.ind], col='red')

#This does not matter

#Flood zone?
floodlist<-read.csv("FloodPlots_2019.csv")

dat.re$order<-c(1:nrow(dat.re))

dat.flood<-merge(x=dat.re, y=floodlist, by.x=c('row', 'range'), by.y=c('RowStart', 'Range'), all.x=TRUE)
dat.flood<-dat.flood[order(dat.flood$order),c(1:14,775:778, 15:774)] #reorder to match dat.re and have metadata at the front where I can see it

plot(cvis.400.700~dat.re$dectime);points(cvis.400.700[which(!is.na(dat.flood$Edge.))]~dat.re$dectime[which(!is.na(dat.flood$Edge.))], col='blue', pch=2)
t.test(cvis.400.700[which(!is.na(dat.flood$Edge.))], cvis.400.700[sample(1:length(cvis.400.700), 84)], alternative='greater')

dat.re$flood<-0;dat.re$flood[which(!is.na(dat.flood$Edge.))]<-1
dat.re<-dat.re[,c(1:14,776:777,15:775)]

#This matters



#Recheck
sw.exp<-cvis.400.700;sw.chk<-cvis.400.700[checkind]
chk.meta<-dat.re[checkind,1:16]

refl.cat<-as.character(dat.check$set_id)

par(mfrow=c(1,2))

for(i in 1:length(unique(refl.cat))){
  
  nums<-sw.chk[which(refl.cat==unique(refl.cat)[i]& chk.meta$flood==0)]
  ran<-sw.exp[sample(which(dat.re$flood==0), length(nums))]

  hist(nums, main=paste("SW ", unique(refl.cat)[i]), xlab="VIS albedo", xlim=c(0.01, 0.1), ylim=c(0,10),breaks=seq(from=min(nums), to=max(nums), length.out = 10))
  
  hist(ran, main='random',xlab="VIS albedo", xlim=c(0.01, 0.1), ylim=c(0,10), breaks=seq(from=min(ran), to=max(ran), length.out = 10))
  
  print(paste('mean:', unique(refl.cat)[i],(t.test(nums, ran, var.equal = FALSE)[3])))
  print(paste('var:',unique(refl.cat)[i],(var.test(nums, ran, alternative = 'less')[3])))

}



#White references? No visible evidence of this; don't quite know where they are anyway. Should record next time.

#Row? Range? 

for(i in 1:length(unique(refl.cat))){
plot(cvis.400.700~dat.re$row, main=unique(refl.cat)[i]);points(cvis.400.700[dat.re$set_id==unique(refl.cat)[i]]~dat.re$row[dat.re$set_id==unique(refl.cat)[i]], col='red', pch="*", cex=2)
plot(cvis.400.700~dat.re$range);points(cvis.400.700[dat.re$set_id==unique(refl.cat)[i]]~dat.re$range[dat.re$set_id==unique(refl.cat)[i]], col='red', pch="*", cex=2)
}
#nope not that either

#Row density
plot(cvis.400.700~dat.re$row_density)
# Yeah no relationship there


#curious whether row density checks out

#Check Row Density####
rd.exp<-dat.re$row_density;rd.chk<-dat.re$row_density[checkind]
chk.meta<-dat.re[checkind,1:16]

refl.cat<-as.character(dat.check$set_id)

par(mfrow=c(1,2))

for(i in 1:length(unique(refl.cat))){
  
  nums<-rd.chk[which(refl.cat==unique(refl.cat)[i]& chk.meta$flood==0)]
  ran<-rd.exp[sample(which(dat.re$flood==0), length(nums))]
  
  hist(nums, main=paste("RD ", unique(refl.cat)[i]), xlim=c(5, 20), ylim=c(0,10),breaks=seq(from=min(nums), to=max(nums), length.out = 10))
  
  hist(ran, main='random', xlim=c(5, 20), ylim=c(0,10), breaks=seq(from=min(ran), to=max(ran), length.out = 10))
  
  print(paste('mean:', unique(refl.cat)[i],(t.test(nums, ran, var.equal = FALSE)[3])))
  print(paste('var:',unique(refl.cat)[i],(var.test(nums, ran, alternative = 'less')[3])))
  
}

#Well I have better results than row density...
#####

