first = function (x)x[1];
spread <- function(x){ length(table(as.character(x))); };
mostFrequent <- function(x){ names(sort(-table(as.character(x)))[1]); };

delta = read.table("delta1",sep=";",comment.char="", quote="", col.names=c("m","nm","f","v","mod0","mod","an","ae","at","ct","nadd","msg"),fileEncoding="latin1"); #nm is nmtr, mod is adjusted module (with the largest number of deltas at that month for that maintainer), mod0 has "other" module for trivial modules like include and lib
delta$an = tolower(delta$an);
#delta <- delta[-grep("=>",delta$f)]

mtr = read.table("mtr2",sep=";",comment.char="", quote="", col.names=c("f","mod0","mod","w","ct","nm","m"),fileEncoding="latin1");
mtr$mod1 <- sub("/.*", "",mtr$f,perl=T,useBytes=T);

dat <- read.table("mtrProd.R",sep=";",quote="",col.names=c("t","nm","nf","nw","m","mod","nauth","ncmt"))
d1 <- dat[dat$nm>0,]; 

delta$mod1 <- sub("/.*", "",delta$f,perl=T,useBytes=T); #mod1 is original module
delta$y <- floor(delta$ct);
delta$add <- sub(":.*$","",delta$nadd,perl=T,useBytes=T);
delta$del <- sub("^.*:","",delta$nadd,perl=T,useBytes=T);
tmin <- tapply(delta$ct, delta$an, min, na.rm=T);
delta$fr <- tmin[match(delta$an,names(tmin))];
delta$fr1 <- as.factor(delta$fr);
delta$fry <- as.factor(floor(delta$fr));
delta$mt <- paste(delta$m,delta$ct,sep=";");

mtr$m <- as.character(mtr$m);
mtr$mt <- paste(mtr$m,mtr$ct,sep=";");
delta$y1 <- as.factor(delta$y);
delta$t1 <- as.factor(delta$ct);

d1$mt <- paste(d1$m,d1$t,sep=";");

###########nf0/nauth0/acmt0 could only be calculated on pae
#tmp <- tapply(mtr$f,mtr$mt,spread);
#d1$nf0 <- tmp[match(d1$mt,names(tmp))];
#tmp <- tapply(delta$an,delta$mt,spread);
#d1$nauth0 <- tmp[match(d1$mt,names(tmp))];
#tmp <- tapply(delta$v,delta$mt,spread);
#d1$ncmt0 <- tmp[match(d1$mt,names(tmp))];

nf  <- tapply(d1$nf,d1$mod,mean,na.rm=T); 
nauth  <- tapply(d1$nauth,d1$mod,mean,na.rm=T);
ncmt  <- tapply(d1$ncmt,d1$mod,mean,na.rm=T);
nf0  <- tapply(d1$nf,d1$mod,median,na.rm=T); 
nauth0  <- tapply(d1$nauth,d1$mod,median,na.rm=T);
ncmt0  <- tapply(d1$ncmt,d1$mod,median,na.rm=T);
write(round(nf[mod],1),"workload",sep=" & ");
write(round(nf0[mod],1),"workload",append=TRUE,sep=" & ");
write(round(nauth[mod],1),"workload",append=TRUE,sep=" & ");
write(round(nauth0[mod],1),"workload",append=TRUE,sep=" & ");
write(round(ncmt[mod],1),"workload",append=TRUE,sep=" & ");
write(round(ncmt0[mod],1),"workload",append=TRUE,sep=" & ");
write(mod,"workload",append=TRUE,sep=" & ");
##########################

d1$y1 <- as.factor(floor(as.integer(as.character(d1$t))));
mod <- c("drivers","arch","fs","net","sound","kernel","mm");#crypto and lib don't have much commits (and touched files)
mtr$t1 <- as.factor(mtr$ct);

############ authors by time
authorMTR <- NA;
authoryMTR <- NA;
for (i in mod){
  authorMTR <- cbind(authorMTR,tapply(delta$an[delta$mod==i],delta$t1[delta$mod==i],spread));
  authoryMTR <- cbind(authoryMTR,tapply(delta$an[delta$mod==i],delta$y1[delta$mod==i],spread));
}
authorMTR <- authorMTR[,2:8];
authoryMTR <- authoryMTR[,2:8];
authorM <- authorMTR; authorY <- authoryMTR;
######## author churn
tmp <- NA; for (i in mod) { tmp1 <- tapply(mtr$m[mtr$mod==i],mtr$t1[mtr$mod==i],spread); tmp <- cbind(tmp,tmp1)};
tmp <- tmp[,2:8];

a2m = filter((authorM/tmp), c(1,2,3,4,5,4,3,2,1)/sum(c(1,2,3,4,5,4,3,2,1)));
postscript("Amonthly2Mmonthly.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("Amonthly2Mmonthly.png", width=800,height=600);
matplot(a2m,col=1:7,lwd=3,type="o",xaxt="n",main = "Ratio of (monthly #authors)/(monthly #maintainers)",xlab="Calendar month from April 2009",ylab="Number",ylim=c(0,4.65))
axis(1, at = c(1,13,25,37,49,61,73,85), labels =c(2009:2016));
legend(30,4.6,legend=mod,lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white");  
dev.off();

tmp0 <- NA;
for (i in 1:length(authorY[,1])) {
for (j in 1:12) {tmp0 <- rbind(tmp0,authorY[i,]);}
}
postscript("Ayearly2Mmonthly.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("Ayearly2Mmonthly.png", width=800,height=600);
matplot((tmp0[5:97,]/tmp),col=1:13,type="o",lwd=3,xaxt="n",main = "Ratio of (yearly #authors)/(monthly #maintainers)",xlab="Calendar month from April 2009",ylab="Number",ylim=c(0,25))
axis(1, at = c(1,13,25,37,49,61,73,85), labels =c(2009:2016));
legend(35,25,legend=mod,lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white");  
dev.off();

################# workload of a maintainer (on adjusted modules)

w = c(1.5,4,1.5,1.5,3,0.3,0.1);
tmp <- NA; for (i in mod) { tmp1 <- tapply(d1$nf[d1$mod==i],d1$y1[d1$mod==i],mean); tmp <- cbind(tmp,tmp1)}
tmp = tmp[,2:length(tmp[1,])];
postscript("nf8yrMEAN_mtrm2.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("nf8yrMEAN_mtrm2.png", width=800,height=600);
matplot(t(t(tmp)/w),lwd=4,xaxt="n",col=1:13,type="o",pch=as.character(1:9),main = "(mean) #files a maintainer maintained per month",xlab="Calendar year",ylab="(mean) #files",ylim=c(0,28))
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(1.7,18,legend=paste(mod,w,sep="/"),lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white");  
dev.off();
w = c(1,2,2,2,1,0.4,0.3);
tmp <- NA; for (i in mod) { tmp1 <- tapply(d1$nf[d1$mod==i],d1$y1[d1$mod==i],median); tmp <- cbind(tmp,tmp1)}
tmp = tmp[,2:length(tmp[1,])];
postscript("nf8yrMEDIAN_mtrm2.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("nf8yrMEDIAN_mtrm2.png", width=800,height=600);
matplot(t(t(tmp)/w),col=1:12,xaxt="n",type="b",lwd=4,pch=as.character(1:9),main = "(median) #files a maitainer maintained per month",xlab="Calendar year",ylab="(median) #files",ylim=c(0,6))
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(1.7,3.8,legend=paste(mod,w,sep="/"),lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white");  
dev.off();

tmp <- NA; for (i in mod) {tmp1 <- tapply(d1$nauth[d1$mod==i],d1$y1[d1$mod==i],mean,na.rm=T); tmp <- cbind(tmp,tmp1)}
postscript("natr8yrMEAN_mtrm2.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("natr8yrMEAN_mtrm2.png", width=800,height=600);
matplot(tmp[,2:length(tmp[1,])],lwd=4,col=1:12,xaxt="n",type="o",main = "(mean) #authors a maintainer responsible for per month",xlab="Calendar year",ylab="(mean) #authors",ylim=c(0,3.3))
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(1.5,2,legend=mod,lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white"); 
dev.off();
#tmp <- NA; for (i in mod) { tmp1 <- tapply(d1$nauth[d1$mod==i],d1$y1[d1$mod==i],median,na.rm=T); tmp <- cbind(tmp,tmp1)}
#postscript("natr8yrMEDIAN_mtrm2.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("natr8yrMEDIAN_mtrm2.png", width=800,height=600);
#matplot(tmp[,2:length(tmp[1,])],col=1:12,xaxt="n",lwd=3,type="o",main = "(median) #authors a maitainer responsible for per month for primary modules",xlab="Calendar year",ylab="(median) #authors")
#axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
#legend(1,.45,legend=mod,lwd=3,col=1:8,pch=as.character(1:9)); 
#dev.off();

tmp <- NA; for (i in mod) { tmp1 <- tapply(d1$ncmt[d1$mod==i],d1$y1[d1$mod==i],mean,na.rm=T); tmp <- cbind(tmp,tmp1)}
postscript("ncmt8yrMEAN_mtrm2.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("ncmt8yrMEAN_mtrm2.png", width=800,height=600);
matplot(tmp[,2:length(tmp[1,])],xaxt="n",lwd=4,col=1:12,type="o",main = "(mean) #commits a maintainer responsible for per month",xlab="Calendar year",ylab="(mean) #commits",ylim=c(0,10))
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(1.5,6,legend=mod,lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white"); 
dev.off();
#tmp <- NA; for (i in mod) { tmp1 <- tapply(d1$ncmt[d1$mod==i],d1$y1[d1$mod==i],median,na.rm=T); tmp <- cbind(tmp,tmp1)}
#postscript("ncmt8yrMEDIAN_mtrm2.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("ncmt8yrMEDIAN_mtrm2.png", width=800,height=600);
#matplot(tmp[,2:length(tmp[1,])],col=1:12,xaxt="n",lwd=3,type="o",main = "(median) #commits a maitainer responsible for per month for primary modules",xlab="Calendar year",ylab="(median) #commits")
#axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
#legend(6,1.5,legend=mod,lwd=3,col=1:8,pch=as.character(1:9)); 
#dev.off();

#################################
d <- delta;
#######Fraction of changed files among maintained files -- only run at pae
d1$nfchanged <- NA;
for (i in unique(d1$t)){
tmp <- tapply(d$f[d$ct==i],d$m[d$ct==i],spread);
d1$nfchanged[d1$t==i] <- tmp[match(d1$m[d1$t==i],names(tmp))];
}
d1$nfchanged[d1$nf>0 & is.na(d1$nfchanged)] <- 0;
d1$fracchgf <- d1$nfchanged/d1$nf0;
#d1$effort <- NA;
#d1$effort[d1$nf>0] <- 1;

#######commits on maintained files made by the maintainer
d1$ncmt_own <- NA;
for (i in unique(d1$t)){
tmp <- tapply(d$v[d$ct==i&d$an==d$m],d$m[d$ct==i&d$an==d$m],spread);
d1$ncmt_own[d1$t==i] <- tmp[match(d1$m[d1$t==i],names(tmp))];
}
d1$ncmt_own[is.na(d1$ncmt_own)] <- 0;
d1$owncmtFrac <- d1$ncmt_own/d1$ncmt0;

d1$njoiner <- NA;
for (i in unique(d1$t)){
tmp <- tapply(d$an[d$ct==i&d$fr==d$ct],d$m[d$ct==i&d$fr==d$ct],spread);
d1$njoiner[d1$t==i] <- tmp[match(d1$m[d1$t==i],names(tmp))];
}
d1$fracjoiner <- d1$njoiner/d1$nauth0;

###########
#Fraction of the measures we used done by top 20\% of maintainers
#Fraction of commits done by top 20\% of maintainers
d$topmtr <- 0;
for (j in 2009:2016){
for (i in mod){
sum80 <- 0;
tmp <- tapply(d$v[d$y==j&d$mod==i],d$m[d$y==j&d$mod==i],spread);
tmp <- sort(-tmp);
for (k in 1:length(tmp)) {
	if (sum80/sum(tmp) >= 0.8) break;		
	sum80 <- sum80+tmp[k];
	d$topmtr[d$y==j&d$m==(names(tmp)[k])] <- 1;
	}
}
}
topmtr <- NA;
nummtr <- NA;
for (j in 2009:2016){
tmp1 <- NA;
tmp2 <- NA;
for (i in mod){
tmp <- tapply(d$m[d$topmtr==1&d$mod==i&d$y==j],d$y[d$topmtr==1&d$mod==i&d$y==j],spread);
tmp1 <- cbind(tmp1,tmp);
tmp <- tapply(d$m[d$mod==i&d$y==j],d$y[d$mod==i&d$y==j],spread);
tmp2 <- cbind(tmp2,tmp);
}
topmtr <- rbind(topmtr,tmp1);
nummtr <- rbind(nummtr,tmp2);
}
topmtr_commits <- topmtr;
postscript("topmtr80cmt.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("topmtr80cmt0.png", width=800,height=600);
matplot((topmtr_commits/nummtr)[2:9,2:8],type="o",xaxt="n",col=1:9,lwd=4,main = "Fraction of maintainers who are responsible for 80% of commits",xlab="Calendar year",ylab="Fraction",ylim=c(0,0.8));
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(5.5,0.8,legend=mod,lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white"); 
dev.off();

#Fraction of authors related to top 20\% of maintainers
d$topmtr_atr <- 0;
for (j in 2009:2016){
for (i in mod){
sum80 <- 0;
tmp <- tapply(d$an[d$y==j&d$mod==i],d$m[d$y==j&d$mod==i],spread);
tmp <- sort(-tmp);
for (k in 1:length(tmp)) {
	if (sum80/sum(tmp) >= 0.8) break;		
	sum80 <- sum80+tmp[k];
	d$topmtr_atr[d$y==j&d$m==(names(tmp)[k])] <- 1;
	}
}
}
topmtr <- NA;
nummtr <- NA;
for (j in 2009:2016){
tmp1 <- NA;
tmp2 <- NA;
for (i in mod){
tmp <- tapply(d$m[d$topmtr_atr==1&d$mod==i&d$y==j],d$y[d$topmtr_atr==1&d$mod==i&d$y==j],spread);
tmp1 <- cbind(tmp1,tmp);
tmp <- tapply(d$m[d$mod==i&d$y==j],d$y[d$mod==i&d$y==j],spread);
tmp2 <- cbind(tmp2,tmp);
}
topmtr <- rbind(topmtr,tmp1);
nummtr <- rbind(nummtr,tmp2);
}
topmtr_atr <- topmtr;
postscript("topmtr80atr.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("topmtr80atr0.png", width=800,height=600);
matplot((topmtr_atr/nummtr)[2:9,2:8],type="o",col=1:9,xaxt="n",lwd=4,main = "Fraction of maintainers who are responsible for 80% of authors",xlab="Calendar year",ylab="Fraction",ylim=c(0,0.9));
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(5.5,0.85,legend=mod,lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white"); 
dev.off();

#Fraction of files related to top 20\% of maintainers
d$topmtr_f <- 0;
for (j in 2009:2016){
for (i in mod){
sum80 <- 0;
tmp <- tapply(d$f[d$y==j&d$mod==i],d$m[d$y==j&d$mod==i],spread);
tmp <- sort(-tmp);
for (k in 1:length(tmp)) {
	if (sum80/sum(tmp) >= 0.8) break;		
	sum80 <- sum80+tmp[k];
	d$topmtr_f[d$y==j&d$m==(names(tmp)[k])] <- 1;
	}
}
}
topmtr <- NA;
nummtr <- NA;
for (j in 2009:2016){
tmp1 <- NA;
tmp2 <- NA;
for (i in mod){
tmp <- tapply(d$m[d$topmtr_f==1&d$mod==i&d$y==j],d$y[d$topmtr_f==1&d$mod==i&d$y==j],spread);
tmp1 <- cbind(tmp1,tmp);
tmp <- tapply(d$m[d$mod==i&d$y==j],d$y[d$mod==i&d$y==j],spread);
tmp2 <- cbind(tmp2,tmp);
}
topmtr <- rbind(topmtr,tmp1);
nummtr <- rbind(nummtr,tmp2);
}
topmtr_file <- topmtr;
postscript("topmtr80file.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("topmtr80file0.png", width=800,height=600);
matplot((topmtr_file/nummtr)[2:9,2:8],type="o",col=1:9,xaxt="n",lwd=4,main = "Fraction of maintainers who are responsible for 80% of files",xlab="Calendar year",ylab="Fraction",ylim=c(0,0.75));
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(6,0.75,legend=mod,lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white"); 
dev.off();

#################################
d <- delta;
d$y = sub("\\.[0-9]*$", "", as.character(d$ct), perl=T);
mtr$y = sub("\\.[0-9]*$", "", as.character(mtr$ct), perl=T);

nn = c("drivers","arch","fs","net","sound","kernel","mm");

a<-table(d$y,d$mod,d$v)>0;
a0<-table(d$y,d$mod0,d$v)>0;
ncmt = apply(a,c(1,2),sum);
ncmt0 = apply(a0,c(1,2),sum);

w = c(10,3,1,1,1,1,.4);
postscript("ncommit4mod0.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("ncommit4mod0.png", width=800,height=600);
matplot(t(t(ncmt0[,nn])/w),type="o",lwd=4,xaxt="n",pch=as.character(1:8),col=1:8, main="#commits over time",xlab="Calendar year",ylab="Number",ylim=c(1,4500));
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(5,3000,legend=paste(nn, w,sep="/"), lwd=3,cex=1.5,pch=as.character(1:8),col=1:8,bg="white");
dev.off();
#png("ncommit4mod.png", width=800,height=600);
#postscript("ncommit4mod.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#matplot(t(t(ncmt[,nn])/w),type="o",lwd=3,xaxt="n",pch=as.character(1:8),col=1:8, main="#commits for primary modules",xlab="Calendar year from 2009");
#axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
#legend(1,8000,legend=paste(nn, w,sep="/"), lwd=3,pch=as.character(1:10),col=1:8);
#dev.off();

a<-table(d$y,d$mod,d$an)>0;
a0<-table(d$y,d$mod0,d$an)>0;
na = apply(a,c(1,2),sum);
na0 = apply(a0,c(1,2),sum);
wa = c(5,2,1,1,1,1,.5);
postscript("nauthor4mod0.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("nauthor4mod0.png", width=800,height=600);
matplot(t(t(na0[,nn])/wa),type="o", lwd=4,xaxt="n",pch=as.character(1:8),col=1:9, main="#authors over time",xlab="Calendar year",ylab="Number",ylim=c(1,600));
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(5,360,legend=paste(nn, wa,sep="/"), lwd=3,cex=1.5,pch=as.character(1:9),col=1:9,bg="white");
dev.off();
#wa = c(5,3,1,1,1,1,.5);
#png("nauthor4mod.png", width=800,height=600);
#postscript("nauthor4mod.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#matplot(t(t(na[,nn])/wa),type="o", lwd=3,xaxt="n",pch=as.character(1:8),col=1:9, main="#author for primary modules",xlab="Calendar year from 2009");
#axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
#legend(1,700,legend=paste(nn, wa,sep="/"), lwd=3,pch=as.character(1:9),col=1:9);
#dev.off();

nf0 = apply(table(as.character(mtr$mod0), as.character(mtr$y), as.character(mtr$f))>0,c(1,2),sum,na.rm=T);
nf = apply(table(as.character(mtr$mod), as.character(mtr$y), as.character(mtr$f))>0,c(1,2),sum,na.rm=T);
nm0 = apply(table(as.character(mtr$mod0), as.character(mtr$y), as.character(mtr$m))>0,c(1,2),sum,na.rm=T);
nm = apply(table(as.character(mtr$mod), as.character(mtr$y), as.character(mtr$m))>0,c(1,2),sum,na.rm=T);
wm = c(10,3,1,1,1,.5,.4);
#png("nmtr4mod0.png", width=800,height=600);
postscript("nmtr4mod0.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
matplot(t(nm0[nn,]/wm),type="o",lwd=4,xaxt="n",pch=as.character(1:9),col=1:9, main="#maintainers over time",xlab="Calendar year",ylab="Number",ylim=c(1,110));
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(2,110,legend=paste(nn, wm,sep="/"), lwd=3,cex=1.5,pch=as.character(1:9),col=1:9,bg="white");
dev.off();
#png("nmtr4mod.png", width=800,height=600);
#postscript("nmtr4mod.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#matplot(t(nm[nn,]/wm),type="o", lwd=3,xaxt="n",pch=as.character(1:9),col=1:9, main="#maintainers for primary modules",xlab="Calendar year from 2009");
#axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
#legend(1,70,legend=paste(nn, wm,sep="/"), lwd=3,pch=as.character(1:9),col=1:9);
#dev.off();

wf = c(10,10,1,1,1,.2,.05);
postscript("nf4mod0.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("nf4mod0.png", width=800,height=600);
matplot(t(nf0[nn,]/wf),type="o",lwd=4,xaxt="n",pch=as.character(1:9),col=1:9, main="#files over time",xlab="Calendar year",ylab="Number",ylim=c(1,2200));
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(4,1350,legend=paste(nn, wf,sep="/"), lwd=3,cex=1.5,pch=as.character(1:9),col=1:9,bg="white");
dev.off();
#png("nf4mod.png", width=800,height=600);
#postscript("nf4mod.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#matplot(t(nf[nn,]/wf),type="o",lwd=3,xaxt="n",pch=as.character(1:9),col=1:9, main="#files for primary modules");
#axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
#legend(1,4000,legend=paste(nn, wf,sep="/"), lwd=3,pch=as.character(1:9),col=1:9);
#dev.off();

####### joiners by time
joineryMTR0 <- NA;
joineryMTR <- NA;
for (i in mod){
  joineryMTR0 <- cbind(joineryMTR0,tapply(delta$an[delta$mod0==i],delta$fry[delta$mod0==i],spread));
  joineryMTR <- cbind(joineryMTR,tapply(delta$an[delta$mod==i],delta$fry[delta$mod==i],spread));
}
joineryMTR0 <- joineryMTR0[2:8,2:8];
joineryMTR0[,1] <- joineryMTR0[,1]/5;
joineryMTR0[,2] <- joineryMTR0[,2]/2;
wf = c(5,2,1,1,1,1,1);
postscript("njoiner4mod0.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#png("njoiner4mod0.png", width=800,height=600);
matplot(joineryMTR0,type="o",col=1:12,lwd=4,xaxt="n",main = "#joiners over time",xlab="Calendar year",ylab="Number",ylim=c(1,250));
axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
legend(1.2,150,legend=paste(mod, wf,sep="/"),lwd=3,cex=1.5,col=1:8,pch=as.character(1:9),bg="white"); 
dev.off();

#joineryMTR <- joineryMTR[2:8,2:8];
#joineryMTR[,1] <- joineryMTR[,1]/5;
#joineryMTR[,2] <- joineryMTR[,2]/2;
#wf = c(5,2,1,1,1,1,1);
#postscript("njoiner4mod.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
#matplot(joineryMTR,type="o",col=1:12,lwd=3,xaxt="n",main = "Number of joiners by year",xlab="Calendar year from 2010",ylab="Number");
#axis(1, at = c(1,2,3,4,5,6,7,8), labels =c(2009:2016));
#legend(1.5,360,legend=paste(mod, wf,sep="/"),lwd=3,col=1:8,pch=as.character(1:9)); 
#dev.off();

#below are for effort:
#nf is the number of files maintained in mod by m, nmtr is the aggrgation of maintainers over files in that module
# ntotgf is number of files in all "real"(good) modules that maintainer maintains that month (ntotgf are in all mods)
# ntotgf/ntotf is percent of files in real modules, presumably harder to change than Doc/include
# with counts of files, nums of other maintainers, and proportion of changes in "good"  modules 
m2m = read.table("mtr2mod",sep=";",comment.char="", quote="",
  col.names=c("nmtr","t","m","mod","nmod","nf", "ntotf","ntotgf","topMod"),fileEncoding="latin1");
# col.names=c("t","m","mod","nm","nf","nmtr","nmtrInv", "ntotf","ntotgf","topMod","frac"));
m2m = m2m[m2m$mod != "other",]
m2m = m2m[m2m$mod != "Documentation",]
m2m = m2m[m2m$nmtr>0,];
m2m$topMod=as.factor(as.character(m2m$topMod))
m2m$key = paste(m2m$t,m2m$m,sep=";");
first = function (x)x[1];
mod<-tapply(as.character(m2m$topMod),m2m$key,first);
nf <- tapply(m2m$nf,m2m$key,sum);
nmtr <- tapply(m2m$nmtr*m2m$nf,m2m$key,sum);
nfAdj<-tapply(m2m$nf/m2m$nmtr,m2m$key,sum);
m = sub("[^;]*;","",names(table(m2m$key)))
fracGood = tapply(m2m$ntotgf/m2m$ntotf,m2m$key,first);
key=names(table(m2m$key))
m2m$y1 <- as.factor(floor(m2m$t));

model = summary(lm(log(nfAdj)~log(nmtr/nf)+mod+m));
write(round(model$coefficients[1:13,],2),"workload",append=TRUE,sep=" & ");

#lets do ncmt and nauth
d = delta;
#d = read.table("delta1",sep=";",comment.char="", quote="",
#  col.names=c("m","nmtr","f","v","mod0","mod","an","ae","at","y","nadd","msg"));
d$key = paste(d$ct, d$m, sep=";");
#d$an = tolower(d$an);
d = d[d$nm>0,];
#aa = tapply(rep(1,dim(d)[1]),list(d$key, as.character(d$an)),mean);
dat = c();
for (y in names(table(d$ct))){
 ind = d$ct==y;

 ncmt = apply(tapply(1/d$nm[ind],list(as.character(d$m[ind]),as.character(d$v)[ind]), mean),1,sum,na.rm=T);
 nauth = apply(tapply(1/d$nm[ind],list(as.character(d$m[ind]), as.character(d$an)[ind]),mean),1,sum,na.rm=T);
 nmtr = apply(tapply(d$nm[ind],list(as.character(d$m[ind]), as.character(d$f)[ind]),mean),1,sum,na.rm=T);
 nf = apply(tapply(rep(1,sum(ind)),list(as.character(d$m[ind]), as.character(d$f)[ind]),mean),1,sum,na.rm=T);
 res = data.frame(y=rep(y,length(ncmt)));
 res$m = names(table(as.character(d$m[ind])));
 res$ncmt = ncmt;
 res$nauth = nauth;
 res$nmtr = nmtr;
 res$nf = nf;
 dat = rbind(dat, res);
}
write.table(dat, file="deltaProd.R",sep=";",quote=F,row.names=F,col.names=F);

dat <- read.table("deltaProd.R",sep=";",quote="",col.names=c("y","m","ncmt","nauth","nmtr","nf"))
dat$key=paste(dat$y,dat$m,sep=";");
dat$mod=mod[match(dat$key,key)];
modc <- summary(lm(log(ncmt)~log(nmtr/nf)+mod+m,data=dat));
moda <- summary(lm(log(nauth)~log(nmtr/nf)+mod+m,data=dat));
modc$adj.r.squared
[1] 0.6517597
write(round(modc$coefficients[1:13,],2),"workload",append=TRUE,sep=" & ");
write(round(moda$coefficients[1:13,],2),"workload",append=TRUE,sep=" & ");
moda$adj.r.squared

############### one-time-contributors
#install.packages("zoo");
library(zoo);
x = read.table("linux.delta.gz",sep=";",comment.char="", quote="",
col.names=c("prj", "v","tree","parent","an","cn","ae","ce","nadd","at","ct",
 "f","msg"),colClasses=c(rep("character",12)),fileEncoding="latin1");
x$at <- format(as.yearmon(as.POSIXct(as.integer(x$at), origin='1970-1-1')), '%Y.%m');
x$ct <- format(as.yearmon(as.POSIXct(as.integer(x$ct), origin='1970-1-1')), '%Y.%m');
x$an = tolower(x$an);
x$cn = tolower(x$cn);
#write.table(x[,c("f","v","an","ae","at","ct","nadd","msg")],"delta",sep=";", quote=F, row.names=F,col.names=F);
tmin <- tapply(as.numeric(x$ct), x$an, min, na.rm=T);
x$fr <- tmin[match(x$an,names(tmin))];
x$fr1 <- as.factor(x$fr);
x$fry <- as.factor(floor(x$fr));
tmax <- tapply(as.numeric(x$ct), x$an, max, na.rm=T);
x$to <- tmax[match(x$an,names(tmax))];

x <- x[-grep("=>",x$f),]
x$mod0 <- sub("/.*", "",x$f,perl=T,useBytes=T);

#calculating OTCs
tot <- tapply(x$v, x$an,spread);
x$tot <- tot[match(x$an,names(tot))]; # number of commits overall
x$ten <- (x$to-x$fr)+1; # number of months overall
x$isotc <- 0;
x$isotc[x$ten==1] <- 1;

tmp<-NA; for (i in mod) {tmp <- cbind(tmp,tapply(x$an[x$isotc==1&x$mod0==i],x$fry[x$isotc==1&x$mod0==i],spread));}
tmp = tmp[5:11,2:8];
tmp[,1] = tmp[,1]/10;
tmp[,2] = tmp[,2]/3;
wm = c(10,3,1,1,1,1,1);
#png("notc4mod0.png", width=800,height=600);
postscript("notc4mod0.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
matplot(tmp,type="o",lwd=4,xaxt="n",pch=as.character(1:9),col=1:9, main="#one-time contributors over time",xlab="Calendar year",ylab="Number");
axis(1, at = c(1,2,3,4,5,6,7), labels =c(2009:2015));
legend(1.5,67,legend=paste(mod, wm,sep="/"), lwd=3,cex=1.5,pch=as.character(1:9),col=1:9,bg="white");
dev.off();

tmax <- tapply(as.numeric(delta$ct), delta$an, max, na.rm=T);
delta$to <- tmax[match(delta$an,names(tmax))];
delta$ten <- (delta$to-delta$fr)+1; # number of months overall
delta$isotc <- 0;
delta$isotc[delta$ten==1] <- 1;
tmp<-NA; for (i in mod) {tmp <- cbind(tmp,tapply(delta$an[delta$isotc==1&delta$mod==i],delta$fry[delta$isotc==1&delta$mod==i],spread));}
tmp = tmp[1:7,2:8];
tmp[,1] = tmp[,1]/10;
tmp[,2] = tmp[,2]/3;
wm = c(10,3,1,1,1,1,1);
#png("notc4mod.png", width=800,height=600);
postscript("notc4mod.eps", width=10,height=6,horizontal=FALSE, onefile=FALSE, paper = "special");
matplot(tmp,type="o",lwd=4,xaxt="n",pch=as.character(1:9),col=1:9, main="#one-time contributors over time",xlab="Calendar year",ylab="Number");
axis(1, at = c(1,2,3,4,5,6,7), labels =c(2009:2015));
legend(1.1,98,legend=paste(mod, wm,sep="/"), lwd=3,cex=1.5,pch=as.character(1:9),col=1:9,bg="white");
dev.off();

dw = mtr[,c("m","mod1","mod")];
#dw$ismod = match(dw$mod1,mod);
#dw$mod1[is.na(dw$ismod)] = NA;
#dw$ismod = match(dw$mod,mod);
#dw$mod[is.na(dw$ismod)] = NA;

mod0mtr = tapply(dw$mod1,dw$m,spread);
modmtr = tapply(dw$mod,dw$m,spread);

dw1 = data.frame(table(paste(dw$m,dw$mod1,sep=";")))
dw1$m = sub(";.*", "",dw1$Var1,perl=T,useBytes=T);
dw1$mod0 = sub(".*;", "",dw1$Var1,perl=T,useBytes=T);
dw1$nmod0 = mod0mtr[match(dw1$m,names(mod0mtr))];
round(table(dw1$mod0[dw1$nmod0==1])/table(dw1$mod0),2);

dw2 = data.frame(table(paste(dw$m,dw$mod,sep=";")))
dw2$m = sub(";.*", "",dw2$Var1,perl=T,useBytes=T);
dw2$mod = sub(".*;", "",dw2$Var1,perl=T,useBytes=T);
dw2$nmod = modmtr[match(dw2$m,names(modmtr))];
round(table(dw2$mod[dw2$nmod==1])/table(dw2$mod),2);

tmp0 = tapply(dw$m,dw$mod1,spread);
tmp = tapply(dw$m,as.character(dw$mod),spread);

res = cbind(res,c(a,b,c,d,c/a,d/b,a/b));
#a - maintainers who have the most files maintained for that module (in comparison to other modules
#b - maintainers who ever maintained a file in that module
#c subset of a that has more than one module
#d subset of b that has more than one module
       arch drivers    fs kernel    mm   net sound
[1,] 219.00  962.00 65.00  27.00 13.00 61.00 30.00
[2,] 311.00 1139.00 93.00  54.00 27.00 96.00 69.00
[3,] 142.00  104.00 21.00  11.00  3.00 28.00 13.00
[4,] 234.00  281.00 49.00  38.00 17.00 63.00 52.00
[5,]   0.65    0.11  0.32   0.41  0.23  0.46  0.43
[6,]   0.75    0.25  0.53   0.70  0.63  0.66  0.75
[7,]   0.70    0.84  0.70   0.50  0.48  0.64  0.43

#commercial support of different modules (from Fengguang)
#"drivers","arch","fs","net","sound","kernel","mm"
nMtr_sup = c(195,43,12,9,6,15,0);
nMtr = c(574,134,32,46,45,41,9);
nF_sup = c(6215,5463,212,332,114,65,0);
nF = c(21567,16984,1826,1742,1902,356,112);






