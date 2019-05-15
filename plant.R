
#1. Prepare the list of species and percentage cover of your own survey in Excel.
#Put it together with one other pairâs data â you will have two columns with data and one with the species names. 
#i. Import this data in R and create a stacked barplot with the covers. 

data1 <- read.table("~/Environmental/PlantEcol3/test3.csv",header = T,sep = ",")

rownames <- data1[, 1]
data1[1:20,1:3]
data2 <- setNames(data.frame(t(data1[,-1])), data1[,1])
class(data2[,1])
par(mar=c(5,4,6,3))
barplot(as.matrix(t(data2)),las=2, ylab = "Cover by species (%)")
x<-recordPlot()


#ii. Calculate the number of species for the two columns.
sp <- as.matrix(data1[-1])
colSums(sp != 0)

#iii. Calculate the cumulative number of species.
newspec <- vector("numeric",length=ncol(data1)-1)
newspec[1] <- length(which(data1[,2]!=0))

for (j in 3: ncol(data1)){
  tabl <- data1[which(data1[,j]!=0),2:(j-1)]
  if (is.null(dim(tabl))==TRUE){
    ifelse(any(tabl==0),newspec[(j-1)] <- length(which(tabl==0)),newspec[(j-1)] <- 0)
  }else{
    a <- vector("logical",dim(tabl)[1])
    for (k in 1: dim(tabl)[1]){
      a[k] <- all(tabl[k,]==0)
    }
    newspec[(j-1)] <- length(which(a))}}

cumspec <- vector("numeric",length=ncol(data1)-1)
cumspec <- cumsum(newspec)

cumspec

#iv. Guess which âcommunityâ is more diverse.
#Cover 1 community is more diverse

#v. Decide which âcommunityâ is more diverse, yours or the other pairâs on the basis of Shannon and Simpson diversity indices.

#Shannon diversity index
library(vegan)
divtab <- diversity(data2,"shannon")
replayPlot(x)
par(new = T)
plot(divtab, pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2,ylim=c(0,2.5)) # add Shannon divrsity value to the previous plot with extra y axis
axis(4)
mtext("Diversity", side = 4, line=2)
y<-recordPlot()
species.shannon <- diversity(data2)
species.shannon

#simpson diversity index
divtab1 <- diversity(data2,"simpson")
replayPlot(y)
par(new = T)
points(divtab1, pch="O", col="red", xlab=NA, ylab=NA, cex=1.2, ylim=c(0,1.6)) 
axis(4)

par(xpd=T)
legend("top",bg="transparent",inset=c(0,-0.5), horiz=T,pch=c(16,1,4),
       col=c("black","red"),legend = c("Shannon index", "Simpson index"), lty=1:2, cex=0.8,
       box.lty=0)
z<-recordPlot()

#2.Prepare the 2Ã2 contingency table on the basis of your own data. 
#2.Calculate the expected occurrences and compare them to the observed occurrences. 
#2.Decide whether the two species are associated or not.

conting <- matrix(c(6,2,1,16),nrow=2)
conting
chisq.test(conting)
#We have a  chi-squared value of 9.6905 and a p-value of less that 0.05 significance level. 
#So we reject the null hypothesis and conclude that specie A and B have a significant relationship.

#expected occurance
chisq.test(conting, correct=F)$expected
#observed occurance
chisq.test(conting, correct=F)$observed

#3. Import the finches dataset!
library("cooccur")
data("finches")
cotab <- cotab_finches <- cooccur(mat = finches, type = "spp_site", spp_names = TRUE, prob = "comb")
str(cotab)
cotab$results[1:15,]
summary(cotab)
plot(cotab_finches)

