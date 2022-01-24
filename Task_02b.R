Task02b
Question 1: The amount that Beren eats may in fact be directly correlated with his weight, but it is not a measureable hypothesis with this data becuase how much he actually ate is not part of our data, therefore we cannot measure the relationship. 
Hypothesis II is also inappropriate because it is reliant on how much Beren naps per day, but does not give a direct measurement of napping, like does it mean how many naps he took per day or the amount of hours total per day. 
setwd('C://Desktop//Evolution//Tasks//Task02')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
write.csv(Data, 'rawdata.csv', quote=F)
View(Data)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Data[257, ]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
I proved that each of these three ways to obtain the bottle events are the same because I got this same reading in the environment.  
It says int [1:322] with a bunch of data after.
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
head(beren2)
head(beren3)

Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
Feeds[1:6]
beren3$value[1:6]
beren3$age[1:6]
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab = "amount of milk consumed (oz)")
?par
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-comulativeMilkByDayTime.pdf")
unique(beren3$event)
Self-Quiz hypothesis: The number of wet diaper changes he has is positively related to his age. the older he gets the less wet diapers he has. 
The amount of milk Beren drinks is correlated to how long he naps that day. The longer he naps, the less milk he drinks. 
The fisrt graph does not give us any explanation as to why Beren's milk consumption fluctauted, the graph from ecampus has a lot of fluctuation to it. 
The other two graphs give us a better understanding of why this was actually happening. It shows that the amount of naps he has per day plus the duration of the nap highly negatively affects his milk consumption, the longer/more naps the less milk he drinks at the Daycare.
We were testing hypothesis III, The amount of milk Beren drinks in a day has increased over time, which we proved to be true. 
Diaper <- which(beren3$event == "wet")
avgWet <- mean(beren3$value[Diaper])
avgDiaper <- tapply(beren3$value[Diaper], beren3$age[Diaper], mean)
varDiaper <- tapply(beren3$value[Diaper], beren3$age[Diaper], var)
totalDiaper <- tapply(beren3$value[Diaper], beren3$age[Feeds], sum)
numDiaper <- tapply(beren3$value[Diaper], beren3$age[Diaper], length)
cor(beren3$value[Diaper], beren3$age[Diaper])
cor
cor.test
berenCor
summary(berenCor)
graph <- plot(beren4$age, Diaper)


Extra Credit
unique(beren3$event)
nap <- which(beren3$event == "nap")
beren4 <- beren3[nap,]
head(beren4)
Start <- beren4$start_hour+beren4$start_minute/60
End <- beren4$end_hour+beren4$start_minute/60
nap <- Start - End[which(Data$event == 'nap')]
nap <- Start + End[which(Data$event =='nap')]
boxplot(beren4$value[nap] ~ beren4$day[nap], xlab= "total sleep time", ylab= "day")
duration <- End-Start
graph <- plot(beren4$age, duration)
cor(beren4$value[nap], beren4$age[nap])
cor.test
berenCor
summary(berenCor)
berenANOVA
head(nap)
Start <- which(nap =="Start")
End <- which(nap =="End")
Duration <- which(End-Start =="Duration")
plot(Duration ~ beren3$day[nap])

                  
