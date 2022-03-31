# Hypothesis: Cancer cases within the US have increased from 2010 to 2020 due to the mutation of TP53. 
setwd('C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\')

Dat1 <- read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Cancer Data-Kaylea Stover.csv")
Dat2 <- read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Cancer Data Mutations-Kaylea Stover.csv")
Dat3 <- read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Cancer Data TP53-Kaylea Stover.csv")

Males <- as.matrix(Dat1[which(Dat1[,5]=="Male"),])
Females <- as.matrix(Dat1[which(Dat1[,5]=="Female"),])


pdf("barplot.pdf")
par(mfrow=c(1,2), mar=c(6,4,1,1), las=2)
barplot(cbind(as.numeric(Males[,2]), as.numeric(Males[,3]))~Males[,1], beside=TRUE, xlab="", ylab="Number of Cancer Cases for Males")
barplot(cbind(as.numeric(Females[,2]), as.numeric(Females[,3]))~Females[,1], beside=TRUE, xlab="", ylab="Number of Cases for Females")
dev.off()


Int <- intersect(Females[,1], Males[,1])
rownames(Males) <- Males[,1]
rownames(Females) <- Females[,1]
plot(1:length(Int), as.numeric(Males[Int,4]), type='l', xaxt="n", xlab="", ylim=c(0, 100), lwd=2, col="orange", ylab="% change from 2010 to 2020")
axis(1, at=1:length(Int), labels=Int, las=2)
lines(1:length(Int), as.numeric(Females[Int,4]), lwd=2, col="green")

barplot(Dat3[,2], names.arg=Dat3[,1], las=2, ylim=c(0,50), xlab="", ylab="Percent associated with TP53 Mutation in 2010")

head(Dat1)
head(Dat2)
head(Dat3)
