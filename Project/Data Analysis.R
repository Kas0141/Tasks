# Hypothesis: Cancer cases within the US have increased from 2010 to 2020 due to the mutation of TP53. 
setwd('C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\')

Dat1 <- read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Cancer Data-Kaylea Stover.csv")
Dat2 <- read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Cancer Data Mutations-Kaylea Stover.csv")
Dat3 <- read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Cancer Data TP53-Kaylea Stover.csv")

Males <- as.matrix(Dat1[which(Dat1[,5]=="Male"),])
Females <- as.matrix(Dat1[which(Dat1[,5]=="Female"),])


pdf("barplot.pdf")
par(mfrow=c(1,2), mar=c(6,4,1,1), las=2, mgp=c(3, 0.1, 0), cex.axis=0.75)
barplot(cbind(as.numeric(Males[,2]), as.numeric(Males[,3]))~Males[,1], beside=TRUE, xlab="Males", ylab="")
barplot(cbind(as.numeric(Females[,2]), as.numeric(Females[,3]))~Females[,1], beside=TRUE, xlab="Females", ylab="")
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
z<-read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\z Cancer cases for 2010.csv")
x<-read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\x Cancer .csv")
y<-read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\y TP53.csv")

Dat5<-read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\2010 both sexes cancer.csv")
head(Dat5)
head(x)
head(y)
head(z)
Data4<-read.csv(file="C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Cancer 2 Data.csv")
head(Data4)
plot(as.numeric(Data4[,2]), as.numeric(Data4[,3]), xlab="Number of cancer cases in 2010", ylab="% Affected by TP53 Mutations in 2010")
cor.test(as.numeric(Data4[,2]), as.numeric(Data4[,3]))

