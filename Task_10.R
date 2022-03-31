setwd('C:\\Users\\Kayle\\OneDrive\\Desktop\\Evolution\\Tasks\\')
library(phytools)
help(phytools)
# Questions 1-3: 
trees <- list()
births <- vector()
Fractions <- vector()
random <- c()
random2 <- c()
treelist <- c()
for(i in 1:100) {
  births[i]<- runif(1)
  Fractions[i]<- runif(1)
  trees[[i]] <- pbtree(b=births[i], d=(Fractions[i]*births[i]),n=100)
  random[[i]]<-births[i]
  random2[[i]]<- (Fractions[i])
  treelist[[i]]<-mean(trees[[i]]$edge.length)
}
pdf("Trees.pdf")
plot(trees[[i]])
dev.off()
trees[[100]]$tip.label
# Question 4:
?log
sapply(trees, Ntip)
tips <- log(sapply(trees, Ntip))
radnom3 <- unlist(random)
head(tips)
pdf("log tips and diversification.pdf")
plot(tips, xlab="log of tips", ylab="net diversification", pch=16)
dev.off()
?cor
cor(tips, random3)
# Because the correlation test was 0.1245696 there isn't much of a correlation between the log of the tips and net diversification, though it is positive. If it had been closer to 1 or even -1 it would show that they were dependent upon each other.
# Question 5: 
random4 <- unlist(random2)
treelist2 <- unlist(treelist)
plot(treelist2, random3, ylab="Speciation rate", xlab="Average branch length", pch=4)
pdf("speciation rate and average branch length.pdf")
plot(treelist2, random3, ylab="Speciation rate", xlab="Average branch length", pch=1)
dev.off()
?cor
cor(treelist2, random4)
# Question 5-6: The correlation test was 0.1159449, which shows that the relationship is positive, but they are not dependent upon each other. 
# Question 7: 
Tree <- trees[[which.max(tips)]]
View(Tree)
pdf("Tree.pdf")
plot(Tree)
dev.off()
rates<-vector()
traits<-c()
meantraits<-c()
?var
vartraits<-c()
for(i in 1:100) {
  rates[i]<- runif(1)
  traits[[i]]<-fastBM(tree = Tree, sig2 = rates[i])
  meantraits[[i]]<-mean(traits[[i]])
  vartraits[[i]]<-var(traits[[i]])
}
# Question 8: 
meantraits<-unlist(meantraits)
pdf("meantraits and rates.pdf")
plot(meantraits, rates)
dev.off()
cor(meantraits, rates)
# The correlation was 0.02557711. This shows that there is a ver small relationship between the mean of traits and rates. 
# Question 9: 
vartraits<-unlist(vartraits)
pdf("vartraits and rates.pdf")
plot(vartraits, rates)
dev.off()
cor(vartraits, rates)
# The Correlation revealed 0.7144306, showing a much closer relationship between variance of traits and rates than that of mean of traits and rates, because it is much closer to 1.
# Question 10: 
pdf("traits[[1]] and traits[[2]].pdf")
plot(traits[[1]], traits[[2]])
dev.off()
cor(traits[[1]], traits[[2]])
traitMat<-cbind(traits[[1]], traits[[4]])
# the correlation between the first and second element of traits is -0.2723521. There is a negative correlation, but not very significant because it is not very close to -1. 


# Extra Credit:
?phylomorphospace
pdf("Extra Credit plot.pdf")
phylomorphospace(Tree, traitMat, xlab= "Trait 1", ylab= "Trait 2")
dev.off()




