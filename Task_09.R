plot(tree, type="fan")
# Question 1: There are four tips to the tree. The tree has no branch lengths.
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
head(data)
# Question 2: The data is type of lizards with the size of snout. The data is pretty consistent, all of the data is less than 6 but more that 3.  
svl <- setNames(data$svl, rownames(data))
svl<-setNames(svl,rownames(svl))
svl
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
# Question 3: The estimated values are stored in Ancestors. The CI95 is 95% confidence intervals for state estimations. 
# Question 4: We are assuming that the data is normally distributed and that they are independent of each other. 
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan",lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16,cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)),tip1=c(
  "Anolis_alinger", "Anolis_alinger", "Anolis_occultus", "Anolis_ricordii
  ","Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus
                                                      ", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_
                                                      angusticeps", "Anolis_angusticeps"))
fossilNodes <- c()
nodeN <- c()
# Question 5: answered lines 64-68 and 71-76. 
x_for <- nodeN
for(i in 1:10) {              
  x_for <- x_for + 1          
  print(x_for)
}
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i] <- fossilData[i, "svl"]
nodeN[i] <- Node
x_for <- nodeN[i]
for(i in 1:10) {              
  x_for <- x_for + 1          
  print(x_for)
}
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
# Question 7: plot(x=data$svl, y=data$fossilData) 
# points(x=data$svl, y=data$fossilData)
# It looks as the estimated data may have been slightly too high based on the fossil data.
# Question 8-10: 
unlink("C:\\Users\\Kayle\\OneDrive\\Documents\\R\\win-library\\4.1/00LOCK")
options("install.lock" = FALSE)
install.packages("geiger")
library(geiger)
fitContinuous(phy, dat, SE = 0,
   model = c("svl","fossilData" CI = 0.95), ncores=NULL,)
# I woud say that this model is different than what fastAnc assumes. 


