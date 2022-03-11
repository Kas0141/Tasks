library(ape)
library(phytools)
text.string <- 
  "(((((((cow, pig),whale),(bat,(lemur,human))),(robin,iguana)),coelacanth),(gold_fish,trout)),shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle",bg='white',cex=1)
# Question 1: A shark is more closely related to the gold fish.
vert.tree
# Question 2: There are no branch lengths in this tree. 
str(vert.tree)
tree <- read.tree(text="(((A.B),(C,D)),E);")
plotTree(tree,offset=1)
tiplabels(frame="circle",bg='lightblue',cex=1)
nodelabels(frame="circle", bg='white',cex=1)
tree$tip.label
tree$edge
AnolisTree<-force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length,col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50),xlim=c(0,6))
tipEdges<-which(AnolisTree$edge[,2]<=Ntip(AnolisTree))
Lengths<-AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree,cex=0.25)
Labs<-sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
# Question 3: plot(AnolisTree, tiplabels=FALSE)
# Question 4: plot(AnolisTree, cex=0.25, type='radial')
# Question 5: plot(AnolisTree, cex=0.25, tip.color='red')
# Question 6-8: plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length,round,digits=2)
edgelabels(text=Labs, cex=0.25)
which(Lengths == min(Lengths))
names(Lengths)
AnolisTree2 <- drop.tip(AnolisTree, 'Anolis_occultus')
plot(AnolisTree2, cex=0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
# Question 9: there is exponential growth of the graph and does not go down, because the lizards are reproducing. The slope should not always be the same.  
# Questino 10:
fit.bd(AnolisTree, b, d, rho=0.2)
(b/lambda=0.8031), (d/mu=0), and log(L)=132.91613







