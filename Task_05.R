install.packages("learnPopGen")
library(learnPopGen)
coalescent.plot()
install.packages("rehh", dep=T)
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
library(coala)
model <- coal_model(sample_size = 5, loci_number = 10, loci_length =
  500, ploidy = 2) +
  feat_mutation(10) +
  sumstat_trees() +
  sumstat_nucleotide_div()
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
# The numbers are not all the same. DRift, mutation, and different loci make the differences. 
Nloci <- length(stats$trees)
install.packages("ape")
library(ape)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
# Because many individuals have a common ancestor so therefore they will come off of the same thing. 
Agel <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
# The most recent common ancestor is at about 0.3 while the first is at 0.8, so they are not the same age. 
# They do not match. 
par(mfrow=c(1,2))
plot(t1)
aixsPhylo()
plot(t2)
axisphylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_, t1_2)
for(locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for(n in 1:ntrees){
    if(locus ==1 && n == 1){
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
  else {
    outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus
              ]][n]))
  }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
model <- coal_model(sample_size = 5, loci_number = 10, loci_length =
500, ploidy = 2) +
  recombination_rate(10) +
  sumstat_trees() +
  sumstat_nucleotide_div()
stats <- simulate(model, nsim = 1)
# It will show the recombination of genes instead of the mutations, which I believe would increase the number. 
# It is different in the way that I expected it to be. 
model3 <- coal_model(10, 50)+
  feat_mutation(par_prior("theta", sample.int(100, 1)))+
  sunstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x)mean(x$pi))
theta <- sapply(stats, function(x)x$pars[["theta"]])
compare.chronograms(mean_pi, theta)
