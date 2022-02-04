source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), type = "l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h = 1, s = 0)
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation - Observed) ^ 2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "="~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(5, 0, 0, 35)
Chisq <- sum(((Expectation - Observed) ^ 2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "="~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(2, 3, 10, 30)
Chisq <- sum(((Expectation - Observed) ^ 2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "="~.(Chisq)), legend.text=c("expected", "observed"))
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white","#d53e4f","#fee08b","#abdda4","#3288bd","black")
calcChi(counts[1.])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
Avg <- mean(Chisqs)
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
propSig <- length(which(Chiqs > 11.70))/length(Chisqs)
percSig <- round(100 * propSig)
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for(i in backgrounds)
  Data <- Chisqs[which(results[,3] ==i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter <- counter + 1
abline(v = 11.70, lty=2, lwd =2, col='black')
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v = 11.70, lty=2, lwd=2)
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0, 0, 0, 0.25))
mtext(side=2, at=8, line=0, "sel.sim.")
Simulation6 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
n.sims <- 1000
n.color <- rbinom(n.sims, 1000, 0.25)
hist(n.color)
simPop(Popsize=100, nGenerations=100, h=1, s=0, initial_p=0.5, mu = 0, twoway = TRUE, w = NULL)                                    
                                      Questions:
                                    # It makes sense that the most fit allele would go to fixation, but I believe that it's not always the case.
                                    # This is mainly due to genetic drift and heritability, because genetic drift happens by chance when the frequency
                                     of certain genotypes changes within a population as some individuals do not reproduce. 
                                    # Also by chance certain genotypes may be passed on the next generation while the others are not, though there may be a 50/50 chance of either aa or ab
                                     being passed down, but the only offspring inherited ab and so did there offfspring. 
                                    # When observing all 10's the x^2 value 360. 
                                    # When observing all 40's it was also 360. 
                                    # Average Chi-squared is 60.99.
                                    # The squared values observed for all the plots had different ranges, for example one was 300 while another was 3. 
                                    # The chi-squared does not look like it differs by background. The percSig value of 92 didn't actually surprise me and I dont really know why. 
                                    # I do not think that the only thing driving that is natural selection.
                                    # I dont see anything that tells me a percentage to know if it is greater than 11.7.
                                    # The mixture is similar. 
                                    # To me it doesn't actually look like there is a storng correlation.
                                    # Natural selection is one of the evolutionary processes involoved.
                                    # Because the chi-squared value increases I also think that the mutations will do the same. 
                                    # The students numbers are actually pretty accurate. 
                                     
                                     
  
