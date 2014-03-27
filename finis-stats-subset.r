# Script to produce a subset of possible graphs for Finis
args<-commandArgs(TRUE)

library(graphics)
# library('Hmisc')
buylrd <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", 
    "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
library(lattice)

plot.multi.dens <- function(s, name="", colorvec, textvec, ablineflag=FALSE)
{
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s)) {
        junk.x = c(junk.x, density(s[[i]])$x)
        junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- range(junk.x)
    yr <- range(junk.y)

    plot(density(s[[1]]), xlim = xr, ylim = yr, main = name, col=colorvec, pch=19)
    legend('right', col=colorvec, legend=textvec, lty=rep(c(1), length(textvec)))
    if (ablineflag) {
        abline(v=log10(100))
    }

    for(i in 1:length(s)) {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = colorvec[i])
    }
}

#HISTOGRAM
my_data = read.csv(file='r-stats-drosophila.out', sep='\t', skip=1, stringsAsFactors = FALSE)
# convert to numbers and not factors
my_data[,10] = as.numeric(as.character(my_data[,10]))
histsub = subset(my_data, !is.na(my_data[,10]))
# print (histsub)
jpeg("histogram-all-drosophila.jpeg")
hist(as.numeric(histsub[,10]), main="Drosophila histogram of all real gap sizes.", nc=50)
dev.off()

methods = c("greedy", "quadprog")

# drawing the graphs
pdf("drosophila-graphs-subset.pdf")

#USING REAL GAP SIZE
#density plots
my_data = subset(my_data, !is.na(my_data[,10]))
subdensquad = subset(my_data, my_data[,8]=="quadprog")
subdensgreedy = subset(my_data, (my_data[,8]=="greedy" & my_data[,6] != "Invalid"))
subdensnot = subset(my_data, my_data[,6] == "Invalid")

my_cols = c("red", "blue", "coral4", "darkolivegreen1")
my_text = c("all gaps", "quadprog", "greedy", "unsolved")
# print(ncol(text))
plot.multi.dens(list(as.numeric(my_data[,10]), as.numeric(subdensquad[,10]),
    as.numeric(subdensgreedy[,10]), as.numeric(subdensnot[,10])), 
    name="Drosophila real gap size density", my_cols, my_text)

#log
sublog = subset(my_data, !is.na(my_data[,10]))
sublogquad = subset(sublog, sublog[,8] == "quadprog")
subloggreedy = subset(sublog, sublog[,8] == "greedy" & sublog[,6] != "Invalid")
sublognot =  subset(sublog, sublog[,6] == "Invalid")

plot.multi.dens(list(log10(as.numeric(sublog[,10]) + 100), log10(as.numeric(sublogquad[,10]) + 100), 
    log10(as.numeric(subloggreedy[,10]) + 100), log10(as.numeric(sublognot[,10]) + 100)), 
    name="Drosophila log real gap size density", my_cols, my_text, ablineflag=TRUE)

#correctness density plots
subvalid = subset(my_data, !is.na(my_data[,10]) & my_data[,6] != "Invalid")
subvalidcorrect = subset(subvalid, subvalid[,9] == 1)
subincorrect = subset(subvalid, subvalid[,9] == 0)
subvalidquad = subset(subvalid, subvalid[,8] == "quadprog")
subvalidquadcorrect = subset(subvalidquad, subvalidquad[,9] == 1)
subvalidquadincorrect = subset(subvalidquad, subvalidquad[,9] == 0)
subvalidgreedy = subset(subvalid, subvalid[,8] == "greedy")
subvalidgreedycorrect = subset(subvalidgreedy, subvalidgreedy[,9] == 1)
subvalidgreedyincorrect = subset(subvalidgreedy, subvalidgreedy[,9] == 0)

my_cols = c("red", "blue", "coral4", "darkolivegreen1", "black", "darkturquoise")
my_text = c("All correct gaps", "all wrong gaps", "quadprog correct gaps", 
    "quadprog incorrect gaps", "greedy correct gaps", "greedy incorrect gaps")

plot.multi.dens(list(as.numeric(subvalidcorrect[,10]), as.numeric(subincorrect[,10]), 
    as.numeric(subvalidquadcorrect[,10]), as.numeric(subvalidquadincorrect[,10]), 
    as.numeric(subvalidgreedycorrect[,10]), as.numeric(subvalidgreedyincorrect[,10])),
    name = "Drosophila density plot of correctness using real gap size", my_cols, my_text)

#log
sublog = subset(my_data, !is.na(my_data[,10]))
sublogcorrect = subset(sublog, sublog[,9] == 1)
subincorrect = subset(sublog, sublog[,9] == 0)
sublogquad = subset(sublog, sublog[,8] == "quadprog")
sublogquadcorrect = subset(sublogquad, sublogquad[,9] == 1)
sublogquadincorrect = subset(sublogquad, sublogquad[,9] == 0)
subloggreedy = subset(sublog, sublog[,8] == "greedy")
subloggreedycorrect = subset(subloggreedy, subloggreedy[,9] == 1)
subloggreedyincorrect = subset(subloggreedy, subloggreedy[,9] == 0)

plot.multi.dens(list(log10(as.numeric(sublogcorrect[,10]) + 100), log10(as.numeric(subincorrect[,10]) + 100), 
    log10(as.numeric(sublogquadcorrect[,10]) + 100), log10(as.numeric(sublogquadincorrect[,10]) + 100), 
    log10(as.numeric(subloggreedycorrect[,10]) + 100), log10(as.numeric(subloggreedyincorrect[,10]) + 100)),
    name = "Drosophila log density plot of correctness using real gap size", my_cols, my_text,
    ablineflag=TRUE)


#USING OPERA SIZE
#barplot of correctness
subpos = subset(subvalid, subvalid[,5] > 0)
subposgreedy = subset(subpos, subpos[,8] == "greedy")
subposquadprog = subset(subpos, subpos[,8] == "quadprog")
# subposreal = subset(subpos, subpos[])

percentage = sum(subpos[,9]>=1)/nrow(subpos)
percentage_greedy = sum(subposgreedy[,9]>=1)/nrow(subposgreedy)
percentage_quadprog = sum(subposquadprog[,9]>=1)/nrow(subposquadprog)
values = c(percentage, percentage_greedy, percentage_quadprog)

barplot(values, col=c("brown4", "coral4", "darkgoldenrod4"), 
    names.arg=c("All gaps", "greedy", "quadprog"), xlab="Method", 
    ylab="Correctness percentage", 
    main="Drosophila correctness percentage for gaps>0 using opera size.")

#scatterplot opera vs finis size
smoothScatter(as.numeric(subposgreedy[,6]),as.numeric(subposgreedy[,5]), nbin=100, nrpoints=Inf, 
    colramp = colorRampPalette(c(buylrd)), pch="", cex=.7, col="black", 
    xlab="Finis size", ylab="Opera size", 
    main=paste("Drosophila Opera vs Finis size>0 for greedy"))

smoothScatter(as.numeric(subposquadprog[,6]),as.numeric(subposquadprog[,5]), nbin=100, nrpoints=Inf, 
    colramp = colorRampPalette(c(buylrd)), pch="", cex=.7, col="black", 
    xlab="Finis size", ylab="Opera size", 
    main=paste("Drosophila Opera vs Finis size>0 for quadprog"))


# boxplot difference >0
boxplot(as.numeric(subposgreedy[,5])-as.numeric(subposgreedy[,6]),
    as.numeric(subposquadprog[,5])-as.numeric(subposquadprog[,6]), 
    outline=FALSE, col=c("darkorange3", "indianred4"), 
    names=c("greedy", "quadprog"), 
    main=paste("Boxplot of Opera-Finis size for size>0 
        Drosophila gaps using opera size"))

boxplot(as.numeric(subposgreedy[,10])-as.numeric(subposgreedy[,6]),
    as.numeric(subposquadprog[,10])-as.numeric(subposquadprog[,6]), 
    outline=FALSE, col=c("darkorange3", "indianred4"), 
    names=c("greedy", "quadprog"), 
    main=paste("Boxplot of Real-Finis size for size>0 
        Drosophila gaps using real size"))


# boxplots of opera vs finis sizes
boxplot(as.numeric(subposgreedy[,5]), as.numeric(subposgreedy[,6]), outline=FALSE, col=c("darkorange3", "darkorange4"), 
    names=c("Opera size", "Finis size"), main=paste("Drosophila boxplot of gap sizes>0 for greedy"))

# print (subposquadprog)
boxplot(as.numeric(subposquadprog[,5]), as.numeric(subposquadprog[,6]), outline=FALSE, col=c("darkorange3", "darkorange4"), 
    names=c("Opera size", "Finis size"), main=paste("Drosophila boxplot of gap sizes>0 for quadprog"))

dev.off()
