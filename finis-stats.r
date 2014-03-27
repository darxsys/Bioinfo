# Script to produce all possible graphs for Finis

library(graphics)
# library('Hmisc')
buylrd <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", 
    "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
library(lattice)

plot.multi.dens <- function(s, name="")
{
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s)) {
        junk.x = c(junk.x, density(s[[i]])$x)
        junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- range(junk.x)
    yr <- range(junk.y)
    # print("aaaaaa")
    # print(s[[1]])
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = name)
    for(i in 1:length(s)) {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = i)
    }
}

# drawing the graphs
pdf("drosophila-graphs.pdf")
data = read.csv(file='r-stats.out', sep='\t', skip=1)
subvalid = subset(data, data[,6] != 'Invalid')
subpos = subset(subvalid, subvalid[,5] > 0)
subvalidgreedy = subset(subvalid, subvalid[,8]=='greedy')
subposgreedy = subset(subvalidgreedy, subvalidgreedy[,5]>0)
subvalidquadprog = subset(subvalid, subvalid[,8]=='quadprog')
subposquadprog = subset(subvalidquadprog, subvalidquadprog[,5]>0)


# # all gap density plot - real gap size
# plot.multi.dens(list(subrealsize[,10], subrealgreedy[,10], subrealquadprog[,10]),
#     name="Drosophila density plot for all gaps")


# all gaps size distribution
hist(subvalid[,5], nc=8, col='chocolate4', xlab='Opera size', 
    ylab='Number', main="Drosophila Opera size distribution")

# all gaps larger than 0
hist(subpos[,5], nc=8, col='chocolate4', xlab='Opera size', ylab='Number', 
    main="Drosophila Opera size distribution>0")

# same but log
my_hist = hist(log(subpos[,5]), nc=8, plot=FALSE)
plot(my_hist, col='chocolate4', log='x', xlab='Opera size', ylab='Number', 
    main="Drosophila Opera size distribution>0 log scale")


#scatterplot for all
smoothScatter(subvalid[,5]~subvalid[,6], nbin=100, nrpoints=Inf, 
    colramp = colorRampPalette(c(buylrd)), pch="", cex=.7, col="black", 
    xlab="Finis size", ylab="Opera size", main="Opera vs Finis size for Drosophila all gaps")


# boxplot for all gaps
boxplot(subvalid[,5], subvalid[,6], outline=FALSE, col=c("darkorange3", "darkorange4"), 
    names=c("Opera size", "Finis size"), main="Boxplot of sizes for all Drosophila gaps")

# boxplot for gaps > 0
boxplot(subpos[,5], subpos[,6], outline=FALSE, col=c("darkorange3", "darkorange4"), 
    names=c("Opera size", "Finis size"), main="Boxplot of sizes>0 for Drosophila gaps")

# boxplot ratio
boxplot(subvalid[,6]/subvalid[,5], outline=FALSE, col=c("darkorange3"), 
    names=c("Ratio Finis/Opera size"), main="Boxplot of Finis/Opera ratio for all Drosophila gaps")

# boxplot ratio > 0
boxplot(subpos[,6]/subpos[,5], outline=FALSE, col=c("darkorange3"), 
    names=c("Ratio Finis/Opera size"), main="Boxplot of Finis/Opera ratio for size>0 Drosophila gaps")

# boxplot difference
boxplot(subvalid[,5]-subvalid[,6], outline=FALSE, col=c("darkorange3"), 
    names=c("Opera - Finis size"), main="Boxplot of Opera-Finis size for all Drosophila gaps")

# boxplot difference >0
boxplot(subpos[,5]-subpos[,6], outline=FALSE, col=c("darkorange3"), 
    names=c("Opera - Finis size"), main="Boxplot of Opera-Finis size for size>0 Drosophila gaps")


# correctness
# percentage = sum((data[,8]=="greedy") & (data[,9]>=1))/sum(data[,8]=="greedy")
percentage = sum(subvalid[,9]>=1)/nrow(subvalid)
percentage_greedy = sum(subvalidgreedy[,9]>=1)/nrow(subvalidgreedy)
percentage_quadprog = sum(subvalidquadprog[,9]>=1)/nrow(subvalidquadprog)
values = c(percentage, percentage_greedy, percentage_quadprog)
barplot(values, col=c("brown4", "coral4", "darkgoldenrod4"), names.arg=c("All gaps", "greedy", "quadprog"), xlab="Method", 
    ylab="Correctness percentage", main="Drosophila correctness percentage for all gaps.")

# correctness > 0
percentage = sum(subpos[,9]>=1)/nrow(subpos)
percentage_greedy = sum(subposgreedy[,9]>=1)/nrow(subposgreedy)
percentage_quadprog = sum(subposquadprog[,9]>=1)/nrow(subposquadprog)
values = c(percentage, percentage_greedy, percentage_quadprog)
barplot(values, col=c("brown4", "coral4", "darkgoldenrod4"), names.arg=c("All gaps", "greedy", "quadprog"), xlab="Method", 
    ylab="Correctness percentage", main="Drosophila correctness percentage for gaps>0.")


# TODO - put density plots here

methods = c("greedy", "quadprog")

for (m in methods) {
    print (m)
    submethod = subset(subvalid, subvalid[,8] == m)
    submethodpos = subset(subpos, subpos[,8] == m)
    # print(submethodpos)
    # title = paste()
    # all gaps size distribution for method m
    hist(submethod[,5], nc=8, col='chocolate4', xlab='Opera size', ylab='Number', 
        main=paste("Drosophila Opera size distribution for",m))

    # all gaps size distribution for method m >0
    hist(submethodpos[,5], nc=8, col='chocolate4', xlab='Opera size', ylab='Number', 
        main=paste("Drosophila Opera size>0 distribution for",m))

    # same but log
    my_hist = hist(log(submethodpos[,5]), nc=8, plot=FALSE)
    plot(my_hist, col='chocolate4', log='x', xlab='Opera size', ylab='Number', 
        main=paste("Drosophila Opera size distribution>0 log scale for",m))

    # scatter plots
    smoothScatter(submethod[,5]~submethod[,6], nbin=100, nrpoints=Inf, 
        colramp = colorRampPalette(c(buylrd)), pch="", cex=.7, col="black", 
        xlab="Finis size", ylab="Opera size", main=paste("Drosophila Opera vs Finis all gap sizes for",m))
    smoothScatter(submethodpos[,5]~submethodpos[,6], nbin=100, nrpoints=Inf, 
        colramp = colorRampPalette(c(buylrd)), pch="", cex=.7, col="black", 
        xlab="Finis size", ylab="Opera size", main=paste("Drosophila Opera vs Finis size>0 for",m))


    # box plots
    boxplot(submethod[,5], submethod[,6], outline=FALSE, col=c("darkorange3", "darkorange4"), 
        names=c("Opera size", "Finis size"), main=paste("Drosophila boxplot of all gap sizes for", m))
    boxplot(submethodpos[,5], submethodpos[,6], outline=FALSE, col=c("darkorange3", "darkorange4"), 
        names=c("Opera size", "Finis size"), main=paste("Drosophila boxplot of gap sizes>0 for", m))

    #boxplots ratio
    boxplot(submethod[,6]/submethod[,5], outline=FALSE, col=c("darkorange3", "darkorange4"), 
        names=c("Opera size/Finis size"), main=paste("Drosophila boxplot finis/opera ratio of all gap sizes for", m))    
    boxplot(submethodpos[,6]/submethodpos[,5], outline=FALSE, col=c("darkorange3", "darkorange4"), 
        names=c("Opera size/Finis size"), main=paste("Drosophila boxplot finis/opera ratio of gaps >0 for", m)) 

    # boxplots difference
    boxplot(submethod[,5]-submethod[,6], outline=FALSE, col=c("darkorange3"), 
        names=c("Opera - Finis size"), 
        main=paste("Boxplot of Opera-Finis size for all Drosophila gaps for", m))

    # boxplot difference >0
    boxplot(submethodpos[,5]-submethodpos[,6], outline=FALSE, col=c("darkorange3"), 
        names=c("Opera-Finis size"), 
        main=paste("Boxplot of Opera-Finis size for size>0 Drosophila gaps for",m))

    # TODO - add density plots
}

dev.off()
