# version 2.0 of finis graphs R script
args = commandArgs(TRUE)
library(graphics)
buylrd = c("#313895", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", 
    "#FFFFBF", "#FEE090", "#FDAE81", "#F48D43", "#D73027", "#A50028")
library(lattice)

plot.multi.dens = function(s, name="", colorvec, textvec, 
    abline_flag=FALSE, abline_val=0)
{
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s)) {
        junk.x = c(junk.x, density(s[[i]])$x)
        junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr = range(junk.x)
    yr = range(junk.y)

    plot(density(s[[1]]), xlim = xr, ylim = yr, 
        main = name, col=colorvec, pch=19)
    legend('right', col=colorvec, legend=textvec, lty=rep(c(1), length(textvec)))
    if (abline_flag) {
        abline(v=abline_val)
    }

    for(i in 1:length(s)) {
        lines(density(s[[i]]), xlim = xr, ylim = yr, col = colorvec[i])
    }
}

if (length(args) <= 2) {
    print("Wrong number of arguments.")
    print("Rscript script.r <input_file> <out_prefix> <name_of_dataset>")
    stop("Exiting.")
}

input_file = args[1]
out_prefix = args[2]
data_name = args[3]

# do input and filtering of gaps
my_data = read.csv(file=input_file, sep='\t', skip=1, 
    stringsAsFactors = FALSE)
# convert to numbers and not factors
my_data[,10] = as.numeric(as.character(my_data[,10]))
# remove all invalid gaps (those that newContig marks with 0)
my_data = subset(my_data, !is.na(my_data[,10]))
num_gaps = nrow(my_data)

# gap closer numbers
if (data_name == "Drosophila") {
    # number of correctly closed / number of closed
    sub_gapcloser = 1976/2533
    # number of closed gaps
    gapcloser_num = 2553
} else if (data_name == "Celegans") {
    # number of correctly closed / number of closed
    sub_gapcloser = 8186/10802
    # number of closed gaps
    gapcloser_num = 10802
}

# colors and labels for graphs
my_cols = c("orangered", "darkolivegreen1", "indianred4", "navyblue")
my_cols_two = c("darkgoldenrod4", "darkorange3", "chocolate4")
my_text = c("all gaps", "quadprog", "greedy", "unsolved")
my_text_two =  c("All correct gaps", "All wrong gaps")
my_text_quad = c("quadprog correct gaps", "quadprog incorrect gaps") 
my_text_greedy = c("greedy correct gaps", "greedy incorrect gaps") 

# all possible needed subsets of data
# all filled gaps
sub_valid = subset(my_data, my_data[,6] != "Invalid")
# all unfilled gaps
sub_invalid = subset(my_data, my_data[,6] == "Invalid")
# filled correct
sub_valid_correct = subset(sub_valid, sub_valid[,9] == 1)
# filled incorrect
sub_valid_incorrect = subset(sub_valid, sub_valid[,9] == 0)
# all quad
sub_valid_quad = subset(sub_valid, sub_valid[,8] == "quadprog")
# correct quad
sub_valid_quad_correct = subset(sub_valid_quad, sub_valid_quad[,9] == 1)
# incorrect quad
sub_valid_quad_incorrect = subset(sub_valid_quad, sub_valid_quad[,9] == 0)
# all filled greedy
sub_valid_greedy = subset(sub_valid, sub_valid[,8] == "greedy")
# correct greedy
sub_valid_greedy_correct = subset(sub_valid_greedy, sub_valid_greedy[,9] == 1)
# incorrect greedy
sub_valid_greedy_incorrect = subset(sub_valid_greedy, sub_valid_greedy[,9] == 0)

# all the graphs follow

# density plot of sizes in log10
pdf(paste(out_prefix, "/log-dens-real.pdf", sep=""))

plot.multi.dens(list(log10(as.numeric(my_data[,10]) + 100), 
    log10(as.numeric(sub_valid_quad[,10]) + 100), 
    log10(as.numeric(sub_valid_greedy[,10]) + 100), 
    log10(as.numeric(sub_invalid[,10]) + 100)), 
    name=paste(data_name, "density of log of real gap size"), 
    my_cols, my_text,
    abline_flag=TRUE, abline_val=log10(100))

dev.off()

# barplot of correctness rate
pdf(paste(out_prefix, "/barplot-correctness.pdf", sep=""))

barplot(
    c(nrow(sub_valid_correct) / (nrow(sub_valid)), sub_gapcloser,
        nrow(sub_valid_quad_correct) / nrow(sub_valid_quad),
        nrow(sub_valid_greedy_correct) / nrow(sub_valid_greedy)),
    col=my_cols, 
    names.arg=c("Finis", "GapCloser", "Quadprog", "Greedy"),
    ylab="Correctness",
    main=paste(data_name, 
        "barplot of correctness of Finis and gapcloser"))

dev.off()

# boxplot of true gap size - finis size
pdf(paste(out_prefix, "/boxplot-finis-real.pdf", sep=""))

boxplot(as.numeric(sub_valid_greedy[,10])-as.numeric(sub_valid_greedy[,6]),
    as.numeric(sub_valid_quad[,10])-as.numeric(sub_valid_quad[,6]), 
    outline=TRUE, col=c("darkorange3", "indianred4"), 
    names=c("greedy", "quadprog"), 
    main=paste(data_name, "boxplot of Real-Finis size for all gaps"))

dev.off()

# scatterplot opera vs finis size
# greedy
pdf(paste(out_prefix, "/scatter-greedy.pdf", sep=""))

smoothScatter(as.numeric(sub_valid_greedy[,6]),as.numeric(sub_valid_greedy[,5]), 
    nbin=100, nrpoints=Inf, 
    colramp = colorRampPalette(c(buylrd)), 
    pch="", cex=.7, col="black", 
    xlab="Finis size", ylab="Opera size", 
    main=paste(data_name, "Opera vs Finis for greedy"))

dev.off()

#quadprog
pdf(paste(out_prefix, "/scatter-quad.pdf", sep=""))

smoothScatter(as.numeric(sub_valid_quad[,6]),as.numeric(sub_valid_quad[,5]), 
    nbin=100, nrpoints=Inf, 
    colramp = colorRampPalette(c(buylrd)), 
    pch="", cex=.7, col="black", 
    xlab="Finis size", ylab="Opera size", 
    main=paste(data_name, "Opera vs Finis for quadprog"))

dev.off()

# correctness density plots - 3 plots, all, quadprog, greedy
# all gaps
pdf(paste(out_prefix, "/log-dens-correctness-all-real.pdf", sep=""))

plot.multi.dens(list(log10(as.numeric(sub_valid_correct[,10]) + 100),
    log10(as.numeric(sub_valid_incorrect[,10]) + 100)), 
    name = paste(data_name, "density plot of correctness using real gap size"), 
    my_cols_two, my_text_two, abline_flag=TRUE, abline_val=log10(100))

dev.off()

# quad
pdf(paste(out_prefix, "/log-dens-correctness-quad-real.pdf", sep=""))

plot.multi.dens(list(log10(as.numeric(sub_valid_quad_correct[,10]) + 100),
    log10(as.numeric(sub_valid_quad_incorrect[,10]) + 100)), 
    name = paste(data_name, "density plot of correctness using real gap size"), 
    my_cols_two, my_text_quad, abline_flag=TRUE, abline_val=log10(100))

dev.off()

# greedy
pdf(paste(out_prefix, "/log-dens-correctness-greedy-real.pdf", sep=""))

plot.multi.dens(list(log10(as.numeric(sub_valid_greedy_correct[,10]) + 100),
    log10(as.numeric(sub_valid_greedy_incorrect[,10]) + 100)), 
    name = paste(data_name, "density plot of correctness using real gap size"), 
    my_cols_two, my_text_greedy, abline_flag=TRUE, abline_val=log10(100))

dev.off()

# barplot of number of gaps closed by finis/gapcloser
pdf(paste(out_prefix, "/barplot-num-closed.pdf", sep=""))

barplot(c(nrow(sub_valid), gapcloser_num,
    nrow(sub_valid_quad), nrow(sub_valid_greedy)),
    col=my_cols, 
    names.arg=c("Finis", "GapCloser", "Quadprog", "Greedy"),
    ylab="Number",
    main=paste(data_name, 
        "barplot of closed gaps of Finis and gapcloser"))

dev.off()

# barplot of correctness for each method
# portioned by size of the gaps
size_cats = c(0, 500, 2000)

overlaps = subset(sub_valid, sub_valid[,10] < size_cats[1])
small_gaps = subset(sub_valid, 
    sub_valid[,10] >= size_cats[1] & sub_valid[,10] < size_cats[2])
medium_gaps = subset(sub_valid, 
    sub_valid[,10] >= size_cats[2] & sub_valid[,10] < size_cats[3])
large_gaps = subset(sub_valid, 
    sub_valid[,10] >= size_cats[3])

overlaps_invalid = subset(sub_invalid, sub_invalid[,10] < size_cats[1])
small_gaps_invalid = subset(sub_invalid, 
    sub_invalid[,10] >= size_cats[1] & sub_invalid[,10] < size_cats[2])
medium_gaps_invalid = subset(sub_invalid, 
    sub_invalid[,10] >= size_cats[2] & sub_invalid[,10] < size_cats[3])
large_gaps_invalid = subset(sub_invalid, 
    sub_invalid[,10] >= size_cats[3])

# barplot of correctness for each method
methods = c("quadprog", "greedy")
leg = c("correct", "incorrect", "unsolved")

for (m in methods) {
    m_overlaps_correct = subset(overlaps, 
        overlaps[,8] == m & overlaps[,9] == 1)
    m_small_gaps_correct = subset(small_gaps, 
        small_gaps[,8] == m & small_gaps[,9] == 1)
    m_medium_gaps_correct = subset(medium_gaps, 
        medium_gaps[,8] == m & medium_gaps[,9] == 1)
    m_large_gaps_correct = subset(large_gaps, 
        large_gaps[,8] == m & large_gaps[,9] == 1)

    m_overlaps_incorrect = subset(overlaps, 
        overlaps[,8] == m & overlaps[,9] == 0)
    m_small_gaps_incorrect = subset(small_gaps, 
        small_gaps[,8] == m & small_gaps[,9] == 0)
    m_medium_gaps_incorrect = subset(medium_gaps, 
        medium_gaps[,8] == m & medium_gaps[,9] == 0)
    m_large_gaps_incorrect = subset(large_gaps, 
        large_gaps[,8] == m & large_gaps[,9] == 0)

    m_overlaps_invalid = subset(overlaps_invalid, 
        overlaps_invalid[,8] == m & overlaps_invalid[,9] == 0)
    m_small_gaps_invalid = subset(small_gaps_invalid, 
        small_gaps_invalid[,8] == m & small_gaps_invalid[,9] == 0)
    m_medium_gaps_invalid = subset(medium_gaps_invalid, 
        medium_gaps_invalid[,8] == m & medium_gaps_invalid[,9] == 0)
    m_large_gaps_invalid = subset(large_gaps_invalid, 
        large_gaps_invalid[,8] == m & large_gaps_invalid[,9] == 0)

    name = paste(out_prefix, "/", sep="")
    # name = out_prefix
    name = paste(name, m, sep="")
    pdf(paste(name, "-barplot-correctness-categories.pdf", sep=""))

    values_correct = c(nrow(m_overlaps_correct), nrow(m_small_gaps_correct), 
        nrow(m_medium_gaps_correct), nrow(m_large_gaps_correct))

    values_incorrect = c(nrow(m_overlaps_incorrect), nrow(m_small_gaps_incorrect), 
        nrow(m_medium_gaps_incorrect), nrow(m_large_gaps_incorrect))

    values_invalid = c(nrow(m_overlaps_invalid), nrow(m_small_gaps_invalid), 
        nrow(m_medium_gaps_invalid), nrow(m_large_gaps_invalid))


    # what does this even do?
    x = rbind(values_correct, values_incorrect, values_invalid)

    overlap_name = paste("<", size_cats[1], sep="")
    small_name = paste(size_cats[1], "-", sep="")
    small_name = paste(small_name, size_cats[2], sep="")
    medium_name = paste(size_cats[2], "-", sep="")
    medium_name = paste(medium_name, size_cats[3], sep="")
    large_name = paste(">=", size_cats[3], sep="")
    labels = c(overlap_name, small_name, medium_name, large_name)

    colnames(x) = labels
    barplot(x, beside=TRUE, col=my_cols_two, xlab = "Real gap size", 
        ylab="Number of results", 
        main = paste(data_name, paste("barplot of correctness for", m)))
    

    legend("top", "groups", legend=leg, col=my_cols_two, lty=c(1,1), lwd=c(3,3))

    dev.off()
}
