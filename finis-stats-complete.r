# final version of finis graphs script
args = commandArgs(TRUE)
if (length(args) <= 2) {
    print("Wrong number of arguments.")
    print("Rscript script.r <input_file> <out_prefix> <name_of_dataset>")
    stop("Exiting.")
}

input_file = args[1]
out_prefix = args[2]
data_name = args[3]

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

my_data = read.csv(file=input_file, sep='\t', skip=1, 
    stringsAsFactors = FALSE)
# convert to numbers and not factors
my_data[,10] = as.numeric(as.character(my_data[,10]))
my_data = subset(my_data, !is.na(my_data[,10]))
# print (my_data[,10])
# exclude invalid gaps from the analysis
my_cols = c("red", "blue", "coral4", "darkolivegreen1")
my_text = c("all gaps", "quadprog", "greedy", "unsolved")

pdf(paste(out_prefix, "-log-dens-real.pdf", sep=""))
# log of size density plot
sub_quad = subset(my_data, my_data[,8] == "quadprog")
sub_greedy = subset(my_data, my_data[,8] == "greedy" & my_data[,6] != "Invalid")
sub_not = subset(my_data, my_data[,6] == "Invalid")


plot.multi.dens(list(log10(as.numeric(my_data[,10]) + 100), 
    log10(as.numeric(sub_quad[,10]) + 100), 
    log10(as.numeric(sub_greedy[,10]) + 100), 
    log10(as.numeric(sub_not[,10]) + 100)), 
    name=paste(data_name, "density of log of real gap size"), 
    my_cols, my_text,
    abline_flag=TRUE, abline_val=log10(100))

dev.off()

#correctness density plots - 3 plots, all, quadprog, greedy
subvalid = subset(my_data, my_data[,6] != "Invalid")
sub_valid_correct = subset(subvalid, subvalid[,9] == 1)
sub_incorrect = subset(subvalid, subvalid[,9] == 0)
sub_valid_quad = subset(subvalid, subvalid[,8] == "quadprog")
sub_valid_quad_correct = subset(sub_valid_quad, sub_valid_quad[,9] == 1)
sub_valid_quad_incorrect = subset(sub_valid_quad, sub_valid_quad[,9] == 0)
sub_valid_greedy = subset(subvalid, subvalid[,8] == "greedy")
sub_valid_greedy_correct = subset(sub_valid_greedy, sub_valid_greedy[,9] == 1)
sub_valid_greedy_incorrect = subset(sub_valid_greedy, sub_valid_greedy[,9] == 0)

my_cols = c("darkgoldenrod4", "darkorange3")
my_text = c("All correct gaps", "all wrong gaps")

# all gaps
pdf(paste(out_prefix, "-log-dens-correctness-all-real.pdf", sep=""))

plot.multi.dens(list(log10(as.numeric(sub_valid_correct[,10]) + 100),
    log10(as.numeric(sub_incorrect[,10]) + 100)), 
    name = paste(data_name, "density plot of correctness using real gap size"), 
    my_cols, my_text, abline_flag=TRUE, abline_val=log10(100))

dev.off()

my_text = c("quadprog correct gaps", "quadprog incorrect gaps") 
pdf(paste(out_prefix, "-log-dens-correctness-quad-real.pdf", sep=""))

plot.multi.dens(list(log10(as.numeric(sub_valid_quad_correct[,10]) + 100),
    log10(as.numeric(sub_valid_quad_incorrect[,10]) + 100)), 
    name = paste(data_name, "density plot of correctness using real gap size"), 
    my_cols, my_text, abline_flag=TRUE, abline_val=log10(100))

dev.off()

my_text = c("greedy correct gaps", "greedy incorrect gaps") 
pdf(paste(out_prefix, "-log-dens-correctness-greedy-real.pdf", sep=""))

plot.multi.dens(list(log10(as.numeric(sub_valid_greedy_correct[,10]) + 100),
    log10(as.numeric(sub_valid_greedy_incorrect[,10]) + 100)), 
    name = paste(data_name, "density plot of correctness using real gap size"), 
    my_cols, my_text, abline_flag=TRUE, abline_val=log10(100))

dev.off()

# barplot of cathegories for each method
size_cats = c(0, 500, 2000)
overlaps = subset(my_data, my_data[,10] < size_cats[1])
small_gaps = subset(my_data, 
    my_data[,10] >= 0 & my_data[,10] < size_cats[2])
medium_gaps = subset(my_data, 
    my_data[,10] >= size_cats[2] & my_data[,10] < size_cats[3])
large_gaps = subset(my_data, 
    my_data[,10] >= size_cats[3])

# print(small_gaps)
# barplot of correctness for each method
methods = c("quadprog", "greedy")
# my_col = c()
# 
leg = c("correct", "incorrect")
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

    name = paste(out_prefix, "-", sep="")
    name = paste(name, m, sep="")
    pdf(paste(name, "-barplot-correctness-categories.pdf", sep=""))

    values = c(nrow(m_overlaps_correct), nrow(m_overlaps_incorrect),
        nrow(m_small_gaps_correct), nrow(m_small_gaps_correct),
        nrow(m_medium_gaps_correct), nrow(m_medium_gaps_incorrect),
        nrow(m_large_gaps_correct), nrow(m_large_gaps_incorrect))

    values = matrix(values, nr=2, byrow=TRUE)


    overlap_name = paste("<", size_cats[1], sep="")
    small_name = paste(size_cats[1], "-", sep="")
    small_name = paste(small_name, size_cats[2], sep="")
    medium_name = paste(size_cats[2], "-", sep="")
    medium_name = paste(medium_name, size_cats[3], sep="")
    large_name = paste(">=", size_cats[3], sep="")
    labels = c(overlap_name, small_name, medium_name, large_name)
    # names(values) = labels
    # print(labels)
    barplot(values, col=my_cols, beside=TRUE, 
        names.arg=labels, xlab = "Real gap size", 
        ylab="Number of results",
        main = paste("Barplot of correctness for", m))
    legend("topright", legend=leg, col=my_cols, lty=c(1,1), lwd=c(3,3))

    dev.off()

}

