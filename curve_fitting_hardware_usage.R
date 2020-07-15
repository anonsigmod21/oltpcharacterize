library(MASS)
library(fitdistrplus)
library(actuar)
library(moments)
##############################################################################
## prereq: database tables with hardware usage (disk IO, cpu, network)
## and workload demand data exported to the following CSV files
disk <-
    read.table(
        'op_get_delete_allops_join_io_aug_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <-
    read.table(
        'op_get_delete_allops_join_HW_aug_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )


#Filter Riak HOT buckets only:
cpunet <- cpunet[cpunet$LogType == 'riak_nodes', ]

disk <- disk[disk$LogType == 'riak_hot_buckets_io', ]


## sums
diskSums <-
    data.frame(
        cbind(
            disk$fiveminstamp,
            disk$VolumeIdleTimeSUM,
            disk$VolumeQueueLengthSUM,
            disk$VolumeReadBytesSUM,
            disk$VolumeReadOpsSUM,
            disk$VolumeTotalReadTimeSUM,
            disk$VolumeTotalWriteTimeSUM,
            disk$VolumeWriteBytesSUM,
            disk$VolumeWriteOpsSUM
        )
    )
colnames(diskSums) <-
    c(
        "fiveminstamp",
        "VolumeIdleTimeSUM",
        "VolumeQueueLengthSUM",
        "VolumeReadBytesSUM",
        "VolumeReadOpsSUM",
        "VolumeTotalReadTimeSUM",
        "VolumeTotalWriteTimeSUM",
        "VolumeWriteBytesSUM",
        "VolumeWriteOpsSUM"
    )
data <- merge(cpunet, diskSums, by = "fiveminstamp")
colnames(data)
diskAvgs <-
    data.frame(
        cbind(
            disk$fiveminstamp,
            disk$VolumeIdleTimeAVG,
            disk$VolumeQueueLengthAVG,
            disk$VolumeReadBytesAVG,
            disk$VolumeReadOpsAVG,
            disk$VolumeTotalReadTimeAVG,
            disk$VolumeTotalWriteTimeAVG,
            disk$VolumeWriteBytesAVG,
            disk$VolumeWriteOpsAVG
        )
    )
colnames(diskAvgs) <-
    c(
        "fiveminstamp",
        "VolumeIdleTimeAVG",
        "VolumeQueueLengthAVG",
        "VolumeReadBytesAVG",
        "VolumeReadOpsAVG",
        "VolumeTotalReadTimeAVG",
        "VolumeTotalWriteTimeAVG",
        "VolumeWriteBytesAVG",
        "VolumeWriteOpsAVG"
    )

data <- merge(data, diskAvgs, by = "fiveminstamp")
r <- data
plot(r$n, type = "l")
##############################################################################



##############################################################################
## Curve-fitting: what's the best model used to fit each one of these variables?
## Understanding how each I/O operation should be fitted differently improves the quality
## of predictions for that respective variable.
## helpful tutorial: http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html

##############################################################################
## High-res CDF plots for the paper:
##############################################################################
x = data$VolumeWriteBytesSUM

## normalize, per company request
minX = min(x)
x = x / minX

## fit different statistical distributions to the empirical data:
fit_n  <- fitdist(x, "norm")
fit_w  <- fitdist(x, "weibull")
fit_g  <- fitdist(x, "gamma")
fit_ln <- fitdist(x, "lnorm")
fit_exp <- fitdist(x, "exp")
fit_nbinom <- fitdist(x, "nbinom")
fit_unif <- fitdist(x, "unif")
fit_logis <- fitdist(x, "logis")
sh = fit_w$estimate[1]
sc = fit_w$estimate[2]
fit_pareto <-
    fitdist(x, "pareto", start = list(shape = sh, scale = sc))

## plot, high res:
dev.off()

counterName = "Normalized Write Throughput"
#(MB/sec)
png(
    "figures/cdf_diskwritebytes_aug.png",
    width = 1.2,
    height = 1,
    units = "in",
    res = 2400,
    pointsize = 2.5
)
cdfcomp(
    list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto),
    main = c("Empirical and theoretical CDFs", "(Best fits: Normal, Weibull)"),
    xlab = counterName,
    lwd = 0.8
)
axis(1, cex.lab = 2)

axis(2, cex.axis = 1.1, cex.lab = 1.6)
dev.off()

## compare fits:
denscomp(
    list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto),
    main = counterName,
    lwd = 3
)
rs <- gofstat(list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto))
## compare KS and AD statistics:
barplot(rs$ks, ylab = "Kolmogorov-Smirnov statistic", cex.names = 0.7)
barplot(rs$ad, ylab = "AD statistic", cex.names = 0.7)
summary(fit_n)
summary(fit_w)

##############################################################################
x = data$VolumeReadBytesSUM

## normalize, per company request
minX = min(x)
x = x / minX

## fit different statistical distributions to the empirical data:
fit_n  <- fitdist(x, "norm")
fit_w  <- fitdist(x, "weibull")
fit_g  <- fitdist(x, "gamma")
fit_ln <- fitdist(x, "lnorm")
fit_exp <- fitdist(x, "exp")
fit_nbinom <- fitdist(x, "nbinom")
fit_unif <- fitdist(x, "unif")
fit_logis <- fitdist(x, "logis")
sh = fit_w$estimate[1]
sc = fit_w$estimate[2]
fit_pareto <-
    fitdist(x, "pareto", start = list(shape = sh, scale = sc))

## plot, high res:
dev.off()

counterName = "Normalized Read Throughput"
#(MB/sec)
png(
    "figures/cdf_diskreadbytes_aug.png",
    width = 1.2,
    height = 1,
    units = "in",
    res = 2400,
    pointsize = 2.5
)
cdfcomp(
    list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto),
    main = c("Empirical and theoretical CDFs", "(Best fit: Gamma)"),
    xlab = counterName,
    lwd = 0.8
)
axis(1, cex.lab = 2)

axis(2, cex.axis = 1.1, cex.lab = 1.6)
dev.off()

## compare fits:
denscomp(
    list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto),
    main = counterName,
    lwd = 3
)
rs <- gofstat(list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto))
barplot(rs$ks, ylab = "Kolmogorov-Smirnov statistic", cex.names = 0.7)
barplot(rs$ad, ylab = "AD statistic", cex.names = 0.7)
summary(fit_g)
summary(fit_n)
summary(fit_w)
##############################################################################


##############################################################################
x = data$VolumeQueueLengthAVG

plot(ecdf(x))
fit_n  <- fitdist(x, "norm")
fit_w  <- fitdist(x, "weibull")
fit_g  <- fitdist(x, "gamma")
fit_ln <- fitdist(x, "lnorm")
fit_exp <- fitdist(x, "exp")
fit_nbinom <- fitdist(x, "nbinom")
fit_unif <- fitdist(x, "unif")
fit_logis <- fitdist(x, "logis")
sh = fit_w$estimate[1]
sc = fit_w$estimate[2]
fit_pareto <-
    fitdist(x, "pareto", start = list(shape = sh, scale = sc))

dev.off()

counterName = "Disk Queue Length"
#(MB/sec)
png(
    "figures/cdf_diskqueue_aug.png",
    width = 1.2,
    height = 1,
    units = "in",
    res = 2400,
    pointsize = 2.5
)
cdfcomp(
    list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto),
    main = c("Empirical and theoretical CDFs", "(Best fit: Weibull)"),
    xlab = counterName,
    lwd = 0.8
)
axis(1, cex.lab = 2)

axis(2, cex.axis = 1.1, cex.lab = 1.6)
dev.off()

rs <- gofstat(list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto))
barplot(rs$ks, ylab = "Kolmogorov-Smirnov statistic", cex.names = 0.7)
barplot(rs$ad, ylab = "AD statistic", cex.names = 0.7)
summary(fit_g)
summary(fit_n)
summary(fit_w)
##############################################################################


##############################################################################
x = data$CPUUtilAVG

plot(ecdf(x))
fit_n  <- fitdist(x, "norm")
fit_w  <- fitdist(x, "weibull")
fit_g  <- fitdist(x, "gamma")
fit_ln <- fitdist(x, "lnorm")
fit_exp <- fitdist(x, "exp")
fit_nbinom <- fitdist(x, "nbinom")
fit_unif <- fitdist(x, "unif")
fit_logis <- fitdist(x, "logis")
sh = fit_w$estimate[1]
sc = fit_w$estimate[2]
fit_pareto <-
    fitdist(x, "pareto", start = list(shape = sh, scale = sc))

dev.off()

counterName = "Average CPU Utilization"

png(
    "figures/cdf_cpu_aug.png",
    width = 1.2,
    height = 1,
    units = "in",
    res = 2400,
    pointsize = 2.5
)
cdfcomp(
    list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto),
    main = c("Empirical and theoretical CDFs", "(Best fit: Lognormal)"),
    xlab = counterName,
    lwd = 1
)
dev.off()


rs <- gofstat(list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto))
rs$ks
barplot(rs$ks, ylab = "Kolmogorov-Smirnov statistic", cex.names = 0.7)
rs$ad
barplot(rs$ad, ylab = "AD statistic", cex.names = 0.7)
summary(fit_g)
summary(fit_n)
summary(fit_w)
##############################################################################




##############################################################################
x = data$NetworkInAVG

plot(ecdf(x))
fit_n  <- fitdist(x, "norm")
fit_w  <- fitdist(x, "weibull")
fit_g  <- fitdist(x, "gamma")
fit_ln <- fitdist(x, "lnorm")
fit_exp <- fitdist(x, "exp")
fit_nbinom <- fitdist(x, "nbinom")
fit_unif <- fitdist(x, "unif")
fit_logis <- fitdist(x, "logis")
sh = fit_w$estimate[1]
sc = fit_w$estimate[2]
fit_pareto <-
    fitdist(x, "pareto", start = list(shape = sh, scale = sc))

dev.off()

counterName = "Normalized Network Traffic (Inbound)"

png(
    "figures/cdf_netIN_aug.png",
    width = 1.05,
    height = 1,
    units = "in",
    res = 2400,
    pointsize = 2.5
)
cdfcomp(
    list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto),
    main = c("Empirical and theoretical CDFs", "(Best fits: Gamma, Lognormal)"),
    xlab = counterName,
    lwd = 0.7
)
dev.off()

rs <- gofstat(list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto))
rs$ks
barplot(rs$ks, ylab = "Kolmogorov-Smirnov statistic", cex.names = 0.7)
rs$ad
barplot(rs$ad, ylab = "AD statistic", cex.names = 0.7)
summary(fit_g)
##############################################################################
x = data$NetworkOutAVG

plot(ecdf(x))
fit_n  <- fitdist(x, "norm")
fit_w  <- fitdist(x, "weibull")
fit_g  <- fitdist(x, "gamma")
fit_ln <- fitdist(x, "lnorm")
fit_exp <- fitdist(x, "exp")
fit_nbinom <- fitdist(x, "nbinom")
fit_unif <- fitdist(x, "unif")
fit_logis <- fitdist(x, "logis")
sh = fit_w$estimate[1]
sc = fit_w$estimate[2]
fit_pareto <-
    fitdist(x, "pareto", start = list(shape = sh, scale = sc))

dev.off()

counterName = "Normalized Network Traffic (Outbound)"
#(MB/sec)
png(
    "figures/cdf_netOUT_aug.png",
    width = 1.05,
    height = 1,
    units = "in",
    res = 2400,
    pointsize = 2.5
)
cdfcomp(
    list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto),
    main = c("Empirical and theoretical CDFs", "(Best fit: Lognormal)"),
    xlab = counterName,
    lwd = 0.7
)
dev.off()


rs <- gofstat(list(fit_n, fit_w, fit_g, fit_ln, fit_exp, fit_pareto))
rs$ks
barplot(rs$ks, ylab = "Kolmogorov-Smirnov statistic", cex.names = 0.7)
rs$ad
barplot(rs$ad, ylab = "AD statistic", cex.names = 0.7)
##############################################################################





##########################################################################
## ALL MONTHS: Compare evolution of distributions over time
rm(list = ls())


disk <-
    read.table(
        'op_get_delete_allops_join_io_aug_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <-
    read.table(
        'op_get_delete_allops_join_HW_aug_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <- cpunet[cpunet$LogType == 'riak_nodes', ]
disk <- disk[disk$LogType == 'riak_hot_buckets_io', ]
cpunet_aug <- cpunet
disk_aug <- disk

disk <-
    read.table(
        '../september/op_get_delete_allops_join_io_sept_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <-
    read.table(
        '../september/op_get_delete_allops_join_HW_sept_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <- cpunet[cpunet$LogType == 'riak_nodes', ]
disk <- disk[disk$LogType == 'riak_hot_buckets_io', ]
cpunet_sept <- cpunet
disk_sept <- disk

disk <-
    read.table(
        '../october/op_get_delete_allops_join_io_oct_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <-
    read.table(
        '../october/op_get_delete_allops_join_HW_oct_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <- cpunet[cpunet$LogType == 'riak_nodes', ]
disk <- disk[disk$LogType == 'riak_hot_buckets_io', ]
cpunet_oct <- cpunet
disk_oct <- disk


disk <-
    read.table(
        '../november/op_get_delete_allops_join_io_nov_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <-
    read.table(
        '../november/op_get_delete_allops_join_HW_nov_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <- cpunet[cpunet$LogType == 'riak_nodes', ]
disk <- disk[disk$LogType == 'riak_hot_buckets_io', ]
cpunet_nov <- cpunet
disk_nov <- disk


disk <-
    read.table(
        '../december/op_get_delete_allops_join_io_dec_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <-
    read.table(
        '../december/op_get_delete_allops_join_HW_dec_5min.csv',
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
    )

cpunet <- cpunet[cpunet$LogType == 'riak_nodes', ]

disk <- disk[disk$LogType == 'riak_hot_buckets_io', ]
cpunet_dec <- cpunet
disk_dec <- disk


################################################################################
## (1) Txns over time, including all months in the dataset:
allN <- c(disk_aug$n, disk_sept$n, disk_oct$n, disk_nov$n, disk_dec$n)
plot(allN, type = "l")
################################################################################
## (2) CDFs per month
counterName = "VolumeWriteBytesSUM"
colIdx = grep(counterName, colnames(disk_aug))
minAug = min(disk_aug[, colIdx])
disk_aug[, colIdx] = disk_aug[, colIdx] / minAug
disk_sept[, colIdx] = disk_sept[, colIdx] / minAug
disk_oct[, colIdx] = disk_oct[, colIdx] / minAug
disk_nov[, colIdx] = disk_nov[, colIdx] / minAug
disk_dec[, colIdx] = disk_dec[, colIdx] / minAug

## increase in busy months w.r.t. august
median(disk_nov[, colIdx]) / median(disk_aug[, colIdx])
median(disk_dec[, colIdx]) / median(disk_aug[, colIdx])

################################################################################
dev.off()
png(
    "figures/cdf_diskwritebytes_alllmonths.png",
    width = 1.1,
    height = 1,
    units = "in",
    res = 1200,
    pointsize = 2.5
)
plot(
    ecdf(disk_aug[, colIdx]),
    lwd = 1,
    main = "",
    xlab = "Normalized Write Throughput",
    ylab = "Empirical CDF",
    cex.lab = 1.3
)

lines(ecdf(disk_sept[, colIdx]),
      lty = 1,
      col = "darkgreen",
      lwd = 0.35)
lines(ecdf(disk_oct[, colIdx]),
      col = "red",
      lty = 1,
      lwd = 1)
lines(ecdf(disk_nov[, colIdx]),
      col = "blue",
      lty = 1,
      lwd = 0.75)
lines(ecdf(disk_dec[, colIdx]),
      col = "purple",
      lty = 1,
      lwd = 0.3)
legend(
    min(disk_aug[, colIdx]) + 0.3,
    0.95,
    legend = c("aug", "sept", "oct", "nov", "dec"),
    col = c("black", "darkgreen", "red", "blue", "purple"),
    lty = c(1, 1, 1, 1, 1),
    cex = 1,
    lwd = c(1, 0.35, 1, 0.75, 0.3),
    box.lwd = 0.2
)
dev.off()

################################################################################
counterName = "VolumeReadBytesSUM"
colIdx = grep(counterName, colnames(disk_aug))
minAug = min(disk_aug[, colIdx])
disk_aug[, colIdx] = disk_aug[, colIdx] / minAug
disk_sept[, colIdx] = disk_sept[, colIdx] / minAug
disk_oct[, colIdx] = disk_oct[, colIdx] / minAug
disk_nov[, colIdx] = disk_nov[, colIdx] / minAug
disk_dec[, colIdx] = disk_dec[, colIdx] / minAug

median(disk_dec[, colIdx]) / median(disk_aug[, colIdx])
median(disk_nov[, colIdx]) / median(disk_aug[, colIdx])
################################################################################
dev.off()

png(
    "figures/cdf_diskreadbytes_alllmonths.png",
    width = 1.1,
    height = 1,
    units = "in",
    res = 1200,
    pointsize = 2.5
)
plot(
    ecdf(disk_aug[, colIdx]),
    lwd = 1,
    main = "",
    #main =paste("CDF(",counterName,") by month",sep=""),
    xlab = "Normalized Read Throughput",
    ylab = "Empirical CDF",
    cex.lab = 1.3
)

lines(ecdf(disk_sept[, colIdx]),
      lty = 1,
      col = "darkgreen",
      lwd = 0.35)
lines(ecdf(disk_oct[, colIdx]),
      col = "red",
      lty = 1,
      lwd = 1)
lines(ecdf(disk_nov[, colIdx]),
      col = "blue",
      lty = 1,
      lwd = 0.75)
lines(ecdf(disk_dec[, colIdx]),
      col = "purple",
      lty = 1,
      lwd = 0.3)
legend(
    18,
    0.5,
    legend = c("aug", "sept", "oct", "nov", "dec"),
    col = c("black", "darkgreen", "red", "blue", "purple"),
    lty = c(1, 1, 1, 1, 1),
    cex = 1,
    lwd = c(1, 0.35, 1, 0.75, 0.3),
    box.lwd = 0.2
)
dev.off()

################################################################################
counterName = "VolumeQueueLengthAVG"
colIdx = grep(counterName, colnames(disk_aug))

median(disk_nov[, colIdx]) / median(disk_aug[, colIdx]) # 1.963
median(disk_dec[, colIdx]) / median(disk_aug[, colIdx]) # 2.485

dev.off()

png(
    "figures/cdf_diskqueue_alllmonths.png",
    width = 1.1,
    height = 1,
    units = "in",
    res = 1200,
    pointsize = 2.5
)
plot(
    ecdf(disk_aug[, colIdx]),
    lwd = 1,
    main = "",
    xlab = "Disk Queue Length",
    ylab = "Empirical CDF",
    cex.lab = 1.3
)

lines(ecdf(disk_sept[, colIdx]),
      lty = 1,
      col = "darkgreen",
      lwd = 0.35)
lines(ecdf(disk_oct[, colIdx]),
      col = "red",
      lty = 1,
      lwd = 1)
lines(ecdf(disk_nov[, colIdx]),
      col = "blue",
      lty = 1,
      lwd = 0.75)
lines(ecdf(disk_dec[, colIdx]),
      col = "purple",
      lty = 1,
      lwd = 0.3)
legend(
    0.6,
    0.5,
    legend = c("aug", "sept", "oct", "nov", "dec"),
    col = c("black", "darkgreen", "red", "blue", "purple"),
    lty = c(1, 1, 1, 1, 1),
    cex = 1,
    lwd = c(1, 0.35, 1, 0.75, 0.3),
    box.lwd = 0.2
)
dev.off()

################################################################################
counterName = "NetworkInAVG"
colIdx = grep(counterName, colnames(cpunet_aug))
minAug = min(cpunet_aug[, colIdx])

cpunet_aug[, colIdx] = cpunet_aug[, colIdx] / minAug
cpunet_sept[, colIdx] = cpunet_sept[, colIdx] / minAug
cpunet_oct[, colIdx] = cpunet_oct[, colIdx] / minAug
cpunet_nov[, colIdx] = cpunet_nov[, colIdx] / minAug
cpunet_dec[, colIdx] = cpunet_dec[, colIdx] / minAug

dev.off()
png(
    "figures/cdf_netIN_alllmonths.png",
    width = 1.1,
    height = 1,
    units = "in",
    res = 1200,
    pointsize = 2.5
)
plot(
    ecdf(cpunet_aug[, colIdx]),
    lwd = 1,
    main = "",
    xlab = "Normalized Network Traffic (Inbound)",
    ylab = "Empirical CDF"
)

lines(ecdf(cpunet_sept[, colIdx]),
      lty = 1,
      col = "darkgreen",
      lwd = 0.35)
lines(ecdf(cpunet_oct[, colIdx]),
      col = "red",
      lty = 1,
      lwd = 1)
lines(ecdf(cpunet_nov[, colIdx]),
      col = "blue",
      lty = 1,
      lwd = 0.75)
lines(ecdf(cpunet_dec[, colIdx]),
      col = "purple",
      lty = 1,
      lwd = 0.3)
legend(
    14,
    0.5,
    legend = c("aug", "sept", "oct", "nov", "dec"),
    col = c("black", "darkgreen", "red", "blue", "purple"),
    lty = c(1, 1, 1, 1, 1),
    cex = 1,
    lwd = c(1, 0.35, 1, 0.75, 0.3)
)
dev.off()

################################################################################
counterName = "NetworkOutAVG"
colIdx = grep(counterName, colnames(cpunet_aug))
minAug = min(cpunet_aug[, colIdx])

cpunet_aug[, colIdx] = cpunet_aug[, colIdx] / minAug
cpunet_sept[, colIdx] = cpunet_sept[, colIdx] / minAug
cpunet_oct[, colIdx] = cpunet_oct[, colIdx] / minAug
cpunet_nov[, colIdx] = cpunet_nov[, colIdx] / minAug
cpunet_dec[, colIdx] = cpunet_dec[, colIdx] / minAug


dev.off()
png(
    "figures/cdf_netOUT_alllmonths.png",
    width = 1.1,
    height = 1,
    units = "in",
    res = 1200,
    pointsize = 2.5
)
plot(
    ecdf(cpunet_aug[, colIdx]),
    lwd = 1,
    main = "",
    xlab = "Normalized Network Traffic (Outbound)",
    ylab = "Empirical CDF"
)

lines(ecdf(cpunet_sept[, colIdx]),
      lty = 1,
      col = "darkgreen",
      lwd = 0.35)
lines(ecdf(cpunet_oct[, colIdx]),
      col = "red",
      lty = 1,
      lwd = 1)
lines(ecdf(cpunet_nov[, colIdx]),
      col = "blue",
      lty = 1,
      lwd = 0.75)
lines(ecdf(cpunet_dec[, colIdx]),
      col = "purple",
      lty = 1,
      lwd = 0.3)
legend(
    14,
    0.5,
    legend = c("aug", "sept", "oct", "nov", "dec"),
    col = c("black", "darkgreen", "red", "blue", "purple"),
    lty = c(1, 1, 1, 1, 1),
    cex = 1,
    lwd = c(1, 0.35, 1, 0.75, 0.3)
)
dev.off()

################################################################################