datalist_directory <- "./data"
datalist_file <- "Data_list.csv" #import raw data

# load sources
source("./src/read_datalist.R")
source("./src/read_fitvalues.R")
source("./src/read_statistics.R")
source("./src/read_voxelvalues.R")
source("./src/get_median.R")
source("./src/get_delta.R")
source("./src/get_fits.R")
source("./src/get_paramindex.R")


datalist <- read_datalist(datalist_directory, datalist_file)

fitvalues_h_1 <- read_fitvalues(datalist, "h", 1)
fitvalues_h_2 <- read_fitvalues(datalist, "h", 2)
fitvalues_h_3 <- read_fitvalues(datalist, "h", 3)
fitvalues_s_1 <- read_fitvalues(datalist, "s", 1)
fitvalues_s_2 <- read_fitvalues(datalist, "s", 2)
fitvalues_s_3 <- read_fitvalues(datalist, "s", 3)

fitADC_h <- get_fits(fitvalues_h_1, fitvalues_h_2, fitvalues_h_3, "ADC")
deltafitADC_h <- get_delta(fitADC_h)
fitADC_s <- get_fits(fitvalues_s_1, fitvalues_s_2, fitvalues_s_3, "ADC")
deltafitADC_s <- get_delta(fitADC_s)

fitDA_h <- get_fits(fitvalues_h_1, fitvalues_h_2, fitvalues_h_3, "DA")
deltafitDA_h <- get_delta(fitDA_h)
fitDA_s <- get_fits(fitvalues_s_1, fitvalues_s_2, fitvalues_s_3, "DA")
deltafitDA_s <- get_delta(fitDA_s)

fitf_h <- get_fits(fitvalues_h_1, fitvalues_h_2, fitvalues_h_3, "f")
deltafitf_h <- get_delta(fitf_h)
fitf_s <- get_fits(fitvalues_s_1, fitvalues_s_2, fitvalues_s_3, "f")
deltafitf_s <- get_delta(fitf_s)

fitK_h <- get_fits(fitvalues_h_1, fitvalues_h_2, fitvalues_h_3, "K")
deltafitK_h <- get_delta(fitK_h)
fitK_s <- get_fits(fitvalues_s_1, fitvalues_s_2, fitvalues_s_3, "K")
deltafitK_s <- get_delta(fitK_s)

voxelvalues_h_1 <- read_voxelvalues(datalist, "h", 1)
voxelvalues_h_2 <- read_voxelvalues(datalist, "h", 2)
voxelvalues_h_3 <- read_voxelvalues(datalist, "h", 3)
voxelvalues_s_1 <- read_voxelvalues(datalist, "s", 1)
voxelvalues_s_2 <- read_voxelvalues(datalist, "s", 2)
voxelvalues_s_3 <- read_voxelvalues(datalist, "s", 3)


medianADC_h <- get_median(sapply(voxelvalues_h_1, function(x) x[,1]),
                          sapply(voxelvalues_h_2, function(x) x[,1]),
                          sapply(voxelvalues_h_3, function(x) x[,1]))
deltaADC_h <- get_delta(medianADC_h)
medianADC_s <- get_median(sapply(voxelvalues_s_1, function(x) x[,1]),
                          sapply(voxelvalues_s_2, function(x) x[,1]),
                          sapply(voxelvalues_s_3, function(x) x[,1]))
deltaADC_s <- get_delta(medianADC_s)

medianDA_h <- get_median(sapply(voxelvalues_h_1, function(x) x[,3]),
                         sapply(voxelvalues_h_2, function(x) x[,3]),
                         sapply(voxelvalues_h_3, function(x) x[,3]))
deltaDA_h <- get_delta(medianDA_h)
medianDA_s <- get_median(sapply(voxelvalues_s_1, function(x) x[,3]),
                         sapply(voxelvalues_s_2, function(x) x[,3]),
                         sapply(voxelvalues_s_3, function(x) x[,3]))
deltaDA_s <- get_delta(medianDA_s)

medianDB_h <- get_median(sapply(voxelvalues_h_1, function(x) x[,4]),
                         sapply(voxelvalues_h_2, function(x) x[,4]),
                         sapply(voxelvalues_h_3, function(x) x[,4]))
deltaDB_h <- get_delta(medianDB_h)
medianDB_s <- get_median(sapply(voxelvalues_s_1, function(x) x[,4]),
                         sapply(voxelvalues_s_2, function(x) x[,4]),
                         sapply(voxelvalues_s_3, function(x) x[,4]))
deltaDB_s <- get_delta(medianDB_s)

medianf_h <- get_median(sapply(voxelvalues_h_1, function(x) x[,5]),
                        sapply(voxelvalues_h_2, function(x) x[,5]),
                        sapply(voxelvalues_h_3, function(x) x[,5]))
deltaf_h <- get_delta(medianf_h)
medianf_s <- get_median(sapply(voxelvalues_s_1, function(x) x[,5]),
                        sapply(voxelvalues_s_2, function(x) x[,5]),
                        sapply(voxelvalues_s_3, function(x) x[,5]))
deltaf_s <- get_delta(medianf_s)

medianK_h <- get_median(sapply(voxelvalues_h_1, function(x) x[,9]),
                        sapply(voxelvalues_h_2, function(x) x[,9]),
                        sapply(voxelvalues_h_3, function(x) x[,9]))
deltaK_h <- get_delta(medianK_h)
medianK_s <- get_median(sapply(voxelvalues_s_1, function(x) x[,9]),
                        sapply(voxelvalues_s_2, function(x) x[,9]),
                        sapply(voxelvalues_s_3, function(x) x[,9]))
deltaK_s <- get_delta(medianK_s)

medianFA_h <- get_median(sapply(voxelvalues_h_1, function(x) x[,11]),
                        sapply(voxelvalues_h_2, function(x) x[,11]),
                        sapply(voxelvalues_h_3, function(x) x[,11]))
deltaFA_h <- get_delta(medianFA_h)
medianFA_s <- get_median(sapply(voxelvalues_s_1, function(x) x[,11]),
                        sapply(voxelvalues_s_2, function(x) x[,11]),
                        sapply(voxelvalues_s_3, function(x) x[,11]))
deltaFA_s <- get_delta(medianFA_s)

histvsfit_h_1 <- (medianADC_h[[1]] - as.numeric(sapply(fitvalues_h_1, function(x) x[,1])))/medianADC_h[[1]]

pdf("./graphs/Exam1vsExam2_healthy.pdf")
boxplot(medianADC_h[[1]], medianADC_h[[2]], 
        main = paste("ADC(mono), p-value=", wilcox.test(medianADC_h[[1]], medianADC_h[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"), 
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianDA_h[[1]], medianDA_h[[2]], 
        main = paste("D_slow, p-value=", wilcox.test(medianDA_h[[1]], medianDA_h[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"), 
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianDB_h[[1]], medianDB_h[[2]], 
        main = paste("D_fast, p-value=", wilcox.test(medianDB_h[[1]], medianDB_h[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianf_h[[1]], medianf_h[[2]], 
        main = paste("f, p-value=", wilcox.test(medianf_h[[1]], medianf_h[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"), ylim = c(5,30),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianK_h[[1]], medianK_h[[2]], 
        main = paste("Kurtosis, p-value=", wilcox.test(medianK_h[[1]], medianK_h[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianFA_h[[1]], medianFA_h[[2]], 
        main = paste("FA, p-value=", wilcox.test(medianFA_h[[1]], medianFA_h[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
dev.off()

pdf("./graphs/Exam1vsExam2_sick.pdf")
boxplot(medianADC_s[[1]], medianADC_s[[2]], 
        main = paste("ADC(mono), p-value=", wilcox.test(medianADC_s[[1]], medianADC_s[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianDA_s[[1]], medianDA_s[[2]], 
        main = paste("D_slow, p-value=", wilcox.test(medianDA_s[[1]], medianDA_s[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianDB_s[[1]], medianDB_s[[2]], 
        main = paste("D_fast, p-value=", wilcox.test(medianDB_s[[1]], medianDB_s[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianf_s[[1]], medianf_s[[2]], 
        main = paste("f, p-value=", wilcox.test(medianf_s[[1]], medianf_s[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"), ylim = c(5,30),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianK_s[[1]], medianK_s[[2]], 
        main = paste("Kurtosis, p-value=", wilcox.test(medianK_s[[1]], medianK_s[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianFA_s[[1]], medianFA_s[[2]], 
        main = paste("FA, p-value=", wilcox.test(medianFA_s[[1]], medianFA_s[[2]], paired=TRUE)[3]),
        names = c("Exam1", "Exam2"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
dev.off()

pdf("./graphs/HvsS_delta.pdf")
boxplot(deltaADC_h[[1]], deltaADC_s[[1]], 
        main = paste("ADC(mono), p-value=", wilcox.test(deltaADC_h[[1]], deltaADC_s[[1]], paired=FALSE)[3]))
boxplot(deltaDA_h[[1]], deltaDA_s[[1]], 
        main = paste("D_slow, p-value=", wilcox.test(deltaDA_h[[1]], deltaDA_s[[1]], paired=FALSE)[3]))
boxplot(deltaDB_h[[1]], deltaDB_s[[1]], 
        main = paste("D_fast, p-value=", wilcox.test(deltaDB_h[[1]], deltaDB_s[[1]], paired=FALSE)[3]))
boxplot(deltaf_h[[1]], deltaf_s[[1]], 
        main = paste("f, p-value=", wilcox.test(deltaf_h[[1]], deltaf_s[[1]], paired=FALSE)[3]))
boxplot(deltaK_h[[1]], deltaK_s[[1]], 
        main = paste("Kurtosis, p-value=", wilcox.test(deltaK_h[[1]], deltaK_s[[1]], paired=FALSE)[3]))
boxplot(deltaFA_h[[1]], deltaFA_s[[1]], 
        main = paste("FA, p-value=", wilcox.test(deltaFA_h[[1]], deltaFA_s[[1]], paired=FALSE)[3]))
dev.off()

pdf("./graphs/Healthy_vs_Sick_Exam1.pdf")
boxplot(medianADC_h[[1]], medianADC_s[[1]], 
        main = paste("ADC(mono), p-value=", wilcox.test(medianADC_h[[1]], medianADC_s[[1]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianDA_h[[1]], medianDA_s[[1]], 
        main = paste("D_slow, p-value=", wilcox.test(medianDA_h[[1]], medianDA_s[[1]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianDB_h[[1]], medianDB_s[[1]], 
        main = paste("D_fast, p-value=", wilcox.test(medianDB_h[[1]], medianDB_s[[1]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianf_h[[1]], medianf_s[[1]], 
        main = paste("f, p-value=", wilcox.test(medianf_h[[1]], medianf_s[[1]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianK_h[[1]], medianK_s[[1]], 
        main = paste("Kurtosis, p-value=", wilcox.test(medianK_h[[1]], medianK_s[[1]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianFA_h[[1]], medianFA_s[[1]], 
        main = paste("FA, p-value=", wilcox.test(medianFA_h[[1]], medianFA_s[[1]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
dev.off()

pdf("./graphs/Healthy_vs_Sick_Exam2.pdf")
boxplot(medianADC_h[[2]], medianADC_s[[2]], 
        main = paste("ADC(mono), p-value=", wilcox.test(medianADC_h[[2]], medianADC_s[[2]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianDA_h[[2]], medianDA_s[[2]], 
        main = paste("D_slow, p-value=", wilcox.test(medianDA_h[[2]], medianDA_s[[2]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianDB_h[[2]], medianDB_s[[2]], 
        main = paste("D_fast, p-value=", wilcox.test(medianDB_h[[2]], medianDB_s[[2]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianf_h[[2]], medianf_s[[2]], 
        main = paste("f, p-value=", wilcox.test(medianf_h[[2]], medianf_s[[2]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianK_h[[2]], medianK_s[[2]], 
        main = paste("Kurtosis, p-value=", wilcox.test(medianK_h[[2]], medianK_s[[2]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
boxplot(medianFA_h[[2]], medianFA_s[[2]], 
        main = paste("FA, p-value=", wilcox.test(medianFA_h[[2]], medianFA_s[[2]], paired=FALSE)[3]),
        names = c("Healthy", "Sick"),
        pars = list(boxwex = 0.8, boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, outlwd = 2))
dev.off()