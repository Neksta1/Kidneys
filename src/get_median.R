get_median <- function (data1, data2, data3) {
        # directory is the folder where the datalist is to be found
        
        # file is the filename of the datalist.csv file
        
        # return is the datalist including information about patient names, 
        # complete datasets and sick or healthy kidneys
        
        data_out <- list(sapply(lapply(data1, as.numeric), median),
                     sapply(lapply(data2, as.numeric), median),
                     sapply(lapply(data3, as.numeric), median))
        names(data_out) <- c("Exam1", "Exam2", "Exam2")
        data_out
}	