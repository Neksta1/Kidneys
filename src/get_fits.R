get_fits <- function(data1, data2, data3, param) {
        # directory is the folder where the datalist is to be found
        
        # file is the filename of the datalist.csv file
        
        # return is the datalist including information about patient names, 
        # complete datasets and sick or healthy kidneys
        
        data_out <- list()
        
        index <- get_paramindex(param)
        
        
        data_out[[1]] <- as.numeric(sapply(data1, function(x) x[1,index]))
        names(data_out[[1]]) <- names(data1)
        data_out[[2]] <- as.numeric(sapply(data2, function(x) x[1,index]))
        names(data_out[[2]]) <- names(data2)
        data_out[[3]] <- as.numeric(sapply(data3, function(x) x[1,index]))
        names(data_out[[3]]) <- names(data3)
        data_out
}