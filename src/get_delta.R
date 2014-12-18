get_delta <- function (data) {
        # directory is the folder where the datalist is to be found
        
        # file is the filename of the datalist.csv file
        
        # return is the datalist including information about patient names, 
        # complete datasets and sick or healthy kidneys
        

        
        data_out <- list()
        data_out[[1]] = data[[2]]-data[[1]]
        data_out[[2]] = 1
        data_out[[3]] = 1
        index <- c(1:length(data[[3]]))
        strvec <- names(data[[3]])
        for (i in 1:length(strvec)){
                index[i] <- which(names(data[[2]])==strvec[i])
                data_out[[2]][i] = data[[3]][i] - data[[2]][index[i]]                
                index[i] <- which(names(data[[1]])==strvec[i])
                data_out[[3]][i] = data[[3]][i] - data[[1]][index[i]]                
        }
        names(data_out[[2]]) <- strvec
        names(data_out[[3]]) <- strvec
        names(data_out) <- c("2-1", "3-2", "3-1")
        data_out
}        