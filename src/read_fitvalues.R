read_fitvalues <- function (datalist, type, timepoint) {	
	# datalasit = patient information	
	# type = type of kidney (sick "s" or healthy "h)	
	# timepoint =  1st, 2nd or 3rd measurement	
	# returns fitvalues from least squares fit
	
	patients <- datalist[,1]	
	sicklist <- which(datalist == type, arr.ind = TRUE)
	sicklist <- sicklist[sort.list(sicklist[,1]),]	
	measurements <- datalist[sicklist[,1],2:5]
	patients <- patients[sicklist[,1]]
			
# 	if (side == "R"){
# 		measurements <- datalist[,2:6]
# 	} else {
# 		measurements <- datalist[,c(2:5, 7)]
# 	}
# 	
# 	missing_kidneys <- which(measurements[,5] == "na",)
# 	
# 	if (length(missing_kidneys) > 0){
# 		patients <- patients[-missing_kidneys]
# 		measurements <- measurements[-missing_kidneys,]
# 	}
	
	if (timepoint ==3){
		missing_measurements <- which(measurements[,4] == 0)
		patients <- patients[-missing_measurements]
		measurements <- measurements[-missing_measurements,]
		sicklist <- sicklist[-missing_measurements,]
	}

	side <- character(length = length(patients))
	side[which(sicklist[,2] == 6)] <- "R"
	side[which(sicklist[,2] == 7)] <- "L"	
	filelist <- paste("./data/patients/",patients, "/",timepoint,"/Export/Kidney",side,"_cortex_fitvalues.csv", sep="")
	fitvalues <- lapply(filelist, read.csv2, header = TRUE, na.strings = c("NaN","#NAME?"), stringsAsFactors = FALSE)
	fitvalues <- lapply(fitvalues, function(x) x[,1:10])
	names(fitvalues) <- (patients) 
	fitvalues	
}	