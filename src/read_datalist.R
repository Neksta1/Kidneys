read_datalist <- function (directory, file) {
	# directory is the folder where the datalist is to be found
	
	# file is the filename of the datalist.csv file
	
	# return is the datalist including information about patient names, 
	# complete datasets and sick or healthy kidneys
	
	datalist <- read.csv2(paste(directory, "/", file, sep=""), na.strings = c("na"))
	
}	