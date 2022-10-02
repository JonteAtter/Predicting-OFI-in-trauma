#### Need to add collumns here???

remove_columns <- function(data) {

data <- data[,-grep("VK_", colnames(data))]
data <- data[,-grep("AIS", colnames(data))]
data <- data[,-grep("ICD_", colnames(data))]
data <- data[,-grep("pac_", colnames(data))]

return(data)
}
