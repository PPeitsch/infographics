# Library imports
library(tidyverse)
# Datasets folder
data_folder <- "datasets"


# I need to modify, I think the column names doesn't appears
# ademas lo imprime como el culo
create_datasets <- function(data_folder){
    "This function return a list of df of tables present in a folder."
    # List of files on datasets folder
    list_files <- list.files(data_folder)
    # Number of datasets
    n <- length(list_files)
    
    df <- list()
    for (i in 1:n){
        temp_name <- list_files[i]
        temp_name <- tools::file_path_sans_ext(temp_name)
        tmp_df <- data.frame(read.csv(file.path(data_folder, list_files[i]), dec=","))
        #assign(temp_name, )
        df[i] <- append(df, tmp_df)
    }
    return(df)
}