# setup_my_google.R
#
# source("~/Rfunctions/setup_my_google.R")
#
# Call this once to set up methods to easily upload data frames to a Google Spreadsheet.
#
# may need:  devtools::install_github("jennybc/googlesheets")
library(dplyr)
library(lazyeval)
library(googlesheets)
library(httr)

# change the default user with 'gs_auth(new_user = T)'

# function upload_google_ss
# assumes that you've set up the environment by calling setup_my_google.R
#
upload_google_ss <- function(df_name, title = "NONE", check = TRUE) {
  # title: label the data frame on Google Drive
  # check: launch a browser to look at the data frame when it's been uploaded
  if (is.data.frame(df_name)) {
    df_name_up <- deparse(substitute(df_name))
    df_name_quote_csv <- paste0(df_name_up,".csv")
    names(df_name) <-  gsub("[_.]"," ", names(df_name))
    write.csv(df_name, file = df_name_quote_csv, row.names = F) # create local copy
    sheet_title <- ifelse(title == "NONE", df_name_up, title)
    my_ss <- gs_upload(df_name_quote_csv, sheet_title = sheet_title) # local copy
    file.remove(df_name_quote_csv) # local copy
    if (check) browseURL(my_ss$browser_url)
    my_ss
  }
}
# # NAME the SPREADSHEET
# my_ss  <- gs_new(title = "Vancouver - new 1")
# # NAME the 1st WORKSHEET
# my_ss  <- gs_ws_rename( my_ss, from = 1, to = "mtcars", verbose = T)
# # UPLOAD data to the 1ST WORKSHEET
# gs_edit_cells(ss = my_ss, ws = "mtcars", input = mtcars, anchor = "A1", trim = T)
#
# # NAME the 2nd WORKSHEET
# my_ss  <- gs_ws_new( my_ss, ws = "iris", verbose = T)
# # UPLOAD data to the 2ND WORKSHEET
# gs_edit_cells(ss = my_ss, ws = "iris", input = iris, anchor = "A1", trim = T)
#
