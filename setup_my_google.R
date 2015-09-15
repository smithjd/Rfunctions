# setup_my_google.R
#
# Call this once to set up methods to easily upload data frames to a Google Spreadsheet.
#
# may need:  devtools::install_github("jennybc/googlesheets")
library(dplyr)
library(lazyeval)
library(googlesheets)
library(httr)

# my_google <- oauth_app("google", key = Sys.getenv("jd8_oauth_id"), secret = Sys.getenv("jd8_oauth_secret" ))

my_google <- oauth_app("google", key = Sys.getenv("jds_shambhala_id"), secret = Sys.getenv("jds_shambhala_secret" ))

google_token <- oauth2.0_token(oauth_endpoints("google"), my_google,
                               scope = "https://www.googleapis.com/auth/userinfo.profile")

# function upload_google_ss
#
# assumes that you've set up the environment by calling setup_my_google.R
#
upload_google_ss <- function(df_name, title = "NONE") {
  if (is.data.frame(df_name)) {
    df_name_quote_csv <- paste(deparse(substitute(df_name)),".csv", sep = "")
    names(df_name) <-  gsub("[_.]"," ", names(df_name))
    write.csv(df_name, file = df_name_quote_csv, row.names = F) # create local copy
    sheet_title <- ifelse(title == "NONE", df_name, title)
    my_ss <- gs_upload(df_name_quote_csv, sheet_title = sheet_title) # local copy
    file.remove(df_name_quote_csv) # local copy
    my_ss
  }
}
