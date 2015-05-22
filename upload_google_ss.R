# upload_google_ss.R
#
# assumes that you've set up the environment by calling setup_my_google.R
#
upload_google_ss <- function(df_name) {
  if (is.data.frame(df_name)) {
    df_name_quote_csv <- paste(deparse(substitute(df_name)),".csv", sep = "")
    write.csv(df_name, file = df_name_quote_csv, row.names = F) # create local copy
    my_ss <- upload_ss(df_name_quote_csv) # local copy
    my_ss
    file.remove(df_name_quote_csv) # local copy
  }
  else {
    message("Needs the name of a data frame.")
  }
}
