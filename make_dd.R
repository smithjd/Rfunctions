# make_dd produces a summary data frame (aka data dictionary)
#
# source("~/Rfunctions/make_dd.R")
#
# for example, called as:
#
# iris_dd <- make_dd(iris)

parse_str_out <- function(str_output) {
  require(stringr)
  str_output <- str_replace_all(str_output, ":", " :")
  var_name <- word(str_output, 3)
  var_format <- word(str_output, 5, sep = " +")
  df_out <- tibble(var_name, var_format)
  df_out
}

str_to_df <- function(df) {
  str_out <- capture.output(str(df, list.len = ncol(df)))
  # str_out <- capture.output(str(df))
  # str(str_out)
  str_as_df <- map_df(str_out, parse_str_out)
  str_as_df[2:length(str_out),]
}

fivenumsum <- function(x){
  # emulate fivenum function for characters, numerics and POSIXct
  # if the values of a variable are all missing, this function crashes

  require(tidyverse)
  require(lubridate)
  n_rows <- length(x)
  # handle SQL "missing value" for dates...
  x <- ifelse(is.character(x) &
                (x == "0000-00-00 00:00:00" | x == "0000-00-00"), "", x)
  x <- parse_guess(x)
  non_blanks <- x[complete.cases(x)]
  non_missing_n <- length(non_blanks)
  if (non_missing_n == 0) {
    summary <- tibble(n_rows = n_rows, num_blank = n_rows, num_unique = 0,
                      min = "", q_25 = "", q_50 = "", q_75 = "", max = "")
    return(summary)
  } else if (non_missing_n == 1) {
    summary <- tibble(n_rows = n_rows, num_blank = n_rows, num_unique = 0,
                      min = "", q_25 = "", q_50 = "", q_75 = "", max = "")
    summary$q_50 = as.character(non_blanks[1])
    return(summary)
  }
    else {
    # num_blank <- ifelse(is.Date(x),
    #                     sum(is.na(x)),
    #                     sum(x == ""))
    num_blank <- sum(is.na(x))
    num_unique <- length(unique(non_blanks))
    med_pos <- round((max(1,non_missing_n/2)),0)
    qrt_pos <- round((max(1,non_missing_n/4)),0)
    ordered <- non_blanks[order(non_blanks)]
    min <- ordered[1]
    q_25 <- ordered[(med_pos - qrt_pos)]
    q_50 <- ordered[med_pos]
    q_75 <- ordered[(med_pos + qrt_pos)]
    max <-  ordered[non_missing_n]
    summary <-
      tibble(n_rows, num_blank, num_unique,
             min = as.character(min),
             q_25 = as.character(q_25),
             q_50 = as.character(q_50),
             q_75 = as.character(q_75),
             max = as.character(max))
    return(summary)
  }
}

make_dd <- function(df, df_alias = NULL){
  require(tidyverse)
  df_name <- substitute(df)
  if(!is.na(df_alias)){df_name <- df_alias}
  df_var_nums <- dim(df)[2]
  fivesum <- map_df(df,fivenumsum)
  snames <- as.character(names(df))
  fivesum <- bind_cols(tibble(var_name = snames), fivesum)
  str_out <- str_to_df(df)
  label_df <- as_tibble(rep(as.character(df_name),df_var_nums))
  names(label_df) <- "table_name"
  dd_out <- left_join(str_out, fivesum, by = "var_name")
  dd_out <- bind_cols(label_df,dd_out)
  dd_out
}
