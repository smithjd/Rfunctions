pretty_names <- function(df_name) {
  # saves data frame names and pretties them up
  # usage:
  #   iris <- good_names(iris)
  ##   upload or whatever... then restore the old names with:
  #   names(iris) <- old_name_list
  #
  old_name_list  <<- names(df_name)
  names(df_name) <-  gsub("[_.]"," ", names(df_name))
  df_name
}
