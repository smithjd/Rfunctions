# My functions library
My R function library collected in one place

* pretty_names.R - creates nice names for data frame variables. Looks good as headers on a Google Spreadsheet
* setup_my_google.R - include file to make it easy and fast to upload a data frame to a Google Spreadsheet
* make_dd.R - creates a snapshot of a data frame that is nice when added to metadata obtained from SQL or elsewhere.  By processing every column in the data frame, the output data frame has these variables:
     + var_name
     + var_format
     + n_rows
     + num_blank
     + num_unique
     + min
     + q_25
     + q_50
     + q_75
     + max   