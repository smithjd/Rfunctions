# setup_my_google.R
#
# Call this once to set up methods to easily upload data frames to a Google Spreadsheet.
#
library(googlesheets)
library(httr)

# my_google <- oauth_app("google", key = Sys.getenv("jd8_oauth_id"), secret = Sys.getenv("jd8_oauth_id" ))

my_google <- oauth_app("google", key = Sys.getenv("jds_shambhala_id"), secret = Sys.getenv("jds_shambhala_secret" ))

google_token <- oauth2.0_token(oauth_endpoints("google"), my_google,
                               scope = "https://www.googleapis.com/auth/userinfo.profile")
