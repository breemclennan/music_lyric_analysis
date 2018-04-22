# R Script: 00_DataLoad.R
# Description: Import PDF files and convert to one large dataset, and a feather file for future usage
# Author: Bree McLennan
# Date: 22/04/2018
#
# ======================================================================================================================== #

# Setup

# Load libraries
library(rprojroot)
library(data.table)
library(feather)
library(tm)       #recommended
library(pdftools)
library(RWeka)
library(tidytext) #recommended
#library(quenteda) #Recommended

# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 
# Example usage: F("Data/Raw") 

# Get a List of all files named with a key word, use regex pattern to identify only the athletics graded results csv files
filenames <- list.files(F("Data/Raw") , pattern = "*.pdf", full.names = TRUE)

# Load and bind all data sets
#raw.data <- rbindlist(lapply(filenames,fread))


# TODO: the heading styles arent being interpreted
# maybe this package can help https://cran.r-project.org/web/packages/officer/vignettes/word.html
tt <- readPDF(control = list(text = "-layout")) 
rr1 <- tt(elem = list(uri = filenames[[1]]), language = "en", id = "id1") 
MyCorpus <- c(VCorpus(VectorSource(rr1))) 

BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))} 
dtm <- DocumentTermMatrix(MyCorpus, control = list(tokenize = BigramTokenizer, 
                                                   weighting = weightTf, wordLengths = c(3, 60), bounds = list(global = c(1,Inf)))) 

data1 <- pdf_text(filenames[1])
data2 <- pdf_text(filenames[2])
data3 <- pdf_text(filenames[3])
data4 <- pdf_text(filenames[4])
data5 <- pdf_text(filenames[5])
data6 <- pdf_text(filenames[6])



#========== EXTRA DATA FROM SPOTIFY =======================


#REF: https://beta.developer.spotify.com/dashboard/applications
# Sign up developer.spotify.com
# API DOCO:https://developer.spotify.com/web-api/
# Create an "app" via the dashboard to create the client ID and client secret

devtools::install_github('charlie86/spotifyr')
install.packages('spotifyr')
library(spotifyr)
# https://github.com/charlie86/spotifyr

# app name "MusicLyricAnalysis1"
Sys.setenv(SPOTIFY_CLIENT_ID = "addtokenhere")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "addtokenhere")

access_token <- get_spotify_access_token()

#Extract data from spotify

spotify_df_U2 <- get_artist_audio_features('U2',access_token)
spotify_df_DaftPunk <- get_artist_audio_features('Daft Punk',access_token)
spotify_df_EltonJohn <- get_artist_audio_features('Elton John',access_token)
spotify_df_LedZeppelin <- get_artist_audio_features('Led Zeppelin',access_token)
spotify_df_KillswitchEngage <- get_artist_audio_features('Killswitch Engage',access_token)
spotify_df_IronMaiden <- get_artist_audio_features('Iron Maiden',access_token)

# other nice functions to try
get_album_popularity()
get_album_tracks()
get_albums()
get_artist_albums()
get_artists()
#spotifyr::