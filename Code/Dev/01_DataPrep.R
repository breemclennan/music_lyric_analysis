# R Script: 01_DataPrep.R
# Description: 
# 1. Load in data created by 00_DataLoad.R
# 2. datatype conversions, rename variables to data science convention: KEY, FOR, TXT, NUM, BIN, ORD, CAT, DAT, TIM
# 3. data integrity checks
# 4. Add in features
# 5. Apply risk treatments & controls: DATA SENSITIVITY AND SECURITY
# 6. Consider applying centering, scaling, if appropriate
# Author: Bree McLennan
# Date: 03/01/2018
#
# ======================================================================================================================== #

# Load packages
library(data.table)
library(feather)
library(dplyr)
library(forcats)
library(lubridate)
library(glue)
library(DescTools)
library(RDCOMClient)
library(stringr)
library(rprojroot)
library(purrr) #reduce and map functions
`%ni%` <- Negate(`%in%`)

# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 

# Load feather data
wrk.data <- setDT(read_feather(glue(F("Data/Raw/raw.AllMusicLyrics.feather"))))

# 1. Data Type configurations ==============================================================================================#

# USEFUL THINGS!
# IF text contains "[Instrumental]" , flag track as instrumental

wrk.01_Data_Prep <- wrk.data %>%
  mutate(BINTrackIsInstrumental = ifelse(style_name == "body" & trimws(text) == "[Instrumental]", 1, 0)) %>%
  mutate(KEYTrackName = toupper(trimws(CATTrackName)))
         
      

#========== EXTRA DATA FROM SPOTIFY =======================
#REF: https://beta.developer.spotify.com/dashboard/applications
# Sign up developer.spotify.com
# API DOCO:https://developer.spotify.com/web-api/
# Create an "app" via the dashboard to create the client ID and client secret

#devtools::install_github('charlie86/spotifyr')
#install.packages('spotifyr')
library(spotifyr)
# https://github.com/charlie86/spotifyr

# app name "MusicLyricAnalysis1"
Sys.setenv(SPOTIFY_CLIENT_ID = "ADD TOKEN HERE")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "ADD TOKEN HERE")

access_token <- get_spotify_access_token()

#Extract data from spotify
spotify_df_U2 <- get_artist_audio_features('U2',access_token)
spotify_df_DaftPunk <- get_artist_audio_features('Daft Punk',access_token)
spotify_df_EltonJohn <- get_artist_audio_features('Elton John',access_token)
spotify_df_LedZeppelin <- get_artist_audio_features('Led Zeppelin',access_token)
spotify_df_KillswitchEngage <- get_artist_audio_features('Killswitch Engage',access_token)
spotify_df_IronMaiden <- get_artist_audio_features('Iron Maiden',access_token)

spotify_U2_filtered <- filter(spotify_df_U2, album_name == "The Joshua Tree (Deluxe)") #we got extra live songs
spotify_DaftPunk_filtered <- filter(spotify_df_DaftPunk, album_name == "Discovery")
spotify_EltonJohn_filtered <- filter(spotify_df_EltonJohn, album_name == "Honky Chateau") #no results
spotify_LedZeppelin_filtered <- filter(spotify_df_LedZeppelin, album_name == "Physical Graffiti")
spotify_KillswitchEngage_filtered <- filter(spotify_df_KillswitchEngage, album_name == "Alive or Just Breathing [Topshelf Edition]")
spotify_IronMaiden_filtered <- filter(spotify_df_IronMaiden, album_name == "Powerslave (1998 Remastered Edition)")

# We can keep these datasets in data frame format for now, dataset size is tiny.
raw.SpotifyArtistList <- list(spotify_U2_filtered, spotify_DaftPunk_filtered, spotify_EltonJohn_filtered,
                              spotify_LedZeppelin_filtered, spotify_KillswitchEngage_filtered, spotify_IronMaiden_filtered)
# Append all above raw datasets together
raw.SpotifyArtistAlbumTrackData <- rbindlist(raw.SpotifyArtistList) %>%
  mutate(KEYTrackName = toupper(trimws(track_name)))


# Save Feather file from 
write_feather(raw.SpotifyArtistAlbumTrackData, F("Data/Raw/raw.SpotifyArtistAlbumTrackData.feather"))

# Read feather file if we've restarted sessions
raw.SpotifyArtistAlbumTrackData <- read_feather(F("Data/Raw/raw.SpotifyArtistAlbumTrackData.feather"))

#======================================#
# Left join our reference spotify data  onto our lyrics dataset
#======================================#

wrk.01_DataPrep_LyricsWithSpotify <- list(wrk.01_Data_Prep, raw.SpotifyArtistAlbumTrackData) %>%
  reduce(left_join, by = c("KEYTrackName" = "KEYTrackName"))

# Save Feather file from 
write_feather(wrk.01_DataPrep_LyricsWithSpotify, F("Data/Processed/wrk.01_DataPrep_LyricsWithSpotify.feather"))


# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #



