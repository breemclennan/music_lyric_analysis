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
library(quanteda) #Recommended
library(officer) # for reading docx
library(dplyr)
library(zoo) #for row fill down function track names

# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 
# Example usage: F("Data/Raw") 

# Get a List of all files named with a key word, use regex pattern to identify only the athletics graded results csv files
#FilenamesPDF <- list.files(F("Data/Raw") , pattern = "*.pdf", full.names = TRUE)
FilenameDocx <- list.files(F("Data/Raw") , pattern = "*.docx", full.names = TRUE)


# Working with word documents:
# Read in each file and use doc summary to map styles used in document as music lyric components
# Then apply some common hirarchial groupings: Artist, Album, track name and grouping

doc01 <- read_docx(F("Data/Raw/Daft Punk - Discovery.docx"))
raw.data01 <- docx_summary(doc01) %>%
  mutate(CATMusicArtist = "Daft Punk",
         CATMusicAlbum = "Discovery",
         CATTrackName = ifelse(style_name == "heading 3", text, "None"),
         CATTrackName = na.locf(CATTrackName), #fill down track number
         style_name = ifelse(is.na(style_name), "body", style_name)
         ) %>%
  group_by(CATTrackName) %>%
  mutate(NUMTrackLyricLineNumber = sequence(n()) - 1) #using minus 1 so we dont include the heading


doc02 <- read_docx(F("Data/Raw/U2 - The Joshua Tree.docx"))
raw.data02 <- docx_summary(doc02) %>%
  mutate(CATMusicArtist = "U2",
         CATMusicAlbum = "The Joshua Tree",
         CATTrackName = ifelse(style_name == "heading 3", text, "None"),
         CATTrackName = na.locf(CATTrackName), #fill down track number
         style_name = ifelse(is.na(style_name), "body", style_name)
  ) %>%
  group_by(CATTrackName) %>%
  mutate(NUMTrackLyricLineNumber = sequence(n()) - 1) #using minus 1 so we dont include the heading
  
doc03 <- read_docx(F("Data/Raw/Elton John - Honky Chateau.docx"))
raw.data03 <- docx_summary(doc03) %>%
  mutate(CATMusicArtist = "Elton John",
         CATMusicAlbum = "Honky Chateau",
         CATTrackName = ifelse(style_name == "heading 3", text, "None"),
         CATTrackName = na.locf(CATTrackName), #fill down track number
         style_name = ifelse(is.na(style_name), "body", style_name)
  ) %>%
  group_by(CATTrackName) %>%
  mutate(NUMTrackLyricLineNumber = sequence(n()) - 1) #using minus 1 so we dont include the heading


doc04 <- read_docx(F("Data/Raw/Iron Maiden - Powerslave.docx"))
raw.data04 <- docx_summary(doc04) %>%
  mutate(CATMusicArtist = "Iron Maiden",
         CATMusicAlbum = "Powerslave",
         CATTrackName = ifelse(style_name == "heading 3", text, "None"),
         CATTrackName = na.locf(CATTrackName), #fill down track number
         style_name = ifelse(is.na(style_name), "body", style_name)
  ) %>%
  group_by(CATTrackName) %>%
  mutate(NUMTrackLyricLineNumber = sequence(n()) - 1) #using minus 1 so we dont include the heading
  
  
doc05 <- read_docx(F("Data/Raw/Killswitch Engage - Alive or Just Breathing.docx"))
raw.data05 <- docx_summary(doc05) %>%
  mutate(CATMusicArtist = "Killswitch Engage",
         CATMusicAlbum = "Alive or Just Breathing",
         CATTrackName = ifelse(style_name == "heading 3", text, "None"),
         CATTrackName = na.locf(CATTrackName), #fill down track number
         style_name = ifelse(is.na(style_name), "body", style_name)
  ) %>%
  group_by(CATTrackName) %>%
  mutate(NUMTrackLyricLineNumber = sequence(n()) - 1) #using minus 1 so we dont include the heading
  
  
doc06 <- read_docx(F("Data/Raw/Led Zeppelin - Physical Graffiti.docx"))
raw.data06 <- docx_summary(doc06) %>%
  mutate(CATMusicArtist = "Led Zeppelin",
         CATMusicAlbum = "Physical Graffiti",
         CATTrackName = ifelse(style_name == "heading 3", text, "None"),  #set the track name based on heading 3
         CATTrackName = na.locf(CATTrackName), #fill down track number
         style_name = ifelse(is.na(style_name), "body", style_name)
  ) %>%
  group_by(CATTrackName) %>%
  mutate(NUMTrackLyricLineNumber = sequence(n()) - 1) #using minus 1 so we dont include the heading
  

# We can keep these datasets in data frame format for now, dataset size is tiny.
raw.dsetnames <- list(raw.data01, raw.data02, raw.data03, raw.data04, raw.data05, raw.data06)
# Append all above raw datasets together
raw.data <- rbindlist(raw.dsetnames)

# Save Feather file from 
write_feather(raw.data, F("Data/Raw/raw.AllMusicLyrics.feather"))

# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #

