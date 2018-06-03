# R Script: 02_TextAnalysis.R
# Description: 
# Begin analysis of dataset using text mining, NLP and machine learning techniques where appropriate
# Author: Bree McLennan
# Date: 14/05/2018
#
# ======================================================================================================================== #

library(knitr)
library(stringr)
library(lubridate)
library(ggrepel)
library(wordcloud)
library(gridExtra)
library(tidyverse)
library(tidytext)
library(feather)
library(rprojroot)
library(purrr) #reduce and map functions
`%ni%` <- Negate(`%in%`)

# Install songsim (online version)
library(devtools)
devtools::install_github("gsimchoni/songsim")
library(songsim)

# Define some colour themese to use in visualisations
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#ggplot2 custom theme settings
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

titles.format <- theme(plot.title = element_text(face="bold", size=13, color='grey50'),
                       plot.subtitle = element_text(color='grey50'),
                       axis.title = element_text(size=9, color='grey50'), 
                       axis.text = element_text(size=9, color='grey50'),
                       plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"))



# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 

wrk.01_DataPrep_LyricsWithSpotify <- read_feather(F("Data/Processed/wrk.01_DataPrep_LyricsWithSpotify.feather"))

# ======================= MANUAL DATA EXPLORATION ============================================================= #

# ==== PART A: Data Prep. Cleaning dataset with TM package, text pre-processing

# Create a new dataframe with one row of lyrics for each track (instead of multiple rows per verse/chorus)
wrk.02_TextAnalysis_00 <-  wrk.01_DataPrep_LyricsWithSpotify %>% 
  group_by(CATTrackName) %>% 
  mutate(TXTAllTrackLyrics = paste0(text, sep = "<br>", collapse = " ")) %>%
  mutate(NUMMaxLyricLines = max(as.numeric(NUMTrackLyricLineNumber)))

# Create a dataset with one row per song. One variable & record to hold all lyrics for a song.
wrk.02_TextAnalysis_01 <- wrk.02_TextAnalysis_00 %>%
  filter(style_name == "heading 3")

# lets now remove songs which were purely instrumental only
wrk.02_TextAnalysis_02 <- wrk.02_TextAnalysis_01 %>%
  filter(str_detect(TXTAllTrackLyrics, "Instrumental") == FALSE )

#Summary
glimpse(wrk.02_TextAnalysis_02)

#Identify any specific/customisable words we we with to eliminate
undesirable_words <- c("chorus", "lyrics", "verse")

#Create lyric verse tokens
lineToken <- wrk.02_TextAnalysis_02 %>%
  ungroup() %>%
  unnest_tokens(line, TXTAllTrackLyrics, token = stringr::str_split, pattern = '<br>') %>% #break the lyrics into verses
  mutate(lineCount = row_number())

#Create lyric word tokens and apply tidy text format
wordToken <-  lineToken %>% 
  unnest_tokens(word, line) %>%  #Break the lyrics into individual words
  #anti_join(stop_words) %>% #removing stop words
  #distinct() %>%
  filter(!word %in% undesirable_words) #removing custom configured stop words
  #mutate(wordCount = row_number())

# DESCRIPTIVE STATS
# Full word count for each song (non-distinct)
wrk.02_TextAnalysis_03_WordCount <- wordToken %>%
  group_by(CATMusicArtist,CATMusicAlbum, CATTrackName) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

# Common words
#overall (validation)
wrk.02_TextAnalysis_03_CommonWordsAll <- wordToken %>%
  count(word, sort = TRUE)
# Most common words in each song
wrk.02_TextAnalysis_03_CommonWordsSong <- wordToken %>%
  anti_join(stop_words) %>% # remove stop words, allow us to see some more meaningful results
  count(CATTrackName, word, sort = TRUE) %>%
  ungroup()

wrk.02_TextAnalysis_03_CommonWordsGrouped

# Most common words in each album
wrk.02_TextAnalysis_03_CommonWordsAlbum <- wordToken %>%
  anti_join(stop_words) %>% # remove stop words, allow us to see some more meaningful results
  count(CATMusicArtist, CATMusicAlbum, word, sort = TRUE) %>%
  ungroup()


# WORD CLOUDS - for each album
library(wordcloud2)
#==== DAFT PUNK
albums_wordcloud_DaftPunk <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Daft Punk") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_DaftPunk[1:89, ], size = .5) #wont render if rownum set to more than whats in the dataset

#==== U2
albums_wordcloud_U2 <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "U2") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_U2[1:100, ], size = .5)

#==== Elton John
albums_wordcloud_Elton <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Elton John") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_Elton[1:100, ], size = .5)

#==== Led Zeppelin
albums_wordcloud_LedZep <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Led Zeppelin") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_LedZep[1:100, ], size = .5)

#==== Killswitch Engage
albums_wordcloud_KsE <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Killswitch Engage") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_KsE[1:100, ], size = .5)


#==== Iron Maiden
albums_wordcloud_IronMaiden <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  as.data.frame(albums_wordcloud) %>%
  filter(CATMusicArtist == "Iron Maiden") %>%
  select(word, n) 
wordcloud2(albums_wordcloud_IronMaiden[1:100, ], size = .5)



# Find the TF-IDF within the albums
# We expect the albums to differ in terms of subject/topic, content and sentiment, we therefore expect the frequency of words to differ 
# between albums, We can use TF-IDF metric to calculate these differences.
tf_idf <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  bind_tf_idf(word, CATMusicArtist, n) %>%
  arrange(desc(tf_idf))

tf_idf

# Examining the TF-IDF. By Albums as groups
tf_idf %>%
  #filter(str_detect(CATMusicArtist, "^sci\\.")) %>%
  group_by(CATMusicArtist) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = CATMusicArtist)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ CATMusicArtist, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# Which albums are similar to each other in text content? We can explore this by finding
# the pairwise correlation of word frequencies within each newsgroup, using the pairwise_cor()
# function from the widyr package

library(widyr)

album_cors <- wrk.02_TextAnalysis_03_CommonWordsAlbum %>%
  pairwise_cor(CATMusicArtist, word, n, sort = TRUE)

album_cors

#Lets now filter on stronger correlations with the albums and visualise in a network
library(ggraph)
library(igraph)
set.seed(1234)

album_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# It seems these albums don't correlate strongly! Maybe we can try at the song level?

song_cors <- wrk.02_TextAnalysis_03_CommonWordsSong %>%
  pairwise_cor(CATTrackName, word, n, sort = TRUE)

song_cors

set.seed(4321)

song_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# Curious, Elton John's Mona Lisa and Led Zeppelin's Down by the Seaside have a connection with a correlation greater than 0.4.
# Interesting that no other songs had a high enough correlation to be considered as significant.



# Summary table for full word counts. This includes all words, non-distinct.
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function

# Number of songs by artist
wrk.02_TextAnalysis_03_SongPerArtist <- wrk.02_TextAnalysis_01 %>%
  ungroup() %>%
  mutate(BINSongIsInstrumental = as.factor(ifelse(str_detect(TXTAllTrackLyrics, "Instrumental") == TRUE, 1,0 ))) %>%
  mutate(BINSongIsInstrumental = as.numeric(BINSongIsInstrumental)) %>%
  group_by(CATMusicArtist,CATMusicAlbum) %>%
  add_tally() %>% #count the total number of rows in the by group assign to variable "n"
  rename(NUMTotalSongsForArtist = n) %>% # RENAME PARAMETERS (NEW NAME = OLD NAME).
  ungroup() 

#instrumental songs
instrumental <- filter(wrk.02_TextAnalysis_03_SongPerArtist, str_detect(TXTAllTrackLyrics, "Instrumental") == TRUE )

   
# Songs by the highest unique word count
wrk.02_TextAnalysis_03_WordCount %>%
  ungroup(num_words, CATTrackName) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(CATTrackName = color_tile("lightpink","lightpink")(CATTrackName)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)



# ==== PART B: Natural Language Processinh (NLP) - Sentiment Polarity
#we can consider stemming/root words here, incase we want to explore matching words in lexicons
#syuzhet pkg
#Calls the NRC sentiment dictionary to calculate the presence of 
#eight different emotions and their corresponding valence in a text file.


library(syuzhet)
SongSentiment <- get_nrc_sentiment(wrk.02_TextAnalysis_02$TXTAllTrackLyrics)
#SongSentimentValues <- get_nrc_values(wrk.02_TextAnalysis_02$TXTAllTrackLyrics)

wrk.02_TextAnalysis_03 <- bind_cols(wrk.02_TextAnalysis_02, SongSentiment) 

wrk.02_TextAnalysis_03_nrcsentiment <- wrk.02_TextAnalysis_03 %>%
  select(CATMusicArtist, CATMusicAlbum, CATTrackName, anger, anticipation, disgust,
         fear, joy, sadness, surprise , trust ) 

wrk.02_TextAnalysis_03_nrcsentiment_daftpunk <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Daft Punk")

wrk.02_TextAnalysis_03_nrcsentiment_U2 <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "U2")

wrk.02_TextAnalysis_03_nrcsentiment_Elton <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Elton John")

wrk.02_TextAnalysis_03_nrcsentiment_ledzep <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Led Zeppelin")

wrk.02_TextAnalysis_03_nrcsentiment_KsE <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Killswitch Engage")

wrk.02_TextAnalysis_03_nrcsentiment_IronMaiden <- wrk.02_TextAnalysis_03_nrcsentiment %>%
  filter(CATMusicArtist == "Iron Maiden")


#wrk.02_TextAnalysis_03_nrc_sentiment <- wrk.02_TextAnalysis_02 %>%
#  inner_join(get_sentiments("nrc")) %>%
#  filter(!sentiment %in% c("positive", "negative"))

par(mfrow = c(3,2)) # 3 rows and 2 columns
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_daftpunk[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Daft Punk - Discovery", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_U2[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in U2 - Joshua Tree", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_Elton[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Elton John - Honky Chateau", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_ledzep[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Led Zeppelin - Physical Graffiti", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_KsE[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Killswitch Engage - Alive or Just Breathing", xlab="Percentage",
  col = rainbow(8)
)
barplot(
  sort(colSums(prop.table(wrk.02_TextAnalysis_03_nrcsentiment_IronMaiden[, 4:11]))), 
  horiz = TRUE, 
  cex.names = .8, 
  las = 1, 
  main = "Emotions in Iron Maiden - Powerslave", xlab="Percentage",
  col = rainbow(8)
)



# ====== EXPERIMENTAL CODE ========== #
# ------- SONGSIM ... idea for later... zooropa as data art would be really cool...
library(songsim)
library(heatmaply)
# lets test this with kashmir, and then with one more time
SongSim_LedZepKashmir <- wrk.01_DataPrep_LyricsWithSpotify %>%
  filter(CATMusicArtist == "Led Zeppelin" & CATTrackName == "Kashmir") %>%
  filter(style_name == "body") %>%
  select(text )   #CATTrackName NUMTrackLyricLineNumber


fileConn <- file("SongSim_LedZepKashmir.txt")
writeLines(SongSim_LedZepKashmir$text, fileConn)
close(fileConn)

#songsim(path = (F("SongSim_LedZepKashmir.txt")),interactiveMode = TRUE, colorfulMode = TRUE, mainTitle = "Led Zeppelin - Kashmir")
resLedZepKashmir <- songsim(path = (F("Data/Raw/SongSim_LedZepKashmir.txt")),interactiveMode = TRUE, singleColor = "blue", mainTitle = "Led Zeppelin - Kashmir")
# We want to capture the repetition stats for the song, so we'll store it in a variable for now..
str(resLedZepKashmir)
# repetitiveness= two triangles (upper & lower) of the matrix square. Measure is mean (percentage, out of 100%)  of the 1's and 0's across both triangles
resLedZepKashmir$songMat
resLedZepKashmir$repetitiveness


#======= run this for all songs...
library(glue)
SongSim_All_01 <- wrk.01_DataPrep_LyricsWithSpotify %>%
  filter(style_name == "body" & text != "[Instrumental]") %>%
  mutate(CATMusicAlbum = as.factor(CATMusicAlbum),
         CATMusicArtist = as.factor(CATMusicArtist),
         CATTrackName = as.factor(CATTrackName)) %>%
  mutate(CATArtistAlbumTrack = paste(CATMusicArtist, CATMusicAlbum, CATTrackName, sep = " - ")) %>%
  select(CATArtistAlbumTrack, text)

SongList <- tapply(SongSim_All_01$text, SongSim_All_01$CATArtistAlbumTrack, matrix, byrow = TRUE)

lapply(names(SongList),
       function(x, SongList) write.table(SongList[[x]], paste((F("Data/Raw")),"/", x ,".txt", sep = ""),
                                                    col.names = FALSE, row.names=FALSE, sep="\t", 
                                                    quote = FALSE), SongList)


RenderSongSim <- lapply(names(SongList),
                        function(x, SongList) songsim(path = paste((F("Data/Raw")),"/", x ,".txt", sep = ""),
                                                      colorfulMode = TRUE,
                                                      interactiveMode = FALSE, 
                                                      #singleColor = "blue",
                                                      plotOptions = c(width = 4, height = 4),
                                                      mainTitle = x),
                                                      SongList)

#Save the base graphics from songsim to pdf
pdf(paste((F("Data/Processed/SongSim_AllSongs.pdf"))))
  print(
        lapply(names(SongList),
               function(x, SongList) songsim(path = paste((F("Data/Raw")),"/", x ,".txt", sep = ""),
                                             colorfulMode = TRUE,
                                             interactiveMode = FALSE, 
                                             #singleColor = "blue",
                                             plotOptions = c(width = 4, height = 4),
                                             mainTitle = x), SongList)
  )

# Save PDF
dev.off()  

# Convert pdf pages to png
library(pdftools)
#This will save the PNGs to the work dir
pdf_file <- F("Data/Processed/SongSim_AllSongs.pdf")
sapply(pdf_file, function(x)
  pdf_convert(x, format = "png", pages = NULL, filenames = NULL, dpi = 300, opw = "", upw = "", verbose = TRUE))

# Combine the PNG files together to form one rendering
# GO HERE: https://www.filesmerge.com/merge-images , set to 3 columns, PNG or JPG
# THEN OPTIMISE OUTPUT FOR WEB: http://optimizilla.com/



# read in and arrange the png files -- TODO - fix this bit
#library(png)
#filenames <- dir(path = F("Data/Processed/"), pattern = "SongSim_AllSongs_", full.names = TRUE)
#PNGList <- list()
#for (j in 1:59) PNGList[[j]] <- readPNG(filenames[j]) #might be looking at working dir...we moved the files



#===================

## PART A: LYRICAL ANALYSIS
# What do the songs say?
wrk.02_TextAnalysis_03_CommonWordsSong


#plot setup
uniqueWords <- wrk.02_TextAnalysis_03_CommonWordsSong %>% 
  select(CATTrackName, word) %>%
  filter(!str_detect(word, '[0-9]')) %>% #or we could transmute digits to words...
  group_by(CATTrackName, word) %>% 
  filter(row_number(word) == 1) %>%
  arrange(word)
nbreUniqWords <- nrow(uniqueWords)


## BI GRAMS & TRI GRAMS
nGram <- data_frame(text=paste(wrk.02_TextAnalysis_03_CommonWordsSong$word, collapse = ' '))
nGramCleaned <- data_frame(text=paste(wrk.02_TextAnalysis_03_CommonWordsSong$word, collapse = ' '))

biGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

biGramsCleaned <-  nGramCleaned %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

triGrams <-  nGram %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
  count(ngram, sort = TRUE)

b.G <- biGrams %>%
  mutate(ngram = reorder(ngram, n)) %>%
  slice(1:20) %>%
  ggplot(., aes(x=ngram, y=n)) + 
  geom_bar(stat = "identity", 
           color='white',
           fill='#FCCB85') + 
  xlab('bi-grams') +
  #       ylab('Number of Press Clippings') +
  coord_flip() +
  labs(title='Most Frequent bi-Grams') +
  titles.format

t.G <- triGrams %>%
  mutate(ngram = reorder(ngram, n)) %>%
  slice(1:20) %>%
  ggplot(., aes(x=ngram, y=n)) + 
  geom_bar(stat = "identity", 
           color='white',
           fill='#FCCB85') + 
  xlab('tri-grams') +
  #       ylab('Number of Press Clippings') +
  coord_flip() +
  labs(title='Most Frequent tri-Grams') +
  titles.format
grid.arrange(b.G, t.G, ncol=2, widths=c(1,1.2))


# SENTIMENTS FEELS...
bing <- get_sentiments('bing')
sentLyricsWord <- wordToken %>%
  inner_join(bing)

bing <- get_sentiments('bing')
sentLyricsWord <- wordToken %>%
  inner_join(bing)

mostSentWord <- sentLyricsWord %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

ggplot(mostSentWord, aes(word, n, fill = n > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title='Most Frequent Positive And Negative Sentiment Words',
       subtitle='In the 14 studio albums overall',
       y='Frequency',
       x=NULL) +
  coord_flip() +
  titles.format



# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #
