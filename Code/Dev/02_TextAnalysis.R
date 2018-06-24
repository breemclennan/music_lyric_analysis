# R Script: 02_TextAnalysis.R
# Description: 
# Begin analysis of dataset using text mining, NLP and machine learning techniques where appropriate
# Author: Bree McLennan
# Date: 14/05/2018
#
# ======================================================================================================================== #

library(dplyr)
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
library(qdap)
`%ni%` <- Negate(`%in%`)

# Install songsim (online version)
#library(devtools)
#devtools::install_github("gsimchoni/songsim")
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
  # QDAP Remove stopwords
  #rm_stopwords(text$Text, Top200Words, strip = TRUE, separate = FALSE)
  #anti_join(stop_words) %>% #TM/tidytext removing stop words
  #distinct() %>%
  filter(!word %in% undesirable_words) #removing custom configured stop words
  #mutate(wordCount = row_number())

# DESCRIPTIVE STATS
# Full word count for each song (non-distinct)
wrk.02_TextAnalysis_03_WordCount <- wordToken %>%
  group_by(CATMusicArtist,CATMusicAlbum, CATTrackName) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

#TOTAL WORD COUNT: USING QDAP
wrk.02_TextAnalysis_03_QDAPWordCount <- with(wrk.02_TextAnalysis_02, qdap::gantt_rep(rm.var = CATMusicArtist, text.var = TXTAllTrackLyrics, grouping.var = CATTrackName ))
#gantt_wrap(plot(wrk.02_TextAnalysis_03_QDAPWordCount))
gantt_wrap(wrk.02_TextAnalysis_03_QDAPWordCount,
           plot.var = "CATMusicArtist", fill.var = "CATTrackName",
           border.color = "black",
           legend.position = "bottom",
           major.line.freq = 250,
           ncol = 2,
           size = 7,
           border.size = 4,
           border.width = .4,
           title = "Total word count for each artist & album") 


# Common words
#overall (validation)
wrk.02_TextAnalysis_03_CommonWordsAll <- wordToken %>%
  count(word, sort = TRUE)
# Most common words in each song
wrk.02_TextAnalysis_03_CommonWordsSong <- wordToken %>%
  anti_join(stop_words) %>% # remove stop words, allow us to see some more meaningful results
  count(CATTrackName, word, sort = TRUE) %>%
  ungroup()

wrk.02_TextAnalysis_03_CommonWordsSong


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
wordcloud2(albums_wordcloud_DaftPunk[1:88, ], size = .5) #wont render if rownum set to more than whats in the dataset

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

#===== LEXICAL DENSITY
# Let's observe the lexical diversity, or vocabulary, of each of the song lyrics.
# Assumption: the more diverse the lyrics, the larger the vocabulary
# Using a continuous dependent variable "word_count", as a function of the categorical independent variable, album.
LexicalDensity_01 <- wordToken %>%
  anti_join(stop_words) %>% 
  group_by(CATMusicArtist, CATMusicAlbum, CATTrackName) %>%
  mutate(CATArtistAlbumTrack = paste(CATMusicArtist, CATMusicAlbum, CATTrackName, sep = " - ")) %>%
  mutate(CATArtistAlbum = paste(CATMusicArtist, CATMusicAlbum, sep = " - ")) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(CATMusicArtist, CATMusicAlbum, CATTrackName, CATArtistAlbum, CATArtistAlbumTrack, word_count) %>%
  distinct() %>% #Summarise to one record per song, distinct word count
  ungroup()

library(yarrr)
#Lets draw the plot
par(mar = c(5.1, 4.1, 4.1, 2.1)) # Set the margin on all sides to 2
par(mfrow = c(1, 1))
pirateplot(formula =  word_count ~ CATMusicArtist, #Formula
           data = LexicalDensity_01, #Data frame
           xlab = NULL, ylab = "Song Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Artist & Album", #Plot title
           pal = "espresso", #Color scheme
           back.col = gray(.98), # Add light gray background
           gl.col = "gray", # Gray gridlines
           gl.lwd = c(.75, 0),
           inf.f.o = .6, # Turn up inf filling
           inf.disp = "rect", # Wrap inference around bean
           bean.b.o = .4, # Turn down bean borders
           quant = c(.25, .75), # 25th and 75th quantiles
           quant.col = "black", # Black quantile lines
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 2, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = 1, cex.names = .9) #Axis label size
#dev.off()

# TF IDF ===================================================================
#TF is "term frequency". IDF is "inverse document frequency",
# which attaches a lower weight for commonly used words and a higher weight for 
# words that are not used much in a collection of text. When you combine TF and IDF,
# a term's importance is adjusted for how rarely it is used. The assumption behind TF-IDF 
# is that terms that appear more frequently in a document should be given a higher weight,
# unless it also appears in many documents. The formula can be summarized below:
# - Term Frequency (TF): Number of times a term occurs in a document
# - Document Frequency (DF): Number of documents that contain each word
# - Inverse Document Frequency (IDF) = 1/DF
# - TF-IDF = TF * IDF
#The IDF of any term is therefore a higher number for words that occur in fewer of the documents in the collection. 

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
  ylab("Term Frequency - Inverse Document Frequency (TF-IDF)") +
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

instrumental %>%
  select(CATMusicArtist, CATMusicAlbum, CATTrackName) %>%
  kable("html", escape = FALSE, align = "c", caption = "Instrumental Songs") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), full_width = FALSE)

   
# Songs by the highest unique word count
wrk.02_TextAnalysis_03_WordCount %>%
  ungroup(num_words, CATTrackName) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(CATTrackName = color_tile("lightpink","lightpink")(CATTrackName)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Total Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)



# ==== PART B: Natural Language Processinh (NLP) - Sentiment Polarity
# Do we need to perform more Data Preparation?
# It may be the case that you need a few more data preparation steps. Here are three techniques to consider before performing sentiment analysis:
# Stemming: generally refers to removing suffixes from words to get the common origin
# Lemmatization: reducing inflected (or sometimes derived) words to their word stem, base or root form
#Word replacement: replace words with more frequently used synonyms
#An advanced concept in sentiment analysis is that of synonym (semantically similar peer)
#and hypernym (a common parent) replacement. These are words that are more frequently used than
#the related word in the lyric, and actually do appear in a lexicon, thus giving a higher match percentage. 
#There is not enough space in this tutorial to address additional data preparation, 
#but it's definitely something to consider!
#Challenge: do a little research on lexicons and how they are created. Is there already one that exists that is better suited to musical lyrics? If you're really interested, maybe consider what it would take to build your own lexicon. 
#What is the difference between classifier-based sentiment analysis and lexicon-based sentiment analysis?
#==================
#we can consider stemming/root words here, incase we want to explore matching words in lexicons
#syuzhet pkg
#Calls the NRC sentiment dictionary to calculate the presence of 
#eight different emotions and their corresponding valence in a text file.

#METHOD 1: USING TIDY TEXT
SongSentiment_TidyText_NRC <- wordToken %>%
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(CATMusicArtist ,sentiment) %>%
  count(word, CATMusicArtist, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(8)) %>% #consider top_n() from dplyr also
  ungroup()


#PLOT NRC SENTIMENT BY ARTST & ALBUM (ALL WORDS)
par(mar = c(5.1, 4.1, 4.1, 2.1)) # Set the margin on all sides to 2
par(mfrow = c(1, 1))
SongSentiment_TidyText_NRC %>%
  #Set `y = 1` to just plot one variable and use word as the label
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  #You want the words, not the points
  geom_point(color = "transparent") +
  #Make sure the labels don't overlap
  geom_label_repel(force = 1,nudge_y = .5, nudge_x = .5,  
                   direction = "both",
                   box.padding = 0.04,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(CATMusicArtist~sentiment) +
  theme_lyrics() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("NRC Sentiment by Artist & Album") +
  coord_flip()

#SongSentiment_TidyText_NRC
wordToken %>%
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(CATMusicArtist ,sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  facet_wrap(~CATMusicArtist) +
  geom_col() +
  guides(fill = FALSE) +
  theme_lyrics() +
  #scale_color_manual(values=c("#000000","#fca002","#3dbf1c","#0347bc","#23c0ff","#c40e01","#609105","#a25ce8"))+
  labs(x = "NRC Sentiment", y = "Word Count") +
  ggtitle("NRC Sentiment by Artist & Album") +
  coord_flip()

#METHOD 2: USING SYUZHET
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


#======= BUILD SONG SIMs FOR ALL SONGS IN DATASET (59 songs)
# We can use this as an alternative to measuring lexical density (the number of unique words
# divided by the total number of words - used as a measure for indication of word repeition).
# General assumption: As lexical density increases, repetition ought to decrease.

library(songsim)
library(heatmaply)
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
#This will save the PNGs to the work dir, will need to manually move these to correct dir.
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


# What is the repetition level for the songs?
distinctSongList <- SongSim_All_01 %>%
  group_by(CATArtistAlbumTrack) %>%
  distinct(CATArtistAlbumTrack) %>%
  ungroup()
distinctSongList <- as.data.frame(distinctSongList)


#Unpack the songsim big list
length(RenderSongSim)
str(RenderSongSim[[1]])
SongSim_Repetition <- RenderSongSim %>% map_dbl("repetitiveness")
df <- as.data.frame(SongSim_Repetition)

#Dataset with repetitiveness measure and the artist+album+track column
Song_Repetition_DF <- data.frame(bind_cols(distinctSongList, df))
Song_Repetition_DF <- Song_Repetition_DF %>%
  mutate(CATArtistAlbumTrack_split = CATArtistAlbumTrack) %>%
  mutate(SongSim_Repetition = as.numeric(SongSim_Repetition)) %>%
  separate(CATArtistAlbumTrack_split, c("CATMusicArtist", "CATMusicAlbum", "CATTrackName"), " - ", extra = "merge")



library(yarrr)
pirateplot(formula =  SongSim_Repetition ~ CATMusicArtist, #Formula
           data = Song_Repetition_DF, #Data frame
           xlab = "All Songs", ylab = "Lyrical Repetitiveness (Song Sim)", #Axis labels
           main = "SongSim Repetitiveness for each song by Artist", #Plot title
           pal = "espresso", #Color scheme
           back.col = gray(.98), # Add light gray background
           gl.col = "gray", # Gray gridlines
           gl.lwd = c(.75, 0),
           inf.f.o = .6, # Turn up inf filling
           inf.disp = "rect", # Wrap inference around bean
           bean.b.o = .4, # Turn down bean borders
           quant = c(.25, .75), # 25th and 75th quantiles
           quant.col = "black", # Black quantile lines
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 2, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = 1, cex.names = .9) #Axis label size

# === ven diagram for Albums and Song dendogram
VennDiagram <- with(wrk.02_TextAnalysis_02 , qdap::trans_venn(text.var = text, grouping.var = CATMusicArtist, title.name="Album lyric similarity", legend.location = "topright"))

VennDiagram_song <- with(wrk.02_TextAnalysis_02 , qdap::trans_venn(text.var = text, grouping.var = CATTrackName, title.name="Song lyric similarity", legend.location = "topright"))

#Dissimilarity statistics: Uses the distance function to calculate dissimilarity statistics by grouping variables.
#Returns a matrix of dissimilarity values (the agreement between text).
SongDendogram <- with(wrk.02_TextAnalysis_02,  qdap::Dissimilarity(text.var = text, grouping.var = list(CATMusicArtist, CATTrackName), method = "prop",
              diag = FALSE, upper = FALSE, p = 2))
fit <- hclust(SongDendogram)
plot(fit)
## draw dendrogram with red borders around the 3 clusters
rect.hclust(fit, k=14, border=c("red", "blue", "green", "purple", "orange", "brown", "seagreen") )

## Clustering: Dendrogram with p.values
library(pvclust)
wfm.mod <- with(wrk.02_TextAnalysis_02, wfm(text, list(CATMusicArtist, CATTrackName)))
fit2 <- suppressMessages(pvclust(wfm.mod, method.hclust="ward",
                                method.dist="euclidean"))
plot(fit2)
pvrect(fit2, alpha=.95)



# ============= BI grams and TRI grams

# TODO: GET THIS WORKING FOR By_Group artist, album, song + word
#wordToken <-  lineToken %>% 
  BiGrams <- lineToken %>%
  unnest_tokens(ngram, line, token = "ngrams", n = 2) %>%  #Break the lyrics into individual words
  anti_join(stop_words) %>% #removing stop words
  filter(!ngram %in% undesirable_words) %>%
  group_by(CATMusicArtist,CATMusicAlbum, CATTrackName) %>%
  dplyr::count(ngram, sort = TRUE) %>%
  ungroup()

  TriGrams <- lineToken %>%
  unnest_tokens(ngram, line, token = "ngrams", n = 3) %>%  #Break the lyrics into individual words
  anti_join(stop_words) %>% #removing stop words
  filter(!ngram %in% undesirable_words) %>%
  group_by(CATMusicArtist,CATMusicAlbum, CATTrackName) %>%
  count(ngram, sort = TRUE) 

library(ggplot2)
b.G <- BiGrams %>%
  group_by(CATMusicArtist) %>%
  mutate(ngram = reorder(ngram, n, sum)) %>%
  slice(1:10) %>%
  ungroup() %>%
  #to reorder the x axis by sum of n, add in reorder() to aes(x= ..)
  ggplot(aes(x = reorder(ngram, n, sum), n, fill = CATMusicArtist)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ CATMusicArtist, scales = "free") +
  ylab("Most Frequent bi-Grams") +
  xlab('Bi-grams') +
  coord_flip() 
  
b.G 

t.G <- TriGrams %>%
  group_by(CATMusicArtist) %>%
  mutate(ngram = reorder(ngram, n, sum)) %>%
  slice(1:10) %>%
  ungroup() %>%
  #to reorder the x axis by sum of n, add in reorder() to aes(x= ..)
  ggplot(aes(reorder(ngram, n, sum), n, fill = CATMusicArtist)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ CATMusicArtist, scales = "free") +
  ylab("Most Frequent Tri-Grams") +
  xlab('Tri-grams') +
  coord_flip() 

t.G 

# TM / QUANTEDA > TIDYTEXT INTELLIGENT NGRAM APPROACH
library(tm)
library(quanteda)
library(tidytext)

#Setup the dataset for ngram analysis. We don't want to keep the song title in the lyric sheet.
#REF for text cleanup:http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html
# Stopword definitions
custom_stop_words <- c("chorus", "verse", "lyrics", tm::stopwords("en"))


Ngrams_Dset_00 <- wrk.01_DataPrep_LyricsWithSpotify %>%
  group_by(CATTrackName) %>%
  filter(style_name == "body") %>%
  mutate(TXTAllTrackLyrics = paste0(text, sep = ".", collapse = " ")) %>%
  mutate(NUMMaxLyricLines = max(as.numeric(NUMTrackLyricLineNumber))) %>%
  mutate(TXTAllTrackLyrics = tolower(TXTAllTrackLyrics)) %>% #make all text lower case
  mutate(TXTAllTrackLyrics = removePunctuation(TXTAllTrackLyrics)) %>% #Remove all punctuation marks
  #mutate(TXTAllTrackLyrics = removeNumbers(TXTAllTrackLyrics)) %>% #Remove numbers
  mutate(TXTAllTrackLyrics = stripWhitespace(TXTAllTrackLyrics)) %>% #Remove excess whitespace
  mutate(TXTAllTrackLyrics = qdap::bracketX(TXTAllTrackLyrics)) %>% #remove all text within brackets (e.g. “It’s (so) cool” becomes “It’s cool”)
  mutate(TXTAllTrackLyrics = qdap::replace_number(TXTAllTrackLyrics)) %>%  #Replace numbers with their word equivalents (e.g. “2” becomes “two”)
  mutate(TXTAllTrackLyrics = qdap::replace_abbreviation(TXTAllTrackLyrics)) %>% #Replace abbreviations with their full text equivalents (e.g. “Sr” becomes “Senior”)
  mutate(TXTAllTrackLyrics = qdap::replace_contraction(TXTAllTrackLyrics)) %>% #Convert contractions back to their base words (e.g. “shouldn’t” becomes “should not”)
  mutate(TXTAllTrackLyrics = qdap::replace_symbol(TXTAllTrackLyrics)) %>% #Replace common symbols with their word equivalents (e.g. “$” becomes “dollar”)
  mutate(TXTAllTrackLyrics = tm::removeWords(TXTAllTrackLyrics, custom_stop_words))  #Remove stop words
 #TODO see reference and add in stemming if appropriate


#Filter out instrumental songs & keep only variables we need.
Ngrams_Dset_01 <- Ngrams_Dset_00 %>%
  filter(str_detect(TXTAllTrackLyrics, "Instrumental") == FALSE ) %>%
  filter(row_number() <= 1) %>% # Select the first record for each song. This contains all lyrics for the song.
  ungroup() %>%
  # setup our primary key, record id counter
  mutate(lineCount = row_number()) %>% 
  select(lineCount, TXTAllTrackLyrics, CATMusicArtist, CATMusicAlbum, CATTrackName ) %>%
  rename(doc_id = lineCount) %>%
  rename(text = TXTAllTrackLyrics) %>%
  mutate() #for later remerging with DTM results

Ngram_DTM_Merge <- Ngrams_Dset_01 %>%
  mutate(ID = doc_id) %>%
  select(ID, CATMusicArtist, CATMusicAlbum, CATTrackName)

#Create the tm VCorpus so RWeka ngram will work correctly
TM_Corpus <- VCorpus(DataframeSource(Ngrams_Dset_01))

# Manually keep ID information for Corpus from https://stackoverflow.com/a/14852502/1036500
for (i in 1:length(TM_Corpus)) {
  attr(TM_Corpus[[i]], "ID") <- Ngrams_Dset_01$doc_id[i]
}

#Ngram tokeniser fucntion setup ======
library(RWeka)
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}

#Clean up the corpus [USING THE TM AND QDAP text clean functions in dataframe setup. TM_MAP is removing doc_id attributes]
#rmvNonEngGsub <- function(x) { gsub(pattern="[^[:alpha:]]", " ", x) }  # remove letters that are not A-Z or num
#TM_Corpus <- tm_map(TM_Corpus, rmvNonEngGsub)
#TM_map functions are removing the doc_id attributes
#MyCorpus <- tm_map(TM_Corpus, content_transformer(tolower)) 
#TM_Corpus <- tm_map(TM_Corpus, removeNumbers) 
#TM_Corpus <- tm_map(TM_Corpus, removePunctuation) 
#TM_Corpus <- tm_map(TM_Corpus, removeWords, stopwords('english')) 
#TM_Corpus <- tm_map(TM_Corpus, removeWords, stopwords(source = "smart")) 
#TM_Corpus <- tm_map(TM_Corpus, stemDocument, language = "english") 
#TM_Corpus <- tm_map(TM_Corpus, PlainTextDocument) 

#BIGRAM DOCUMENT TERM MATRIX
dtm_bigram <- DocumentTermMatrix(TM_Corpus, control = list(tokenize = BigramTokenizer, wordLengths = c(3,30)))
inspect(dtm_bigram)

#TRIGRAM DOCUMENT TERM MATRIX
dtm_trigram <- DocumentTermMatrix(TM_Corpus, control = list(tokenize = TrigramTokenizer, wordLengths = c(3,30)))
inspect(dtm_trigram)

#Merge the DTM back onto the original dataframe by doc_id
dtm_bigram_df <- data.frame(as.matrix(dtm_bigram))
dtm_trigram_df <- data.frame(as.matrix(dtm_trigram))

# merge by row.names from https://stackoverflow.com/a/7739757/1036500
dtm_bigram_df_merged <- base::merge(Ngram_DTM_Merge, dtm_bigram_df, by.x = "ID", by.y = "row.names" )
head(dtm_bigram_df_merged)

dtm_trigram_df_merged <- base::merge(Ngram_DTM_Merge, dtm_trigram_df, by.x = "ID", by.y = "row.names" )
head(dtm_trigram_df_merged)


#Gather the dataset (transpose wide to long)
dtm_bigram_df_final <- dtm_bigram_df_merged  %>%
  gather(BiGram, Value, -ID, -CATMusicArtist, -CATMusicAlbum, -CATTrackName) %>%
  group_by(CATMusicArtist) %>%
  arrange(desc(Value)) %>%
  ungroup()

str(dtm_bigram_df_final)

dtm_trigram_df_final <- dtm_trigram_df_merged  %>%
  gather(TriGram, Value, -ID, -CATMusicArtist, -CATMusicAlbum, -CATTrackName) %>%
  group_by(CATMusicArtist) %>%
  arrange(desc(Value)) %>%
  ungroup()

str(dtm_trigram_df_final)


Bigrams <- dtm_bigram_df_final %>%
  group_by(CATMusicArtist) %>%
  slice(1:10) %>%
  ungroup() %>%
  #to reorder the x axis by sum of n, add in reorder() to aes(x= ..)
  ggplot(aes(x = reorder(BiGram, Value, sum), Value, fill = CATMusicArtist)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ CATMusicArtist, scales = "free") +
  ylab("Most Frequent bi-Grams") +
  xlab('Bi-grams') +
  coord_flip() 

Bigrams

Trigrams <- dtm_trigram_df_final %>%
  group_by(CATMusicArtist) %>%
  slice(1:10) %>%
  ungroup() %>%
  #to reorder the x axis by sum of n, add in reorder() to aes(x= ..)
  ggplot(aes(x = reorder(TriGram, Value, sum), Value, fill = CATMusicArtist)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ CATMusicArtist, scales = "free") +
  ylab("Most Frequent Tri-Grams") +
  xlab('Tri-grams') +
  coord_flip() 

Trigrams



#========== BI-GRAM NETWORK

centre_words <- c("hey","feel","gonna","people","yeah","love","light","time", "life")

#TM ngram method
bigrams_separated_TM <- dtm_bigram_df_final %>%
  mutate(BiGram = gsub(".", " ", BiGram, fixed = TRUE)) %>%
  separate(BiGram, c("word1", "word2"), sep = " ")

#tidytext ngram method
bigrams_separated <- BiGrams %>%
  separate(ngram, c("word1", "word2"), sep = " ")

library(tidytext)
library(ggraph)
library(igraph)
centre_bigrams <- bigrams_separated_TM %>% #using TM ngram method (Value = n)
  select(word1, word2, Value) %>%
  filter(word1 %in% centre_words) %>%
  #inner_join(AFINN, by = c(word2 = "word")) %>%
  #count(word1, word2, sort = TRUE) %>%
  #mutate(contribution = n * score) %>%
  #arrange(desc(abs(contribution))) %>%
  group_by(word1) %>%
  slice(seq_len(20)) %>%
  #arrange(word1,desc(contribution)) %>%
  ungroup()

bigram_graph <- centre_bigrams %>%
  graph_from_data_frame() #From `igraph`


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.10, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void()

#======= TIDY TEXT STM TOPIC MODELLING ============================

tidy_MusicLyrics <- lineToken %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) #%>%

  #filter(word != "")

tidy_MusicLyrics %>% #check the word list and identify any further words to filter out above.
  count(word, sort = TRUE)

# What are the highest tf-idf words across these 6 music albums? The td-idf statistic identifies words that are
# important to a document in a collection of documents [in our case, Music Albums]. We'll see which words are important
# in one music album, compared to the others.
# how important is this word/song lyric to this document/music track & album, compared to other words in the collection of albums?

library(drlib)

MusicLyrics_tf_idf <- tidy_MusicLyrics %>%
  count(CATMusicArtist, word, sort = TRUE) %>%
  bind_tf_idf(word, CATMusicArtist, n) %>%
  arrange(-tf_idf) %>%
  group_by(CATMusicArtist) %>%
  top_n(10) %>% #the top 10 highest td-idf words
  ungroup()

MusicLyrics_tf_idf %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = CATMusicArtist)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ CATMusicArtist, scales = "free", ncol = 3) +
  #scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in collection of Music Albums",
       subtitle = "Individual songs focus on different themes and narrative elements")

#We see lots of familiar lyric words here.
#as well as specific narrative elements for individual songs, like faster-harder-stronger,
# rocket, minutes-midnight, numbered-days, kashmir, red-hill-mining-town.
#🐦 Exploring tf-idf can be helpful before training topic models.


# TOPIC MODEL (Structured Topic Model, STM)
# inspiration reference: https://juliasilge.com/blog/sherlock-holmes-stm/

#The stm() function take as its input a document-term matrix,
#either as a sparse matrix or a dfm from quanteda.

library(quanteda)
library(stm)

#pre-process the text. This involves removing stop words (common words as well as user specified stop words), stemming words (to remove suffixes), and other basic steps to make the text ready for processing.
tidy_stemming_MusicLyrics <- textProcessor(tidy_MusicLyrics$word, meta = tidy_MusicLyrics, customstopwords = undesirable_words,
                                           stem = TRUE)
#prep the documents. In this step, I remove words that appear in less than 1 document, as well as words that appear in more than 60 articles. This helps remove some noise.
num.docs <- length(tidy_stemming_MusicLyrics$documents)
max.docs <- num.docs-2
out <- prepDocuments(tidy_stemming_MusicLyrics$documents, tidy_stemming_MusicLyrics$vocab, tidy_stemming_MusicLyrics$meta,
                     lower.thresh = 1,upper.thresh = max.docs)

texts<-tidy_MusicLyrics$word[-tidy_stemming_MusicLyrics$docs.removed]


MusicLyrics_dfm <- tidy_MusicLyrics %>%
  count(CATMusicArtist, word, sort = TRUE) %>%
  cast_dfm(CATMusicArtist, word, n)

MusicLyrics_sparse <- tidy_MusicLyrics %>%
  count(CATMusicArtist, word, sort = TRUE) %>%
  cast_sparse(CATMusicArtist, word, n)

#You could use either of these objects (sherlock_dfm or sherlock_sparse)
# as the input to stm(). For this analysis we will use the quanteda dfm object.

#======================================================== >>

# We need to select the number of topic "K" to train the model with
# the stm includes lots of functions and support for choosing an appropriate
#number of topics for the model.
#Reference: http://thomaselliott.me/pdfs/earl/topic_modeling.html

#Next step is the processing workhorse. 
# This actually does the topic modeling. 
#First, we can run stm with init.type=“Spectral” and K=0 
# to have the algorithm calculate the appropriate number of topics itself.
#This does not mean the number of topics it finds is the true number of topics, 
#but it is a good place to start.

#Spectral initialization uses a decomposition of the VxV word co-occurence matrix
#to identify “anchor” words - words that belong to only one topic and therefore identify that topic.
#The topic loadings of other words are then calculated based on these anchor words. 
#This processes is deterministic, so that the same values are always arrived at with the same VxV matrix. 
#The problem is that this process assumes that the VxV matrix is generated from the population of documents
#(or, put another way, assumes it is generated from an infinite number of documents). 
#Thus, the process does not behave well with infrequent words. The solution to this is to remove infrequent words,
#though one should still be careful if you don’t have a lot of documents.
#My guess is that I should use Spectral, but make sure it is robust to a series of LDA models (meaning that Spectral produces results as good as or better than LDA models).
#I also suspect that I don’t have enough documents to trust the number of models to use.

STM_topic_model <- stm(MusicLyrics_dfm, K = 0, seed=12345,
                       verbose = FALSE, init.type = "Spectral")


#The above analysis identifies 32 topics, below are common words for each of these topics:
labelTopics(STM_topic_model)

#Below graphs how common each topic is:
plot.STM(STM_topic_model,type ="summary", xlim = c(0, 0.1))

#Calculating semantic coherance & exclusivity scored, find the harmonic mean.

#plot exclusivity vs semantic coherence
topicQuality(STM_topic_model, documents = out$documents)
#======================================================== <<


#Topic modelling
#stm, quanteda
#fitting the model:
#  unsupervised M learning
#which words contribute to which topics
#which topics contribute to which documents

#beta matrix: what are the words which contribute to each topic
#> plot the matrix, top 10 words in cluster

#gamma matrix:
#  how much did this document contribute to the topic?
#  how likely is this document to belong to this topic


STM_topic_model <- stm(MusicLyrics_dfm, K = 6, 
                       seed=12345,
                       verbose = FALSE, init.type = "Spectral")

td_BetaMatrix <- tidy(STM_topic_model)

td_BetaMatrix %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  #scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

#Now let’s look at another kind of probability we get as output from topic modeling, 
# the probability that each document is generated from each topic.
td_GammaMatrix <- tidy(STM_topic_model, matrix = "gamma",                    
                 document_names = rownames(MusicLyrics_dfm))

ggplot(td_GammaMatrix, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with exactly 1 story",
       y = "Number of stories", x = expression(gamma))

#In this case, each short story is strongly associated with a single topic.
#Topic modeling doesn’t always work out this way,
#but I built a model here with a small number of documents (only 6) 
#and a relatively large number of topics compared to the number of documents.
#In any case, this is how we interpret these gamma probabilities; they tell us which topics are coming from which documents.

# We can see some interesting things; there are shifts through the collection
# as topic 3 stories come at the beginning and topic 5 stories come at the end. 
#Topic 5 focuses on words that sound like spooky mysteries happening at night,
#in houses with doors, and events that you see or hear, topic 1 is about lords, 
#ladies, and wives, and topic 2 is about… GEESE. You can use each tab in the app 
#to explore the topic modeling results in different ways.



# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #
