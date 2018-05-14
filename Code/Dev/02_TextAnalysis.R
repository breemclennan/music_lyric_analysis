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

# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 


titles.format <- theme(plot.title = element_text(face="bold", size=13, color='grey50'),
                       plot.subtitle = element_text(color='grey50'),
                       axis.title = element_text(size=9, color='grey50'), 
                       axis.text = element_text(size=9, color='grey50'),
                       plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"))

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
  filter(BINTrackIsInstrumental == 0)


# ==== PART B: Natural Language Processinh (NLP) - Sentiment Polarity

library(syuzhet)
SongSentiment <- get_nrc_sentiment(wrk.02_TextAnalysis_02$TXTAllTrackLyrics)

wrk.02_TextAnalysis_03 <- bind_cols(wrk.02_TextAnalysis_02, SongSentiment)
#syuzhet pkg
#Calls the NRC sentiment dictionary to calculate the presence of 
#eight different emotions and their corresponding valence in a text file.

barplot(
  sort(colSums(prop.table(SongSentiment[, 1:8]))), 
  #  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Songs", xlab="Percentage"
)
# TODO: fix the BIN instrumental flag, and view the plot by album / song


# ====== EXPERIMENTAL CODE ========== #



#==== TODO: time duration is not available for all albums


  #mutate(CATMusicArtist = as.factor(CATMusicArtist )) %>%
  #mutate(CATMusicAlbum = as.factor(CATMusicAlbum )) %>%
 #mutate(key = as.factor(key)) #%>%
  #mutate(duration_ms = minutes(duration_ms)) 


# Summary table: What artists, albums and tracks?
artists <- df %>% 
  group_by(CATMusicArtist, CATMusicAlbum) %>% 
  summarise(nbreSongs = n(), duration = seconds_to_period(sum(duration_ms)))
kable(artists, format = 'markdown')

# Songs by key
#TODO SHOW BY ARTIST / TRACK
df %>% filter(!is.na(key)) %>%
  group_by(key) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>%
  mutate(key = reorder(key, -n)) %>%
  ggplot(., aes(x=key, y=n)) +
  geom_bar(stat = "identity", 
           color='white',
           fill='#FCCB85',
           ) +
  xlab('Keys') +
  ylab('Number of Songs') +
  labs(title='Songs per Key', subtitle='18 songs are NAs') +
  titles.format
  
# Most songs are in D key



## PART A: LYRICAL ANALYSIS
# What do the songs say?
df <- df %>%
  ungroup() %>%
  mutate(TXTAllTrackLyrics = str_replace_all(TXTAllTrackLyrics, '\'', ' ')) %>%
  mutate(nbreLines = str_count(TXTAllTrackLyrics, '<br>') + 1) %>%
  mutate(nbreWord = str_count(TXTAllTrackLyrics, ' ') + 1)

lineToken <- df %>%
  unnest_tokens(line, TXTAllTrackLyrics, token = stringr::str_split, pattern = '<br>') %>% 
  mutate(lineCount = row_number())

wordToken <-  lineToken %>% 
  unnest_tokens(word, line) %>% 
  mutate(wordCount = row_number())

countWord <- count(wordToken, word, sort=TRUE)
countWord <- head(countWord, 100)
empty <- data.frame(a=character(100),b=rep('|',100),c=character(100), stringsAsFactors = FALSE)

data("stop_words")
wordToken2 <- wordToken %>% 
  anti_join(stop_words) %>%
  arrange(wordCount)
countWord2 <- count(wordToken2, word, sort=TRUE)
countWord2 <- head(countWord2, 100)

#plot setup
medianWord <- median(df$nbreWord)
uniqueWords <- wordToken2 %>% 
  select(word) %>%
  filter(!str_detect(word, '[0-9]')) %>%
  group_by(word) %>% 
  filter(row_number(word) == 1) %>%
  arrange(word)
nbreUniqWords <- nrow(uniqueWords)

#distribution plot of songs by number of words
df %>% 
  ggplot(., aes(x=nbreWord)) +
  geom_histogram(binwidth=10,
                 color='white',
                 fill='#FCCB85') +
  geom_vline(aes(xintercept=medianWord), colour="#990000", linetype="dashed") +
  coord_cartesian(ylim=c(0, 15)) + 
  scale_y_continuous(breaks=seq(0, 15, 1)) +
  scale_x_continuous(breaks=seq(0, 400, 20)) +
  theme(panel.grid.minor = element_blank()) +
  xlab('Total Number of Words') +
  ylab('Nbre of Songs') +
  labs(title='Distribution of Songs by Number of Words', 
       subtitle='Verses repeats not included - Dashed red line is median') +
  titles.format

# Unigrams  and Word clouds 
tab <- cbind(countWord, empty, countWord2)
kable(tab[1:20,], format='markdown', row.names = F,
      col.names = c('All uniGrams', 'Freq', ' ', '|', ' ', 'Cleaned uniGrams', 'Freq'))

layout(matrix(c(1,2),1,2, byrow = TRUE)) #6 ROWS 2 COLS
wordcloud(countWord$word, countWord$n, random.order=FALSE, max.words = 100, 
          colors=brewer.pal(8, "Dark2"), use.r.layout=TRUE)
wordcloud(countWord2$word, countWord2$n, random.order=FALSE, max.words = 100,
          colors=brewer.pal(8, "Dark2"), use.r.layout=TRUE)

#Unigrams by album
l <- length(levels(as.factor(wordToken2$CATMusicAlbum)))
plotList <- list()
for(i in 1:l){
  part <- wordToken2[wordToken2$CATMusicAlbum == levels(as.factor(wordToken2$CATMusicAlbum))[i],] %>%
    filter(BINTrackIsInstrumental == 0) %>%
    group_by(CATMusicAlbum) %>%
    count(word) %>%
    top_n(20)
  p <- ggplot(part[1:20,], aes(reorder(word,n), n)) +
    geom_bar(stat = "identity", fill='#FCCB85', width=0.65) +
    #        scale_fill_discrete(drop=F) +
    labs(y=NULL, x=NULL, title=paste('Album: ', levels(as.factor(wordToken2$CATMusicAlbum))[i], sep='')) +
    coord_flip() +
    titles.format +
    theme(plot.title = element_text(size=11))
  plotList[[i]] <- p
}
do.call(grid.arrange, c(plotList, ncol=3))

## BI GRAMS & TRI GRAMS
nGram <- data_frame(text=paste(wordToken$word, collapse = ' '))
nGramCleaned <- data_frame(text=paste(wordToken2$word, collapse = ' '))

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
