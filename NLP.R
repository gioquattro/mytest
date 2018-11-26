#https://www.datacamp.com/community/tutorials/R-nlp-machine-learning
library(data.table)


prince_orig = fread("prince_raw_data.csv")

str(prince_orig)
prince = prince_orig[, .(lyrics = text, song, year, album, peak, 
                          us_pop = US.Pop, us_rnb = US.R.B)]

library(Amelia)
missmap(prince)
detach(package:Amelia)

dim(prince)

prince[139,]$lyrics


# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

prince$lyrics <- sapply(prince$lyrics, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z]", " ", x)

# remove special characters
prince$lyrics <- sapply(prince$lyrics, removeSpecialChars)

prince[139,]$lyrics

summary(prince)

prince$decade = cut(prince$year, breaks = c(1969, 1979, 1989, 1999, 2009, 2019), 
                    labels = c("1970s", "1980s", "1990s", "2000s", "2010s"))
table(prince$decade)

prince$chart_level = cut(prince$peak, breaks = c(0,10,100), labels = c("Top 10", "Top 100"))
summary(prince$chart_level)
prince[is.na(chart_level), chart_level:="Uncharted"]
summary(prince$chart_level)

prince$charted = as.factor( ifelse(prince$peak %in% 1:100, "Charted", "Uncharted") )
summary(prince)

missmap(prince)


library(ggplot2)

ggplot(prince, aes(x=decade, fill=charted) ) + 
  geom_bar() + 
  labs(x="", y="Song count")

ggplot(prince, aes(x=decade, fill=chart_level) ) + 
  geom_bar() + 
  labs(x="", y="Song count")

ggplot(prince[!is.na(year)], aes(x=factor(year), fill=chart_level) ) + 
  geom_bar() + 
  labs(x="", y="Song count") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

ggplot(prince[charted=="Charted"], aes(x=decade, fill=chart_level) ) + 
  geom_bar() + 
  labs(x="", y="Song count")

prince[peak==1, .(year, song)][order(year)]

undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats", "na", "u")

fwrite(x = prince, file = "prince_data.csv", sep = ",", dec = ".", row.names = F, na = "")

library(quanteda)
tf = dfm(prince$lyrics, remove_punct=TRUE, tolower=TRUE, stem=FALSE, 
         remove=union(stopwords("english"), undesirable_words))
tf = dfm_trim(tf, min_termfreq=5, min_docfreq=5)
tfidf = dfm_tfidf(tf, scheme_tf="logcount", scheme_df="inverse")

to.data.table = function(x)
{
  return ( as.data.table(convert(x, to = "data.frame")) )
}

tf = to.data.table(tf)
tfidf = to.data.table(tfidf)
tf.names = setdiff(colnames(tf), "document")

# Some checks
dim(tf)
dim(tfidf)
tf[1:10, 1:10]
tfidf[1:10, 1:10]

prince$document = tf$document
prince$word_count = rowSums(tf[,2:ncol(tf)])

ggplot(prince, aes(x=word_count)) +
  geom_histogram(bins=15, col=1, fill=3) +
  facet_wrap(~chart_level, scales = "free_y", ncol = 1)

words.s = colSums(tf[,2:ncol(tfidf)])
words = data.table(word=names(words.s), freq=words.s)
remove(words.s)

ggplot(words[order(-freq)][1:10], aes(x=word, y=freq)) +
  geom_col(fill="blue")

# some improvements... flip the coordinaetes, sort for frequency
ggplot(words[order(-freq)][1:10], aes(x=reorder(word, freq), y=freq)) +
  geom_col(fill="blue") +
  coord_flip() +
  labs(x = NULL, y = "Word Count") +
  ggtitle("Popular Words") 

library(wordcloud2)
wordcloud2(data = words[freq>200])

plot_top_terms = function(field.id, tf.matrix, top.n)
{
  tf.id = cbind( data.table(myid=field.id), tf.matrix )
  tf.id = tf.id[!is.na(myid)]
  
  tf.grouped = tf.id[, lapply(.SD, sum, na.rm=TRUE), by=myid]
  
  tf.melt = melt(tf.grouped, id.vars=c("myid"))
  tf.melt = tf.melt[order(myid, -value)]
  tf.melt$pos = nrow(tf.melt):1
  
  tf.grouped2 = tf.melt[, .(maxpos=max(pos)), by=myid]
  
  tf.melt2 = merge(tf.melt, tf.grouped2, by="myid")
  tf.melt2 = tf.melt2[maxpos-pos < top.n]
  
  myplot = ggplot(tf.melt2, aes(x=pos, y=value)) + 
    geom_col(aes(fill=myid)) +
    facet_wrap(~myid, scales = "free") +
    coord_flip() +
    scale_x_continuous(breaks = tf.melt2$pos, 
                       labels = tf.melt2$variable) +
    labs(x = NULL, y = "Word Count") +
    ggtitle("Popular Words") +
    theme(legend.position = "none")
  
  print(myplot)
}

plot_top_terms(field.id=prince$chart_level, tf.matrix=tf[,2:ncol(tf)], top.n=15)
plot_top_terms(field.id=prince$chart_level, tf.matrix=tfidf[,2:ncol(tfidf)], top.n=15)

plot_top_terms(field.id=prince$decade, tf.matrix=tf[,2:ncol(tf)], top.n=10) #timeless words
plot_top_terms(field.id=prince$decade, tf.matrix=tfidf[,2:ncol(tfidf)], top.n=10)

# we can play with tfifd to easily compute lexical diversity
# We can play with string manipulation to compute word length distribution per chart level or per year



# Relationship between chart and decade

library(circlize)

decade_chart = prince[!is.na(decade), 
                      .(songs=length(song)), 
                      by=.(decade, chart_level) ][order(decade, chart_level)]

grid.col = c("1970s"=2, 
             "1980s"=3,
             "1990s"=4,
             "2000s"=5,
             "2010s"=7,
             "Top 10"=8,
             "Top 100"=8,
             "Uncharted"=8) #assign chord colors

circos.clear()
chordDiagram(decade_chart, grid.col = grid.col, transparency = .25)
title("Relationship Between Chart and Decade")


tf.sentiment = dfm(prince$lyrics, remove_punct=TRUE, tolower=TRUE, stem=FALSE, 
                   dictionary = data_dictionary_LSD2015)
tf.sentiment = to.data.table(tf.sentiment)

dim(tf.sentiment)

tf.sentiment
tf.sentiment[, sentiment:=( (positive-neg_positive) - (negative-neg_negative) )/( (positive-neg_positive) + (negative-neg_negative) )]
tf.sentiment

hist(tf.sentiment$sentiment, breaks = 50, col = 3)

# negative
# 2,858 word patterns indicating negative sentiment
# 
# positive
# 1,709 word patterns indicating positive sentiment
# 
# neg_positive
# 1,721 word patterns indicating a positive word preceded by a negation (used to convey negative sentiment)
# 
# neg_negative
# 2,860 word patterns indicating a negative word preceded by a negation (used to convey positive sentiment)


# Example!
#   
# txt <- c("This aggressive policy will not win friends",
#          "This aggressive policy will win friends",
#          "This policy is not bad",
#          "This policy is bad")
# 
# tf.sentiment = dfm(txt, remove_punct=TRUE, tolower=TRUE, stem=FALSE, 
#                    dictionary = data_dictionary_LSD2015)
# tf.sentiment = to.data.table(tf.sentiment)
# tf.sentiment

detach(package:quanteda)

library(tidytext)

txt <- c("This aggressive policy will not win friends",
          "This aggressive policy will win friends",
          "This policy is not bad",
          "This policy is bad")

tmp = data.frame(id=1:4, txt=txt)
tmp$txt = as.character(tmp$txt)

sapply(tmp, class)
 
tmp2 = unnest_tokens(tmp, output = "txt")
