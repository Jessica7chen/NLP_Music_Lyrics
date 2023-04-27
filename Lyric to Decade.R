# Song Lyric Decade Project

# initialize environment
library(tm)
library(NLP)
# load data
song.data<-read.csv("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Stat Project/song_lyrics_sample.csv")
song.data<-song.data[,c(1,2,3,5,8)] # keeps id, title, lyrics, year, tag
decade<-floor(song.data$year/10)*10 # create decade vector
song.data$decade<-decade # save decade as column
song.data<-subset(song.data,decade>=1960)
# function that tallies unique numbers, produces plot, 
unique.count <- function(data){
  x=unique(data)
  y=NULL
  i=1
  for(i in i:length(x)){y<-append(y,sum(data==x[i]))}
  plot(x,y,type='h',xlab='unique',ylab='count',lwd=5)
  return(y[order(x)])
}
unique.count(song.data$decade) # exponential growth of songs in sample

# clean data function using tm
clean <- function(corpus){
  corpus <- VCorpus(VectorSource(corpus))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("en")) # removes common words (the...)
  corpus <- tm_map(corpus, stemDocument) # loved->love
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
# build term document matrix
generateTDM <- function(category,corpus){
  corpus<-clean(corpus)
  tdm<-TermDocumentMatrix(corpus)
  tdm<-removeSparseTerms(tdm,0.95) # removes sparse words
  df<-as.data.frame(as.matrix(tdm))
  df$category<-category
  # NORMALIZE DATA SO UNEVEN DISTRIBUTION BECOMES UNIFORM
  # normalize.df(df,length(df))
  return(df)
}

# running program, creating df
df<-generateTDM(song.data$decade,song.data$lyrics)
