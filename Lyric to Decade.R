# initialize environment
library(NLP) # incorporate machine learning and deep learning techniques.
library(tm) # statistical and computational methods for analyzing text data
library(class) # classify text data into different categories or classes based on their features or content
library(e1071) # functions for SVM training and classification
library(randomForest) # train on features and labels to predict categories for text instances
# load data
song.data<-read.csv("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Stat Project/song_lyrics_sample.csv")
song.data<-song.data[,c(1,2,3,5,8)] # keeps id, title, lyrics, year, tag
decade<-floor(song.data$year/10)*10 # create decade vector
song.data$decade<-decade # save decade as column
song.data<-subset(song.data,decade>=1960)
table(song.data$decade)
table(song.data$tag)

# clean data function using tm
clean <- function(corpus){
  corpus<-VCorpus(VectorSource(corpus))
  corpus<-tm_map(corpus,content_transformer(tolower))
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,removeWords, stopwords()) # removes common words (the...)
  corpus<-tm_map(corpus,stemDocument) # loved->love
  corpus<-tm_map(corpus,stripWhitespace)
  return(corpus)
}
# code to make uneven distribution more even/uniform
weight.df <- function(df,colNum){
  dfb<-df[-colNum] # dfb without colNum
  par(mfrow=c(2,1)) # sets plots up
  cat<-unique(df[,colNum]) # categories of colNum
  i=1 # counter
  counts=NULL # vector
  print("old values")
  for(i in i:length(cat)){
    count<-sum(dfb[df[colNum]==cat[i]]) # separate data from each category
    print(c(cat[i],count)) # print total sums
    counts<-append(counts,count) # vector of total sums
  }
  plot(cat,counts,type='h',xlab='unique',ylab='count',main='old values',lwd=5,ylim=c(0,max(counts)))
  answer=readline("Would you like to make data uniform? (Y/N): ")
  if(tolower(answer)=="y"){
    i=1 # reset counter
    print("new values")
    for(i in i:length(cat)){ # loop through cat
      adj<-round(max(counts)/counts[i]) # finds adjustment, multiply to keep value as ints
      i2<-df[colNum]==cat[i] # indices of category
      dfb[i2,]<-dfb[i2,]*adj # makes adjustment
      # SUM TOTAL AND READJUST COUNTS
      count<-sum(dfb[i2,])
      counts[i]<-count
      print(c(cat[i],count))
    }
    plot(cat,counts,type='h',xlab='unique',ylab='count',main='new values',lwd=5,ylim=c(0,max(counts)))
  }
  dfb$category<-df[colNum] # return original column values
  return(dfb)
}
# build term document matrix, even it out
generateDTM <- function(category,corpus){
  corpus<-clean(corpus)
  dtm<-DocumentTermMatrix(corpus)
  dtm<-removeSparseTerms(dtm,0.99) # removes sparse words
  df<-as.data.frame(as.matrix(dtm))
  df$category<-category
  df<-weight.df(df,length(df))
  return(df)
}
# running program, creating df
df<-generateDTM(song.data$decade,song.data$lyrics)

# getting ready for models
df.factor<-df
df.factor$category<-as.factor(df.factor$category$category)
set.seed(123)
index<-sample(c(T,F),nrow(df),replace=T,prob=c(0.7,0.3))
train.factor<-df.factor[index,]
test.factor<-df.factor[!index,]
# svm - TOO SLOW
#svm <- svm(x=train.factor[-length(train.factor)],
#           y=train.factor[length(train.factor)],
#           kernel = 'linear',
#           cost=1,
#           type = 'C-classification')
#print(svm)
#plot(svm)
# randomForest
rf <- randomForest(x=train.factor[-length(train.factor)],
                   y=train.factor$category,
                   ntree=10)
rf.y<-predict(rf,newdata=test.factor[-length(test.factor)])
cm<-table(test.factor$category,rf.y)
acc<-sum(diag(cm))/sum(cm)
acc
# knn - TOO SLOW
#knn <- knn(train=train[,-length(train)],
#           test=test[,-length(test)],
#           cl=train[,length(train)]$category,
#           k=5 #round(sqrt(nrow(train)))
#           )
