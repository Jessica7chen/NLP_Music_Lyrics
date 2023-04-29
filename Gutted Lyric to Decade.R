# initialize environment
library(NLP)
library(tm)
library(class)
library(e1071)
library(randomForest)
# load data
song.data<-read.csv("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Stat Project/song_lyrics_sample.csv")
song.data<-song.data[,c(1,2,3,5,8)] # keeps id, title, lyrics, year, tag
decade<-floor(song.data$year/10)*10 # create decade vector
song.data$decade<-decade # save decade as column
song.data<-subset(song.data,decade>=1960)
table(song.data$decade)
# clean data function using tm
clean <- function(corpus){
  corpus<-VCorpus(VectorSource(corpus))
  corpus<-tm_map(corpus,content_transformer(tolower))
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,removeWords, stopwords('en')) # removes common words (the...)
  corpus<-tm_map(corpus,stemDocument) # loved->love
  corpus<-tm_map(corpus,stripWhitespace)
  return(corpus)
}
# build document term matrix, even it out
generateDTM <- function(category,corpus){
  corpus<-clean(corpus)
  dtm<-DocumentTermMatrix(corpus)
  dtm<-removeSparseTerms(dtm,0.95) # removes sparse words
  df<-as.data.frame(as.matrix(dtm))
  df$category<-category
  return(df)
}
# running program, creating df
df<-generateDTM(song.data$decade,song.data$lyrics)

sort(colSums(df[df$category==2010,-length(df)]),decreasing = T)[1:10]

# getting ready for models
df$category<-as.factor(df$category)
set.seed(123)
index<-sample(c(T,F),nrow(df),replace=T,prob=c(0.7,0.3))
train<-df[index,]
test<-df[!index,]

# svm - TOO SLOW, can't get to work
#svm <- svm(x=train[-length(train)],
#           y=train[length(train)],
#           kernel='linear',
#           cost=1,
#           type="C-classification")
#svm.y <- predict(svm, test[-length(test)])
#svm.cm<-table(test$category,rf.y)
#svm.cm
#sum(diag(svm.cm))/sum(svm.cm)

# randomForest
rf <- randomForest(x=train[-length(train)],
                   y=train$category,
                   ntree=10)
rf.y<-predict(rf,newdata=test[-length(test)])
rf.cm<-table(test$category,rf.y)
rf.cm
sum(diag(rf.cm))/sum(rf.cm)

# knn - need uniform dataset for this, or make each category a vector of mean values
knn.y <- knn(train=train[,-length(train)],
           test=test[,-length(test)],
           cl=train$category,
           k=13) #k=13 most accurate, but doesn't predict anything from 1960-1980
knn.cm<-table(test$category,knn.y)
knn.cm
sum(diag(knn.cm))/sum(knn.cm)
