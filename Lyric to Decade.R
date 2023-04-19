# Song Lyric Decade Project

# based off this https://www.youtube.com/watch?v=j1V2McKbkLo

# initialize environment:
#   load libraries and data (can be done retroactively)
#   choose NLP language to work with - something popular that's both in R and Python
#   create sample dataset - train/test data

# clean data:
#   keep only necessary data to start (lyrics, year, decade) 
#   create variable "decade"
#     will it be more accurate predicting year then figuring decade, or predicting decade?
#   remove punctuation, remove stop words, remove whitespace, lowercase
#   lemmatize (found->find), stem (playing->play)
#   tokenize ("Down the street the dogs are barking"->"down","street","dog","bark")
#       I think ours should only inspect single words associated with decade/genre
#       it would be technically difficult to study lyric prose and how words associate
#       with other words as a beginner project
clean <- function(x){
  # Find NLP library that does this for us
  return(x.clean)
}

# build term document matrix (we have to research our model)
#   this turns words into numbers, preparing data for model
#   this is our main function that uses 'clean'
generateTDM <- function(category,words){
  words.clean<-clean(words)
  words.tdm<-TermDocumentMatrix(words.clean) # Find NLP library that does this for us
  words.tdm<-removeSparseTerms(words.clean,0.7) # Find NLP library that does this for us
  list(category=category,tdm=words.tdm)
}

# attach category to tdm
bindCategoryTDM <- function(tdm){}
