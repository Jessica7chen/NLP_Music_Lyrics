# Song Lyric Decade Project

# initialize environment:
#   load libraries and data (can be done retroactively)
#   choose NLP language to work with
#   create sample dataset - train/test data

# clean data:
#   keep only necessary data to start (lyrics, year, decade) 
#   create variable "decade"
#     will it be more accurate predicting year then figuring decade, or predicting decade?
#   remove punctuation, remove stop words
#   lemmatize (found->find), stem (playing->play)
#   tokenize ("Down the street the dogs are barking"->"Down","street","dog","bark")
#       I think ours should only inspect single words associated with decade/genre
#       it would be technically difficult to study lyric prose and how words associate
#       with other words as a beginner project

# build term document matrix (we have to research our model)
#   this turns words into numbers, preparing data for model