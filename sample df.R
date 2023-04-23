# Sample from Genius Music Lyric Database

song.lyrics<-read.csv("C:\\Users\\bbkid\\Desktop\\archive\\song_lyrics.csv") # import df
en.song.lyrics<-song.lyrics[song.lyrics$language_cld3=='en',] # only choose english
x<-sample(1:nrow(en.song.lyrics),50000,replace=F) # sample numbers
en.song.lyrics.sample<-en.song.lyrics[x,] # create sample df
head(en.song.lyrics.sample) # inspect head of sample df

write.csv(en.song.lyrics.sample,file = "C:\\Users\\bbkid\\Desktop\\archive\\song_lyrics_sample.csv")
# write sample df