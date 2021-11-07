# Emp
library(stringr)
movies <- readLines("movies.txt")

parseMovies <- function(movie){
  # split by first ":"
  temp <- unlist(str_split(movie, ":", 2))
  movieid <- temp[1]
  # looks for last "):"  
  temp2 <- unlist(strsplit(temp[2], "\\):(?=[^:]+$)", perl=TRUE))
  genres <- temp2[2]
  
  # looks for last "("
  temp3 <- unlist(strsplit(temp2[1], " \\((?=[^(]+$)", perl=TRUE))
  title <- temp3[1]
  year <- temp3[2]
  return(data.frame(movieid = movieid, title = title, year = year, genres = genres))
}

df <- data.frame(movieid = character(), title = character(), year = character(), genres = character())

for (i in 1:length(movies)){
  df <- rbind(df, parseMovies(movies[i]))
}

movies_csv <- df[,c("movieid", "title", "year")]
write.csv(movies_csv, file ="movies.csv", row.names = F)

has_genres <- df[,c("movieid", "genres")]
genresList <- data.frame(movieid = character(), genre = character())
for (i in 1:nrow(has_genres)){
  list <- unlist(strsplit(as.character(has_genres[i,]$genres), split="\\|"))
  for (j in 1:length(list))
    genresList <- rbind(genresList, data.frame(movieid = has_genres[i,]$movieid, genre = list[j]))
}

write.csv(genresList, file ="hasGenres.csv", row.names = F)

tags <- readLines("tags.txt")
parseTags <- function(tag){
  temp <- unlist(str_split(tag, ":", 3))
  userid <- temp[1]
  movieid <- temp[2]
  # looks for the last ":"
  temp2 <- unlist(strsplit(temp[3], ":(?=[^:]+$)", perl=TRUE))
  tag <- temp2[1]
  timestamp <- temp2[2]
  return(data.frame(userid = userid, movieid = movieid, tag = tag, timestamp = timestamp))
}

tags_df <- data.frame(userid = character(), movieid = character(), tag = character(), timestamp = character())
for (i in 1:length(tags)){
  res <- parseTags(tags[i])
  tags_df <- rbind(tags_df, res)
  print(i)
}

write.csv(tags_df, file ="tags.csv", row.names = F)
