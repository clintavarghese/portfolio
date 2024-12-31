library(DT)
library(tidyverse)


table_creation<-function(x){
  datatable(x, 
            options = list(
              searching = FALSE,   # Removes the search bar
              pageLength = 10,      # Optional: Set the number of rows displayed per page
              lengthChange = FALSE,# Removes the option to change the number of rows displayed
              dom = 't'
            ),
            filter = 'none'
  )  
}


get_imdb_file <- function(fname){
  BASE_URL <- "https://datasets.imdbws.com/"
  fname_ext <- paste0(fname, ".tsv.gz")
  if(!file.exists(fname_ext)){
    FILE_URL <- paste0(BASE_URL, fname_ext)
    download.file(FILE_URL, 
                  destfile = fname_ext)
  }
  as.data.frame(readr::read_tsv(fname_ext, lazy=FALSE))
}

count_title<-function(x){
  word<-x
  count<-sum(grepl(word,TITLE_BASICS$titleType,ignore.case = TRUE))
  return(count)                        
}

identify_title<-function(df,word){
  x<-df|>filter(grepl(word,titleType,ignore.case = TRUE))
  return(x)                     
}

highest_top_50<-function(df)
{
  df|>
    arrange(desc(numVotes),desc(new_rating))|>
    slice(1:50)|>
    select(-averageRating,-tconst,-titleType)
}

find_projects<-function(actor_or_director){
  titles_1<-NAME_BASICS |> 
    filter(str_detect(primaryName, actor_or_director))|>select(nconst,knownForTitles)|>rename(tconst =knownForTitles)
  
  #group the titles that actor/director have worked on in TITLE_PRINCIPALS
  # Assuming 'titles' is a vector of nconst values
  titles_2 <- TITLE_PRINCIPALS %>%
    select(nconst,tconst) %>%
    filter(nconst %in% titles_1$nconst)
  
  
  #TITLE_CREW$directors = strsplit(TITLE_CREW$directors, ",")
  
  #similarly, group the TITLE_CREW for director id that they have worked
  titles_3<- TITLE_CREW|>
    select(directors,tconst)|>
    filter(directors %in% titles_1$nconst )|>
    rename(nconst = directors)
  
  xx <- bind_rows(titles_1,titles_2,titles_3)|>distinct()
  
  
  x11<- BASICS_RATING|>
    filter(tconst %in% xx$tconst)
  
    return(x11)
}

