library(pdftools)
library(dplyr)
library(stringr)
library(ggplot2)

### Remove stopwords function ###
rm_stopw <-function(x)
{
  stopw <-tm::stopwords(kind="en")
  x[-(which(str_to_lower(x) %in% stopw))]
}
  
### Create list of nouns by page ###
nouns <-pdf_text("/home/tsang/Bureau/beautifulpeople/beautiful_fall.pdf") %>%
        str_replace_all("\n"," ") %>%
        str_extract_all(pattern="(((?<![:punct:])[:upper:][:lower:]+[:space:]|\n)+)|\\d{4}") %>%
        lapply(str_trim) %>%
        lapply(rm_stopw)

### Give page number : p. ###
names(nouns) <- paste("p.",as.character(1:length(nouns)),sep="")

### Create matrix by number of page ###
ma_page <-function(x,i,j)
{
  page <-list()
  while(i <= j)
  {
    page[[names(x[i])]] <- data_frame(nouns=unlist(x[i]),page=names(x[i])) %>%
                        count(nouns,sort=TRUE,name="count") %>% mutate(page=names(x[i]))
    i = i + 1
  }
  return(page)
}
page <-ma_page(nouns,1,200)

### Plot method ###
ggplot(page[[9]],aes(x=nouns,y=count)) + geom_col() + coord_flip() + ggtitle(names(page[10]))
