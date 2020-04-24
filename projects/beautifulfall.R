library(pdftools)
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)

### Remove stopwords function ###
rm_stopw <-function(x)
{
  stopw <-tm::stopwords(kind="en")
  x[-(which(str_to_lower(x) %in% stopw))]
}
  
### Create list of nouns by page ###
nouns <-pdf_text("beautiful_fall.pdf") %>%
        str_replace_all("\n|\t|\r|\v|\f"," ") %>%
        lapply(str_replace_all,"[:space:]+"," ") %>%
        str_extract_all(pattern="(((?<![:punct:])[:upper:][:lower:]+[:space:])+)|\\d{4}") %>%
        lapply(str_trim) %>%
        lapply(rm_stopw)

### Give page number : p. ###
names(nouns) <- paste("p.",as.character(1:length(nouns)),sep="")

### Create list by number of page function ###
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

### Create dataframe function
dtf_page <-function(x,i,j)
{
  page <-data.frame(nouns=character(),
                    count=integer(),
                    page=character())
  while(i <= j)
  {
    page <- rbind(page,data_frame(nouns=unlist(x[i]),page=names(x[i])) %>%
      count(nouns,sort=TRUE,name="count") %>% mutate(page=names(x[i])))
    i = i + 1
  }
  return(page)
}

### The Most Beautiful People ###
shinyApp(
  ui <- fluidPage(
  titlePanel("The Most Beautiful People in Beautiful People"),
  tags$h4("Splendeurs et Misères de la Mode"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pages", "Choose the page:",
                                    value = 42,
                                    min = 1,
                                    max = 200)),
    mainPanel(
      plotOutput("popular"))
    )
  ),
  
  server <- function(input, output) {
  output$popular <- renderPlot({
  page <- dtf_page(nouns,input$pages,input$pages)
  beautiful <-page %>% group_by(nouns=as.factor(nouns)) %>% summarise(count=sum(count)) %>% arrange(desc(count))
  ggplot(beautiful[1:10,], aes(x=reorder(nouns,count), count)) + geom_col(fill="black") + coord_flip() + labs(x="nouns") + ggtitle(paste("The Most Beautiful People in page",as.character(input$pages)))
  })
})
