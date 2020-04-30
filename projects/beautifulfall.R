library(pdftools)
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(rvest)

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

### Create dataframe function ###
dtf_page <-function(x,i,j)
{
  if (i > j)
  {
    tmp <-i
    i <-j
    j <-tmp
  }
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

### Shiny ###
shinyApp(
  ui <- fluidPage(
  titlePanel("The Most Beautiful People in Beautiful People"),
  tags$h4("Splendeurs et Misères de la Mode"),
  sidebarLayout(
    sidebarPanel(
      numericInput("page1", "Choose the page(s) from:",
                                    value = 1,
                                    min = 1,
                                    max = 200),
      numericInput("page2", "to:", value = 2,
                                   min = 1,
                                   max = 200)),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput("popular")),
        tabPanel("Images",htmlOutput("images")),
        tabPanel("Betty who?",plotOutput("betty"))
        )
      )
    )
  ),
  
  server <- function(input, output) {
    rpage <- reactive({
    page <- dtf_page(nouns,input$page1,input$page2)
  })
  
    output$popular <- renderPlot({
      beautiful <-rpage() %>% group_by(nouns=as.factor(nouns)) %>% summarise(count=sum(count)) %>% arrange(desc(count))
      ggplot(beautiful[1:10,], aes(x=reorder(nouns,count), count)) + geom_col(fill="black") + coord_flip() + labs(x="nouns") + ggtitle(paste("The Most Beautiful People from page",as.character(input$page1),"to",as.character(input$page2)))
    })
  
    output$betty <- renderPlot({
      betty <- dtf_page(nouns,1,200) %>% mutate(nouns=replace(nouns,nouns=="Betty Catroux","Betty")) %>% group_by(nouns=as.factor(nouns)) %>% filter(nouns=="Betty") %>% mutate(page = as.numeric(str_extract(page,"[0-9]+")))
      betty$count[c(6,23,25,28)] <- c(2,2,5,3)
      betty <- betty[-c(7,24,26,29),]
      ggplot(betty,aes(x=page,y=count)) + geom_line()
    })
    
    output$images<-renderText({
    beautiful <-rpage() %>% group_by(nouns=as.factor(nouns)) %>% summarise(count=sum(count)) %>% arrange(desc(count))
    ### Google TBS parameters valid for image search (TBM=isch) ###
    # Image size larger than 640×480: tbs=isz:lt,islt:vga
    size1 <- "isz:lt,islt:vga"
    # Image size larger than 800×600: tbs=isz:lt,islt:svga
    size2 <- "isz:lt,islt:svga"
    # Image size larger than 1024×768: tbs=isz:lt,islt:xga
    size3 <- "isz:lt,islt:xga"
    # Image size larger than 1600×1200: tbs=isz:lt,islt:2mp
    size4 <- "isz:lt,islt:2mp"
    # Image size larger than 2272×1704: tbs=isz:lt,islt:4mp
    size5 <- "isz:lt,islt:4mp"
    # Image size exactly 1000×1000: tbs=isz:ex,iszw:1000,iszh:1000
    # Image in full color: tbs=ic:color
    color1 <- "ic:color"
    # Image in black and white: tbs=ic:gray
    color2 <- "ic:gray"
    # Image that are red: tbs=ic:specific,isc:red [orange, yellow, green, teal, blue, purple, pink, white, gray, black, brown]
    # Image type Face: tbs=itp:face
    # Image type Photo: tbs=itp:photo
    # Image type Animated (gif): tbs=itp:animated (thanks Dan)
    # Group images by subject: tbs=isg:to
    keyword3 <- "marineau"
    keyword4 <- "guy"
    keyword5 <- "photography"
    keyword1 <- sample(beautiful$nouns,1)
    keyword2 <- sample(beautiful$nouns,1)

    url <- paste("http://www.google.com/search?q=%22",
                 keyword3, "+", keyword4, "+", keyword5, "+", keyword1, "+", keyword2,
                 "%22&tbm=isch&tbs=",
                 color2,
                 size5,
                 "tbs=isg:to",
                 sep="")
    page <- read_html(url)
    node <- html_nodes(page,xpath = '//img') %>% lapply(str_extract_all, "http[^\"><]+")
    height <- "height='100'"
    c("<img src=", unlist(node[5]), height, ">",
      "<img src=", unlist(node[6]), height, ">",
      "<img src=", unlist(node[7]), height, ">",
      "<img src=", unlist(node[8]), height, ">",
      "<img src=", unlist(node[9]), height, ">",
      "<img src=", unlist(node[10]), height, ">",
      "<img src=", unlist(node[11]), height, ">",
      "<img src=", unlist(node[12]), height, ">",
      "<img src=", unlist(node[13]), height, ">",
      "<img src=", unlist(node[14]), height, ">")
  })
})
