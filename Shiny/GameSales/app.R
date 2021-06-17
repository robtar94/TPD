#biblioteki
library(shiny)
library(tidyverse)
library(plotly)

# Wczytanie Danych
data <- read.csv("Video_Games.csv")



#aplikacja

ui <- fluidPage(
  titlePanel("Video Games Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("GENREVIEW", h3("Gatunek:"),
                 choices = c(as.character(unique(data$Genre))),
                  selected = "Action"),
      selectInput("PLATFORMVIEW", h3("Platforma:"),
                  choices = c(as.character(unique(data$Platform))),
                 selected = "PC"),
      
      h3("Opis"),
      helpText("Zbiór zawiera tytuły gier posortowane względem ilości sprzedanych kopii")
      
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela", tableOutput("table")),
        tabPanel("Wykres 1", h2("LIczba gier z podziałem na gatunki"),plotlyOutput("plot")),
        tabPanel("Wykres 2", h2("Najpopularniejsze platformy w latach 90 i 2000"), plotlyOutput("plot2"),
                 sliderInput("zakres", "Lata: ", min = 1991, max = 2010, value = 2005, step = 1, sep = "", width = "100%")),
        tabPanel("Wykres 3", h2("Jakie są preferencje graczy w zakresie gatunków gier w zależności od Ratingu."), plotlyOutput("plot3")),
        tabPanel("Wykres 4", h2("Jaka jest średnia ocen dla danego gatunku w Europie."), plotlyOutput("plot4")),
        tabPanel("Wykres 5", h2("Jaka była ilość wydawanych tytułów w roku"), plotlyOutput("plot5"))
                
        
      
    )
     
      
    
    )
    )
)
  
 

server <- function(input, output, session) {
  
  output$table <- renderTable({data[,c(1:5)] %>% 
    filter(Genre == input$GENREVIEW) %>% 
    filter (Platform == input$PLATFORMVIEW) %>% 
    head(20)})
  
  
  
                      
  #1. Liczba gier z podzialem na gatunki
output$plot <- renderPlotly(({
  data %>% 
    group_by(Genre) %>% 
    summarise(Count = n()) %>% 
    plot_ly(x = ~Genre,
            y = ~Count,
            type = "bar")
}))

#Najpopularniejsze platformy w latach 90 i 2000

output$plot2 <- renderPlotly({
  data %>% 
    filter(Year_of_Release == input$zakres) %>%  
    group_by(Platform) %>% 
    summarise(Count = n()) %>% 
    plot_ly(x = ~Platform,
            y = ~Count,
            type = "bar")
   
  
})

# Jakie są preferencje graczy w kontekście wybieranych gatunków gier w zależności od Ratingu.


output$plot3 <- renderPlotly({
  data %>%
    na.omit() %>% 
    filter (Genre == input$GENREVIEW) %>% 
    group_by(Rating) %>% 
    summarise(Count = n()) %>% 
    plot_ly(x = ~Rating,
            y = ~Count,
            type = "bar")
  
  
})

# Jaka jest średnia ocen dla danego gatunku w Europie.


    output$plot4 <- renderPlotly({
      data %>%
        filter(Platform == input$PLATFORMVIEW) %>% 
        group_by(Genre) %>%
        summarise(Mean = mean(EU_Sales, na.rm = T)) %>% 
        plot_ly(x= ~Genre,
                y = ~Mean,
                type = "bar")
        
        
    })
    
    #   Jaka była ilość wydawanych tytułów w roku?

      
    output$plot5 <- renderPlotly({
      data %>%
        group_by(Year_of_Release) %>%
        summarise(Count = n()) %>% 
        plot_ly(x= ~Year_of_Release,
                y = ~Count,
                type = "scatter",
                mode = "lines")
      
      
    })
  
  
  
}

shinyApp(ui, server)