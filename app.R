######################################
#Name: Guess The Stock
#Author: Adam Walker
#Date: November 2017
######################################

library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)

#The UI code
#Controls what's displayed
ui <- fluidPage(
  theme = shinytheme("darkly"),
  #shinythemes::themeSelector(), 
  
  #Suppress messages
  tags$head(
    tags$style(HTML('a[data-title="save and edit plot in cloud"]{display:none;}'))
  ),

  # Formatted title
  column(9,align="center",titlePanel( HTML(paste(tags$span(style="font-size: 24px; font-style: bold; vertical-align: middle", "Guess the stock! Which is ")
  ,tags$span(style="color:#009E73; font-size: 24px;font-style: bold; vertical-align: middle", "green?"), sep = ""))))
  
  # Vertical layout for simplicity 
  ,verticalLayout(

    fluidRow(

      #Current streak, message, and personal best text
      column(width=4,align="center",textOutput("text1", container = span),div(style = "height:110%; padding-bottom: 12px"))
      #,column(width=3,align="center",htmlOutput("text2", container = span),div(style = "height:110%; padding-bottom: 12px"))
      ,column(width=4,offset=1,align="center",textOutput("text3", container = span),div(style = "height:110%; padding-bottom: 12px"))

    ),

     # Plot panel
     mainPanel(width=9,align="center"
       ,plotlyOutput("distPlot")
     )
   
  ,  fluidRow(
    
    #Decision buttons
    column(3,align="center",
           h3(textOutput("c1_full")),
           actionButton("Action1", textOutput("c1"),width = '80%'))
    
    #Prompt
    #https://shiny.rstudio.com/articles/html-tags.html
    ,column(width=3,align="center",style='padding:8px',htmlOutput("text2", container = span))

    ,column(3,align="center",
           h3(textOutput("c2_full")),
           actionButton("Action2", textOutput("c2"),width = '80%'))
    )
  
  #Text formatting blocks
  ,tags$head(tags$style("#text1{color: #009E73;
                       font-size: 18px; font-style: bold;
                       }"))
  
  ,tags$head(tags$style("#c1_full{font-size: 16px;
                        font-style: bold;
                        }"))
  
  ,tags$head(tags$style("#c2_full{font-size: 16px;
                        font-style: bold;
                        }"))
  
  ,tags$head(tags$style("#text3{color: #009E73;
                       font-size: 18px;
                       font-style: bold;
                       }"))

  )
  
)

#Server code
#Note that variables are reactive
#https://shiny.rstudio.com/articles/basics.html
server <- function(input, output) {
  
  #Read in test data
  path<-'./data/'
  
  #Created 999 stock pairings
  n<-999
  
  #Initialize counter
  #i for the current streak
  i <- reactiveVal(0)
  #Initialize PR
  pr <- reactiveVal(0)
  
  #Initialize sample
  #Randomly picking a stock pairing to display
  sample_num<-reactiveVal(sample(1:n,size = 1))
  
  # Pull in main datafile
  # Contains stock symbol, date, and percentage change from baseline
  filedata <- reactive({
    x<-readRDS(paste0(path,'comb.stock.',sample_num(),'.RDS'))
    return(x)
  })
  
  # Symbol lookup
  # Mainly used to get company name, and also to make sure plot colors are correct
  lookupdata <- reactive({
    x<-readRDS(paste0(path,'comb.lookup.',sample_num(),'.RDS'))
    return(x)
  })
  
  #Initialize message text color
  #This is usually green, but switches to red if wrong answer
  textcolor<-reactiveVal("#009E73")
  
  #Assign variables based on inputs
  
  #Stock 1 (the left button) symbol
  c1.symbol<-reactive({x<-lookupdata()[1,] %>% pull(Symbol); return(x)})
  output$c1<-reactive({c1.symbol()})
  #Max value: used to verify colors are correct in chart
  c1.max_value<-reactive({x<-lookupdata()[1,] %>% pull(max_value); return(x)})
  #Company name
  output$c1_full<-reactive({x<-lookupdata()[1,] %>% pull(Name); return(x)})
  #Company color
  c1.color<-reactive({x<-lookupdata()[1,] %>% pull(GroupColor); return(x)})
  
  #Stock 2 (right side) variables
  c2.symbol<-reactive({x<-lookupdata()[2,] %>% pull(Symbol); return(x)})
  output$c2<-reactive({c2.symbol()})
  output$c2_full<-reactive({x<-lookupdata()[2,] %>% pull(Name); return(x)})
  c2.color<-reactive({x<-lookupdata()[2,] %>% pull(GroupColor); return(x)})
  
  #Determine winner
  #This is the stock that has the green color assigned to it
  right_answer <- reactive({if(c1.color()=="#009E73"){1}else{2}})

  #For debugging
  #output$text4 <- renderText({ paste0("Right answer: ", right_answer(),"  ",sample_num()) })
  
  #Initial prompt
  answer<-reactiveVal("Go ahead, make your pick...")
    
  #Streak
  output$text1 <- renderText({ paste0("Current streak: ", i()) })
  #Message
  output$text2 <- renderText({
    paste0('<span style=\"font-size: 18px; color:', textcolor(), 
           '\">',answer(),'</span>')})
  #PR
  output$text3 <- renderText({ paste0("Personal Best: ", pr()) })
  
  #LEFT SIDE BUTTON
  observeEvent(input$Action1, {
    
    #Increment current streak counter
    if(right_answer()==1){i(i()+1)}else{i(0)}
    #Increment PR
    if(right_answer()==1 & i()>pr()){pr(pr()+1)}else{pr()}
    
    #Show reaction based on right/wrong answer and user's current streak
    #Can build out more, but for right now just if statement
    if (right_answer()==1 & i()==1) 
    {answer("CORRECT!")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=2)
    {answer("Now we're rolling")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=3)
    {answer("Have you played before?")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=4)
    {answer("Getting streaky?")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=5)
    {answer("Impressive...")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=6)
    {answer("Martin Shkreli?")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=7)
    {answer("Are you googling these?")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=8)
    {answer("Your success scares me.")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=9)
    {answer("WALL STREET LEGEND")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()<=10)
    {answer("Alright alright alright")
      textcolor('#009E73')
    }
    
    else if (right_answer()==1 & i()>10)
    {answer("You broke the game.")
      textcolor('#009E73')
    }
    
    else {answer("DUN GOOFED")
      textcolor('red')
    }
    
    #Pick a new sample number
    sample_num(sample(1:n,size = 1))
    
  })
  
  #RIGHT SIDE BUTTON
  observeEvent(input$Action2, {
    
    if(right_answer()==2){i(i()+1)}else{i(0)}
    if(right_answer()==2 & i()>pr()){pr(pr()+1)}else{pr()}
    
    if (right_answer()==2 & i()==1) 
    {answer("CORRECT!")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=2)
    {answer("Now we're rolling")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=3)
    {answer("Have you played before?")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=4)
    {answer("Getting streaky?")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=5)
    {answer("Impressive...")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=6)
    {answer("Martin Shkreli?")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=7)
    {answer("Are you googling these?")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=8)
    {answer("Your success scares me.")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=9)
    {answer("WALL STREET LEGEND")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()<=10)
    {answer("Alright alright alright")
      textcolor('#009E73')
    }
    
    else if (right_answer()==2 & i()>10)
    {answer("You broke the game.")
      textcolor('#009E73')
    }
    
    else {answer("DUN GOOFED")
      textcolor('red')
    }
    
    #Pick a new sample number
    sample_num(sample(1:n,size = 1))
    

  })
  
  #MAKE STOCK PLOT
  output$distPlot <- renderPlotly({
    
    #Plotting percentage change from 2year baseline
    p<-ggplot(filedata(),aes(Date,Perc_Change,group=Symbol,text = paste("Percentage:", 100*round(Perc_Change,2),"%"))) +
      geom_line(lwd=2,color=filedata()$GroupColor) + theme(legend.position="none")
    
    #Get rid of annoying modebar!
    p <- ggplotly(p, dynamicTicks = T, tooltip = c("text")) %>% 
      layout(title = "Plot") %>% 
      config(displayModeBar = F) %>% 
      layout(paper_bgcolor='rgb(254, 247, 234)')

    #Force correct colors
    #Had all kinds of issues on this
    p$x$data[[1]]$line$color<-ifelse(max(p$x$data[[1]]$y)==c1.max_value(),c1.color(),c2.color())
    p$x$data[[2]]$line$color<-ifelse(max(p$x$data[[2]]$y)==c1.max_value(),c1.color(),c2.color())
    
    #Make plot
    p %>% 
      layout(
        title = paste0(c1.symbol()," vs. ",c2.symbol()),
        margin=list(t=65,b=15,l=50,r=10),
        xaxis = list(title=NULL,
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(
                count = 1,
                label = "YTD",
                step = "year",
                stepmode = "todate"),
              list(step = "all"))),
          
          rangeslider = list(type = "date")),
        
        yaxis = list(title = NULL,tickformat = "%"))

  })
  
}

shinyApp(ui = ui, server = server)
