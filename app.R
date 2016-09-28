#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("CVL3211 Student Records"),
  
  # Sidebar with a slider input for number of bins Attandence_percentage , Quiz_marks_12
  sidebarLayout(
    sidebarPanel(
      textInput("Roll_Number","Roll Number",NULL) ,
      textInput("Name","Name",NULL)  
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #tableOutput("dispres")
      tabsetPanel(type = "tabs", 
                  tabPanel("Student record", tableOutput("dispres"))
      )
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$dispres <- renderTable({
    
    # generate bins based on input$bins from ui.R
    #setwd("~/Desktop/ITER/CVL3211/attandence/section_c")
    data <- read.csv("section_c.csv")
    p<-0
    a<-0
    b <- c()
    for(j in 1:49){
      for(i in data[j,1:ncol(data)-1]){
        if (i=="p") { p <- (p+1) 
        }else if (i=="a") { a <-(a+1)
        }
      }
      b[j]=p*100/(a+p)
      a<-0
      p<-0
    }
    d <- data.frame(data$reg_no,data$name,b,data$Q1)
    colnames(d)<- c("Roll_Number","Name","Attandence_percentage","Quiz_marks_1")
    # find title in data$Title
    query1 <- grepl(input$Roll_Number , d$Roll_Number,ignore.case = TRUE)
    query2 <- grepl(input$Name , d$Name,ignore.case = TRUE)
    d[query1 & query2,]
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

