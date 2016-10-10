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
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("Roll_Number","Roll Number",NULL) ,
      textInput("Name","Name",NULL),
      p("In case of Quiz- 'x' means absent. Quiz 1 marks are out of 12."),
      p("In case of Class- 'x' means Unregistered so those days won't count for attendance percent."),
      p("In case of Exam- 'a' means absent."),
      p("link for the overall attendence sheet is https://github.com/pranjalm/iter_dir/blob/master/section_c.csv")
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
    library(RCurl)
    x <- getURL("https://raw.githubusercontent.com/pranjalm/iter_dir/master/section_c.csv")
    data <- read.csv(text = x)
    #data <- read.csv("section_c.csv")
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
    d <- data.frame(data$reg_no,data$name,b,data$Q1,data$Q2,data$mid_sem)
    colnames(d)<- c("Roll_Number","Name","Attendance_percentage","Quiz_1_marks","Quiz_2_marks","Mid_Semester_marks")
    # find title in data$Title
    query1 <- grepl(input$Roll_Number , d$Roll_Number,ignore.case = TRUE)
    query2 <- grepl(input$Name , d$Name,ignore.case = TRUE)
    d[query1 & query2,]
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

