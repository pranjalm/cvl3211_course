library(shiny)

ui <- shinyUI(fluidPage(
  
  titlePanel("CVL3211 Student Records"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("Roll_Number","Roll Number",NULL) ,
      textInput("Name","Name",NULL),
      p("In case of Quiz- 'x' means absent. Quiz 1 marks are out of 12."),
      p("In case of Class- 'x' means Unregistered so those days won't count for attendance percent."),
      p("In case of Exam- 'a' means absent."),
      helpText( a("All the course data can be found here",href="https://github.com/pranjalm/iter_dir")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #tableOutput("dispres")
      tabsetPanel(type = "tabs", 
                  tabPanel("Student record", tableOutput("dispres")),
                  tabPanel("Open ended problem statements", tableOutput("dispres1"))
      )
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$dispres <- renderTable({
    library(RCurl)
    x <- getURL("https://raw.githubusercontent.com/pranjalm/iter_dir/master/section_c.csv")
    data <- read.csv(text = x)
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
    query1 <- grepl(input$Roll_Number , d$Roll_Number,ignore.case = TRUE)
    query2 <- grepl(input$Name , d$Name,ignore.case = TRUE)
    d[query1 & query2,]
  })
  
  output$dispres1 <- renderUI({
    str1 <- paste("1.Design a concrete mix for a building subjected to moderate exposure condition to achieve characteristic strength of concrete 15 MPa with air content of 5%.")
    str2 <- paste("2.Design a concrete mix for a building subjected to moderate exposure condition to achieve characteristic strength of concrete 15 MPa with air content of 10%.")
    str3 <- paste("3.Design a concrete mix for a building subjected to moderate exposure condition to achieve characteristic strength of concrete 15 MPa with air content of 15%.")
    str4 <- paste("4.Design a concrete mix for a building subjected to moderate exposure condition to achieve characteristic strength of concrete 25 MPa with air content of 5%.")
    str5 <- paste("5.Design a concrete mix for a building subjected to moderate exposure condition to achieve characteristic strength of concrete 25 MPa with air content of 10%.")
    str6 <- paste("6.Design a concrete mix for a building subjected to moderate exposure condition to achieve characteristic strength of concrete 25 MPa with air content of 15%.")
    str7 <- paste("7.Design a sealent using cement and any other mixture to be used as filling road cracks.")
    HTML(paste(str1, str2,str3,str4,str5,str6,str7, sep = '<br/>'))
     
    })
})

# Run the application 
shinyApp(ui = ui, server = server)

