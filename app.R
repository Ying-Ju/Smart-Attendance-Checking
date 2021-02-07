
# This is a Shiny web application that takes attendance for classes

# Authors:
# Tessa Chen


#_________________________________________________________________________
############ Load Packages and User-Defined Functions into R#############

source("Functions.R") # A File Contains Non Standard/packaged Functions
library(shinydashboard)
library(shiny)
library(htmltools)
library(dplyr)
library(stringr)
options(dplyr.summarise.inform = FALSE)

library(shinyLP)
#library(shinyBS)
#packages <- c("shiny", "shinydashboard","shinyLP","shinyBS") # Packages used in our application
#ipak(packages) # Loading + installing the packages using ipak from Functions.R
#________________________________________________________________________

################ Creating the User Interface for the App #################
ui <- dashboardPage( # Function from Shiny Dashboard
    skin = "red",
    # App header and its Contents Including the Help Menu
    dashboardHeader(title = "Smart Attendance",
                    titleWidth = 300,
                    dropdownMenu(# Produces ? Icon on Right + its contents
                        type = "notifications", 
                        icon = icon("question-circle"),
                        badgeStatus = NULL,
                        headerText = "See also:",
                        notificationItem("University of Dayton", icon = icon("university"),
                                         href = "https://udayton.edu/"),
                        notificationItem("Zoom", icon = icon("video"),
                                         href = "https://zoom.us/")
                    )),
    
    # App Pages as shown on the SideBar
    dashboardSidebar(width = 300,
                     sidebarMenu(
                         menuItem(HTML("<font size = \"5px\">  Home Page </font>"), tabName = "home", icon = icon("home")),
                         menuItem(HTML("<font size = \"5px\">  Check Attendance </font>"), tabName = "automated", icon = icon("check-circle")),
                         menuItem(HTML("<font size = \"5px\">  About the Author </font>"), tabName = "about", icon = icon("users"))
                     )
    ),
    
    # Contents of Each Page
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        tabItems(
            
            # Home Page Contents Using the ShinyLP Boostrapping Functionality
            tabItem(tabName = "home",
                    HTML("<h2> <b>Overview </b></h2>"), 
                    HTML("<font size='3'><p class = \"app\">  The Smart Attendance web app provides a way to check student attendance using a class roster and the zoom usage report. 
                    To use this app, you will need two files:
                   <br> <b> (1) class roster</b>, a .csv file that contains students' names in the first column, called Name. The format of names should be <b>Last Name, First Name</b>. A csv template for a class roster can be found <a href=\'https://github.com/Ying-Ju/Smart-Attendance-Checking/blob/master/roster.csv\' target=\'_blank\'>here</a>.
                   <br> <b> (2) zoom usage report</b>, a .csv file downloaded from zoom.us. A csv template for a zoom usage report can be found <a href=\'https://github.com/Ying-Ju/Smart-Attendance-Checking/blob/master/zoom.csv\' target=\'_blank\'>here</a>.</p>
                   </p></font>
                   
                   
                   
                   <h2> <b> How to Use the App? </b></h2>
                   <font size='3'><p class = \"app\"> We have created a voice-over-screen video to demonstrate how the app can be used to achieve
                   correct results (and no errors). We highly advise the reader to view the video prior to his/her's 
                   first use; the video is short and will reduce the start-up time for new users. <br> </p>
                   
                   <button class=\"button\" onClick=\"window.open('https://youtu.be/JBhssf_kO0Q');\">
                   <h4> <b> Click here for instrucational video!! </b>  </h4> </button>
                   
                   <p> <br> </p> </font>
                   "),
                    
                    fluidRow(
                        column(4, panel_div(class_type = "danger", 
                                            panel_title = "App Status", 
                                            content = HTML("<font size='3'><b> Version: </b> 0.1.0.
                                                   <br> <b> Last Updated at </b> February 8, 2021
                                                   <b> by </b> Tessa Chen.
                                                   <br> <b> Status: </b> No reported outages.</font>"))),
                        column(4, panel_div(class_type = "danger", 
                                            panel_title = "Application Maintainers",
                                            content = HTML("<font size='3'>The maintainer can be contacted via email at: 
                                            <a href='mailto:ychen4@udayton.edu?Subject=Smart%20Attendance%20App%20Help' target='_top'>Tessa Chen</a>.</font>"))),
                        column(4, panel_div(class_type = "danger", 
                                            panel_title = "Copyrights", 
                                            content = HTML("<p> <img height = \" 28\", src=\" http://i.creativecommons.org/p/zero/1.0/88x31.png\"> </img>
                                                                 <font size='3'><style=\"text-align:justify\"> 
                                                   <b> Code & App: </b> CC0 - 'No Rights Reserved' .
                                                   </p></font>")
                        )
                        )
                        
                        
                    ),
                    HTML("<h2> <b>Disclaimers: </b></h2>"), 
                    HTML("<font size='1'><p class = \"app\"> <ul>
                   <li> <p class = \"app\"> This web app is free software and comes with ABSOLUTELY NO WARRANTY.
                   You are welcome to redistribute it, given that this app comes with a CCO license. </p></li>
                   <li> <p class = \"app\">This version is a Beta Version. As of this moment, we have not included any Error Checks to the app. As 
                   such, the app will result in errors if the user inputs incomplete information. This issue may be
                   addressed after we recieve feedback from the community to understand how to better design the 
                   error handling. </p> </li>
                   <li> <p class = \"app\"> To reduce the likelihood of errors, please consult the \"How to Use the App?\" Section. If issues 
                   persist, please contact the maintainers. We will do our best to answer your emails within 2-5 business days. 
                   </p> </li> </ul></font>")
            ),
            
            tabItem(tabName = "automated",
                    HTML("<h1> <b>Instructions for Checking Attendance: </b></h1>"), 
                    HTML("<font size='4'><p class = \"app\">The app will generates the following outputs:
                         <br> (1) Names in the usage report cannot be found in the student roster if any
                         <br> (2) Students who presented in class less than the entire class time if any
                         <br> (3) A download link where you can download the final attendance file
                         <br>
                         <br>
                         </p>
                         </font>"),
                    
                    fluidRow(
                        #
                        column(12,sidebarLayout(
                            sidebarPanel(width = 12, id="sidebar_csv",
                                         HTML("<p.h> <b> <font size=\"5px\">  Upload the student roster CSV file </font> </b> </p>"),
                                         fileInput("roster_file", NULL, accept=c('text/csv', '.csv')),
                                         HTML("<p.h> <b> <font size=\"5px\">  Upload the zoom usage report CSV file </font> </b> </p>"),
                                         fileInput("zoom_file", NULL, accept=c('text/csv', '.csv')),
                                         HTML("<p.h> <b> <font size=\"5px\">  Input your class time (minutes) </font> </b> </p>"),
                                         numericInput("class_time", NULL, 50)
                            ),
                            mainPanel( width = 0)
                        ) )
                        #
                    ),
                    
                    # two conditions for showing the Run button: inputing file & not clicking on the Run button
                    conditionalPanel(
                        condition = "output.button && !input.runit",
                        tags$head(
                            tags$style(HTML('#runit{background-color:#e62e00}'))
                        ),
                        actionButton("runit",h2("Run"), style='padding:20px; font-size:100%; color:#ffffff')
                        
                        
                    ),
                    
                    conditionalPanel(
                        condition = "output.button",
                        textOutput("error_no1"),
                        textOutput("error_no2"),
                        span(textOutput("message"), style="color:black; font-size:10mm"),
                        br()
                    ),
                    conditionalPanel(
                        condition = "output.button",
                        HTML("<h1> <b>The following table shows the students whose total participating time is less than the class time. </b></h1>"), 
                        span(tableOutput("leave_early"), style="color:black; font-size:10mm")
                    ),
                    conditionalPanel(
                        condition = "output.button",
                        #span(tableOutput("file"), style="color:black; font-size:10mm")
                        span(downloadLink("download", "Download the Attendance Sheet Here"), style="font-size:12mm")
                        #downloadButton('download',"Download the Attendance File"),
                        #fluidRow(column(2,dataTableOutput('download')))
                    )
                    
            ),# For automated Tab Items
            
            # About Us Page
            tabItem(tabName = "about",
                    HTML("<h2> <b>Author of the App</b></h2> <br>"), 
                    
                    HTML("<font size='4'><div style=\"clear: left;\">
                   <p style=\"float: left;\"><img src=\"https://static.wixstatic.com/media/0f788f_1207b6b596cb430593bd55a2114aa6f1~mv2.jpg/v1/fill/w_297,h_297/b7Q9rDyU_400x400.jpg\" height=\"150\" width=\"150\" border=\"0px\" hspace=\"20\"></p>
                   <p class = \"app\"> Dr. Ying-Ju (Tessa) Chen is an Assistant Professor in the Department of Mathematics at the University of Dayton. 
                   Her expertise is in applied machine learning, high performance computing, statistical modeling, and
                   survival analysis. Her work has been funded by several foundations
                   and government agencies. 
                   <br> The source code used to create this app can be found <a href=\"https://github.com/Ying-Ju/Smart-Attendance-Checking\" target=\"_blank\">here</a>.
                   <br>
                   <br>
                   <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css\">
                   <a href=\"https://scholar.google.com/citations?user=nfXnYKcAAAAJ&hl=en&oi=sra/\" class=\"fa fa-google\" target=\"_blank\"></a>
                   <a href=\"https://github.com/Ying-Ju/\" class=\"fa fa-github\" targe=\"_blank\"></a>
                   <a href=\"https://twitter.com/ju_tessa/\" class=\"fa fa-twitter\" target=\"_blank\"></a>
                   <a href=\"https://www.linkedin.com/in/ying-ju-chen-a097a270/\" class=\"fa fa-linkedin\" target=\"_blank\"></a> </p>
                   </div>
                   </font>
                   ")
            )
        ) # Tab items
    ) # For Dashboard Body
) # For Dashboard Page


####################################################

server <- function(input, output, session) {
    
    output$button <- reactive({
        return((!is.null(input$roster_file))&(!is.null(input$zoom_file)))
    })
    outputOptions(output, "button", suspendWhenHidden = FALSE)
    
    
    # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
    # file$datapath -> gives the path of the file
    
    parameters <- reactiveValues()
    
    observe({
        file1 <- input$roster_file
        file2 <- input$zoom_file
        
        if ((!is.null(file1))&(!is.null(file2))){
            parameters$data_base1 <- read.csv(file=file1$datapath, sep=',', header = TRUE, stringsAsFactors = FALSE)
            parameters$data_base2 <- read.csv(file=file2$datapath, sep=',', header = TRUE, stringsAsFactors = FALSE)
        }
        
        parameters$class_time <- input$class_time
    })
    
    observeEvent(input$runit, {
        observe({
            req(input$roster_file, input$zoom_file, input$class_time)
            outcomes <- smart_checking(parameters$data_base1, parameters$data_base2, parameters$class_time)
            output$error_no1 <- renderText({outcomes$error_no1})
            output$error_no2 <- renderText({outcomes$error_no2})
            output$message <- renderText({outcomes$message})
            output$leave_early <- renderTable({outcomes$leave_early})
            #output$file <- renderTable({outcomes$file})
            data_file <- outcomes$file
            
            output$download <- downloadHandler(
               filename = function(){"attendance.csv"}, 
               content = function(fname){
                    write.csv(data_file, fname, row.names =FALSE)
                }
            )
            
        })
    
    })
}# end of server

# Run the application
shinyApp(ui, server)