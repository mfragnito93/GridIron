library(shiny)
library(shinydashboard)
library(rhandsontable)
library(plotly)

header <- dashboardHeader(title = "Gridiron") 

sidebar <- dashboardSidebar(
  sidebarUserPanel("Ocenside",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # Image file should be in www/ subdir
                   image = "chalkboard-black.jpg"
  ),
  sidebarMenu(
    menuItem("Play Entry",tabName = "play_entry", icon=icon("th-large")),
    menuItem("Drive Summary", tabName = "drive_summary", icon = icon("th-large")),
    menuItem("Offense", icon = icon("th-large"),
             menuSubItem("Summary", icon = icon("th-large"), tabName = "o_summary"),
             menuSubItem("Down", icon = icon("th-large"), tabName = "o_down"),
             menuSubItem("Run Pass", icon = icon("th-large"), tabName = "o_down"),
             menuSubItem("Formation", icon = icon("th-large"), tabName = "o_formation"),
             menuSubItem("Personnel", icon = icon("th-large"), tabName = "o_personnel"),
             menuSubItem("Plays", icon = icon("th-large"), tabName = "o_play")
             ),
    menuItem("Defense", icon = icon("th-large"),
             menuSubItem("Summary", icon = icon("th-large"), tabName = "d_summary"),
             menuSubItem("Down", icon = icon("th-large"), tabName = "d_down"),
             menuSubItem("Run Pass", icon = icon("th-large"), tabName = "d_down"),
             menuSubItem("Formation", icon = icon("th-large"), tabName = "d_formation"),
             menuSubItem("Personnel", icon = icon("th-large"), tabName = "d_personnel"),
             menuSubItem("Plays", icon = icon("th-large"), tabName = "d_play")
             )
  )
)

body <- dashboardBody(
  tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
   # tags$link(rel = "icon", type = "image/png", href = "images/starticont_Vwm_icon.ico")),
  #fluidRow(column(width = 12, offset = 0,
                  #tags$img(src='images/mcfLong3.png', alt = 'MCF Capital Management')))
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  #data table
  
  
  #input fields
  
  
  #action buttons
  tabItems(
    tabItem(tabName = "play_entry",
            fluidRow(
                     column(width = 1, offset=4,shinyjs::disabled(textInput("id", "PLAY NUMBER", "0"))),
                     column(width = 1, offset=1, numericInput("DRIVE", "DRIVE NUMBER", 0, min=1))
                     ),
            fluidRow(
                     column(width=1, offset = 3,numericInput("O_SCORE","OCEANSIDE", 0)),
                     column(width=2, offset =1, numericInput("QTR","QTR", 1, width= "50%", min =1, max = 5),
                            radioButtons("ODK","",inline = TRUE, choices = c("OSIDE" = "O","OPP" = "D"), selected = "O" ,width="100%")),
                     column(width=1,numericInput("OPP_SCORE","OPPONENT", 0))
                            ),
            fluidRow(
              column(width=1, offset = 3,numericInput("DN","DOWN", 1, min = 1, max = 4)),
              column(width=1, numericInput("DIST","DIST", 0, min =0)),
              column(width=2, offset = 1, fluidRow(
                     column(width=3,radioButtons("SIDE","", choices = c("-"="-","+"="+"), selected = "PLUS")),
                     column(width=5,numericInput("YARD_LN","YDLN", 0, min =0, max= 50)),
                     column(width=4,textInput("HASH","HASH", "M") ))
                    )
            ),
            fluidRow(
              column(width =12 ,
                  rHandsontableOutput("hot"),
                  actionButton("submit", "Submit"),
                  actionButton("new", "New"),
                  actionButton("delete", "Delete")
              ),
              br(),
              br(),
            fluidRow(
              column(width = 12, DT::dataTableOutput("responses")))
            )),
    tabItem(tabName = "drive_summary",
            fluidRow(
              column(width = 3, selectInput("drive", "SELECT A DRIVE", choices = sort(unique(responses$DRIVE),TRUE)))
            ),
            fluidRow(
              column(width = 3, valueBoxOutput("ds_first_downs")),
              column(width = 3, valueBoxOutput("ds_total_yards")),
              column(width = 3, valueBoxOutput("ds_total_plays")),
              column(width = 3, valueBoxOutput("ds_yards_play"))
            ),
            fluidRow(
              column(width = 5, plotlyOutput("ds_rp")),
              column(width = 7, plotlyOutput("drive_plot"))
            ),
            fluidRow(
              column(12, DT::dataTableOutput("drive_sum"))
            )
            ))
)
 


dashboardPage(header,sidebar,body)
