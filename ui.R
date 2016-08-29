library(shiny)
library(shinydashboard)
library(rhandsontable)

header <- dashboardHeader(title = "Gridiron") 

sidebar <- dashboardSidebar(
  sidebarUserPanel("Ocenside",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # Image file should be in www/ subdir
                   image = "chalkboard-black.jpg"
  ),
  sidebarMenu(
    menuItem("Play Entry",tabName = "play_entry", icon=icon("th-large")),
    menuItem("Test", tabName = "test", icon = icon("th-large"))
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
            h2("Play Entry"),
            fluidRow(
              box(width =12 ,
                  h2("Play Number"),
                  shinyjs::disabled(textInput("id", "Id", "0")),
                  actionButton("submit", "Submit"),
                  actionButton("new", "New"),
                  actionButton("delete", "Delete"),
                  rHandsontableOutput("hot")
              ),
            fluidRow(
              box(width = 12, DT::dataTableOutput("responses")))
            ))
))



dashboardPage(header,sidebar,body)
