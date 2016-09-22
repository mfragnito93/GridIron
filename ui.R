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
             menuSubItem("Formation", icon = icon("th-large"), tabName = "o_formation"),
             menuSubItem("Performance", icon = icon("th-large"), tabName = "o_performance")
             ),
    menuItem("Defense", icon = icon("th-large"),
             menuSubItem("Summary", icon = icon("th-large"), tabName = "d_summary"),
             menuSubItem("Down", icon = icon("th-large"), tabName = "d_down"),
             menuSubItem("Run Pass", icon = icon("th-large"), tabName = "d_rp"),
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
                     column(width = 2, offset=3,shinyjs::disabled(textInput("id", "PLAY NUMBER", "0"))),
                     column(width = 2, offset=1, numericInput("DRIVE", "DRIVE NUMBER", 0, min=1))
                     ),
            fluidRow(
                     column(width=2, offset = 2,numericInput("O_SCORE","OCEANSIDE", 0)),
                     column(width=2, offset =1, numericInput("QTR","QTR", 1, width= "50%", min =1, max = 5),
                            radioButtons("ODK","",inline = TRUE, choices = c("OSIDE" = "O","OPP" = "D"), selected = "O" ,width="100%")),
                     column(width=2,numericInput("OPP_SCORE","OPPONENT", 0))
                            ),
            fluidRow(
              column(width=1, offset = 3,numericInput("DN","DOWN", 1, min = 1, max = 4)),
              column(width=1, numericInput("DIST","DIST", 0, min =0)),
              column(width=3, offset = 1, fluidRow(
                     column(width=2,radioButtons("SIDE",label = "", choices = c("-"="-","+"="+"), selected = "PLUS")),
                     column(width=9,numericInput("YARD_LN","YDLN", 0, min =0, max= 50)),
                     column(width=4,textInput("HASH","HASH", "M") )
                     )
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
            ),
    tabItem(tabName = "o_summary",
            h1("Summary"),
            tabsetPanel(
                  tabPanel("Oceanside",fluidRow(
                        column(width = 9, br(),column(width = 12, plotlyOutput("os_rp")),
                                          column(width = 12, plotlyOutput("os_top_plays")),
                                          column(width = 12, plotlyOutput("os_top_pers")),
                                          column(width = 12, plotlyOutput("os_top_forms"))
                               ),
                        column(width = 3, br(),br(),br(),br(),
                                          column(width = 12, valueBoxOutput("os_first_downs")),
                                          column(width = 12, valueBoxOutput("os_total_yards")),
                                          column(width = 12, valueBoxOutput("os_total_plays")),
                                          column(width = 12, valueBoxOutput("os_yards_play")),
                                          column(width = 12, valueBoxOutput("os_run_yards")),
                                          column(width = 12, valueBoxOutput("os_pass_yards")),
                                          column(width = 12, valueBoxOutput("os_run_yards_play")),
                                          column(width = 12, valueBoxOutput("os_pass_yards_play")),
                                          column(width = 12, valueBoxOutput("os_completion_pct")),
                                          column(width = 12, valueBoxOutput("os_drives")),
                                          column(width = 12, valueBoxOutput("os_third_conv")),
                                          column(width = 12, valueBoxOutput("os_fourth_conv"))
                               )
                        )),
                  tabPanel("Defense",fluidRow(
                                          column(width = 12, br(),plotlyOutput("os_def_form"),
                                                             plotlyOutput("os_coverage"),
                                                             plotlyOutput("os_front"),
                                                             plotlyOutput("os_blitz")
                                                )
                                              )
                  ))
              
            ),
    tabItem(tabName = "o_down", 
               fluidRow(column(width = 12, h1("Down and Distance Analysis"),
                               tabsetPanel(
                                  tabPanel("Formation",
                                            plotlyOutput("od_def_form_dn"),
                                            selectInput("def_form_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                            plotlyOutput("od_def_form_dist")),
                                  tabPanel("Coverage",
                                            plotlyOutput("od_coverage_dn"),
                                            selectInput("coverage_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                            plotlyOutput("coverage_dist")),
                                  tabPanel("Front",
                                            plotlyOutput("od_front_dn"),
                                            selectInput("front_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                            plotlyOutput("od_front_dist")),
                                  tabPanel("Blitz", 
                                            plotlyOutput("od_blitz_dn"),
                                            selectInput("blitz_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                            plotlyOutput("od_blitz_dist")),
                                  tabPanel("Offense",
                                            plotlyOutput("od_avg_yds_dn"),
                                            selectInput("rp_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                            plotlyOutput("od_avg_yds_dist"))                                          
                                           )
                                  )
                       )
                      
            ),
    tabItem(tabName = "o_formation",
                fluidRow(column(width =12, h1("Formation Analysis"),
                                tabsetPanel(
                                  tabPanel("Defense",
                                           selectInput("od_formation", "SELECT A FORMATION", choices = unique(filter(rpOnly(responses),ODK=="O")$DEF_FORM)),
                                           fluidRow(column(width = 3, valueBoxOutput("ofd_formation_count")),
                                                    column(width = 3, valueBoxOutput("ofd_formation_yards")),
                                                    column(width = 3, valueBoxOutput("ofd_formation_ryards")),
                                                    column(width = 3, valueBoxOutput("ofd_formation_pyards"))
                                                    ),
                                           column(width = 6, plotlyOutput("ofd_coverages")),
                                           column(width = 6, plotlyOutput("ofd_blitzes"),
                                                             plotlyOutput("ofd_fronts"))
                                           ),
                                  tabPanel("Offense",
                                           selectInput("oo_formation", "SELECT A FORMATION", choices = unique(filter(rpOnly(responses),ODK=="O")$OFF_FORM)),
                                           fluidRow(column(width = 3, valueBoxOutput("ofo_formations_count")),
                                                    column(width = 3, valueBoxOutput("ofo_formation_yards")),
                                                    column(width = 3, valueBoxOutput("ofo_formation_ryards")),
                                                    column(width = 3, valueBoxOutput("ofo_formation_pyards"))
                                           ),
                                           column(width = 6, plotlyOutput("ofo_formations"),
                                                             plotlyOutput("ofo_coverages")),
                                           column(width = 6, plotlyOutput("ofo_blitzes"),
                                                             plotlyOutput("ofo_fronts"))
                                           )
                                )
                                ))
            
            ),
    tabItem(tabName = "o_performance",
            fluidRow(column(width = 12, h1("Performance Analysis")),
                            tabsetPanel(
                              tabPanel("Run Pass",
                                       br(),
                                       fluidRow(column(width = 3, valueBoxOutput("operf_run")),
                                                column(width = 3, valueBoxOutput("operf_pass")),
                                                column(width = 3, valueBoxOutput("operf_run_avg")),
                                                column(width = 3, valueBoxOutput("operf_pass_avg"))
                                       ),
                                       column(width = 6, plotlyOutput("operf_rp_form"),
                                                         plotlyOutput("operf_rp_blitzes")),
                                       column(width = 6, plotlyOutput("operf_rp_coverage"),
                                                         plotlyOutput("operf_rp_front"))),
                              tabPanel("Personnel",
                                       column(width =12,
                                       selectInput("operf_personnel", "SELECT A PERSONNEL", choices = unique(filter(rpOnly(responses),ODK=="O")$PERSONNEL)),
                                       fluidRow(column(width = 3, valueBoxOutput("operf_run_pers")),
                                                column(width = 3, valueBoxOutput("operf_pass_pers")),
                                                column(width = 3, valueBoxOutput("operf_run_avg_pers")),
                                                column(width = 3, valueBoxOutput("operf_pass_avg_pers"))
                                       ),
                                       column(width = 6, plotlyOutput("operf_pers_form"),
                                                         plotlyOutput("operf_pers_blitzes")),
                                       column(width = 6, plotlyOutput("operf_pers_coverage"),
                                                         plotlyOutput("operf_pers_front")))),
                              tabPanel("Formation",
                                       column(width =12,
                                              selectInput("operf_form", "SELECT A FORMATION", choices = unique(filter(rpOnly(responses),ODK=="O")$OFF_FORM)),
                                              fluidRow(column(width = 3, valueBoxOutput("operf_run_form")),
                                                       column(width = 3, valueBoxOutput("operf_pass_form")),
                                                       column(width = 3, valueBoxOutput("operf_run_avg_form")),
                                                       column(width = 3, valueBoxOutput("operf_pass_avg_form"))
                                              ),
                                              column(width = 6, plotlyOutput("operf_form_form"),
                                                     plotlyOutput("operf_form_blitzes")),
                                              column(width = 6, plotlyOutput("operf_form_coverage"),
                                                     plotlyOutput("operf_form_front")))),
                              tabPanel("Play")
                            )
                     )
            )
    )
)
 


dashboardPage(header,sidebar,body)
