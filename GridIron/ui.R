header <- dashboardHeader(title = "Gridiron") 

sidebar <- dashboardSidebar(
  sidebarUserPanel("Ocenside",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # Image file should be in www/ subdir
                   image = "images/o.jpg"
  ),
  sidebarMenu(
    menuItem("Pre Game", tabName = "pre_game", icon = icon("th-large")),
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
  tabItems(
    tabItem(tabName = "pre_game",
            tabsetPanel(
              tabPanel("Offense",
                       br(),
                              fluidRow(column(width = 6, textInput("e_form","ENTER OFFENSIVE FORMATIONS"),
                                       actionButton("of_form_submit", "Submit"),
                                       actionButton("of_form_delete", "Delete"),
                                       dataTableOutput("o_forms"))),
                                fluidRow(column(width = 6, textInput("e_oplays", "OSIDE PLAYS"),
                                       actionButton("of_play_submit", "Submit"),
                                       actionButton("of_play_delete", "Delete"),
                                       dataTableOutput("o_plays"))),
                       
                       column(width = 2, textInput("e_forms", "DEF FORMS")),
                       column(width = 2, textInput("e_cov","OPP COVERAGE")),
                       column(width = 2, textInput("e_blitz", "OPP BLITZES")),
                       column(width = 2, textInput("e_front","OPP FRONTS"))
                       )
            )
            ),
    tabItem(tabName = "play_entry",
            column(width = 12, fluidRow(column(width = 2, offset = 10, shinyjs::disabled(textInput("id", "PLAY", "0")))),
                                fluidRow(h3("SCOREBOARD"),
                                       column(width = 3, column(width = 6, numericInput("DRIVE", "DRIVE", 0, min=1)),
                                            column(width = 6, selectInput("QTR","QTR", choices = c(1,2,3,4,5), selected = 1))),
                                       column(width = 3, column(width = 3, radioButtons("SIDE",label = "", choices = c("-"="-","+"="+"), selected = "PLUS")),
                                            column(width = 6, numericInput("YARD_LN","YDLN", 0, min =0, max= 50)),
                                            column(width = 3,  radioButtons("ODK","SIDE",choices = c("O" = "O","D" = "D")))
                                            ),
                                       column(width = 3, column(width = 6, numericInput("OPP_SCORE","OPP", 0)),
                                            column(width = 6, numericInput("O_SCORE","OSIDE", 0), selected = "O")),
                                       column(width =3, column(width = 6, selectInput("DN","DOWN", choices = c(1,2,3,4), selected = 1)),
                                            column(width = 6, numericInput("DIST","DIST", 0, min = 0)))
                                       
                                       ),
                              column(width =12 ,
                                fluidRow(h3("PLAY ENTRY"),
                                       column(width = 3 ,selectInput("HASH","HASH", choices = c("L","M","R"), selected = "M")),
                                       column(width = 3, selectInput("PERSONNEL","PERSONNEL", choices = c(""), selected = NULL)),#selectInput("PERSONNEL","PERSONNEL", choices = if("ODK" == "O") getDDList("O_PERSONNEL") else getDDList("D_PERSONNEL"), selected = NULL)),
                                       column(width = 3, selectInput("OFF_FORM","OFF FORM", choices = c(""), selected = NULL)),
                                       column(width = 3, selectInput("DEF_FORM", "DEF FORM", choices = c(""), selected = NULL))
                                       ),
                                fluidRow(#h3("POST-PLAY"),
                                        column(width = 3, selectInput("PLAY_TYPE","PLAY TYPE", choices = c("RUN","PASS","SPECIAL"))),
                                        column(width = 3, selectInput("RESULT", "RESULT", choices = c("RUSH","COMPLETE","INCOMPLETE","FUMBLE","INTERCEPTION","SPECIAL"))),
                                        column(width = 3, textInput("GN_LS", "GN LS"))
                                         ),
                                fluidRow(#h3("PLAY INFO"),
                                        column(width = 3, selectInput("OFF_PLAY","OFF PLAY", choices = c(""), selected = NULL)),
                                        column(width = 3, selectInput("DEF_PLAY", "DEF PLAY", choices = c(""), selected = NULL)),
                                        column(width = 2, selectInput("COVERAGE", "COVERAGE", choices = c(""), selected = NULL)),
                                        column(width = 2, selectInput("BLITZ", "BLITZ", choices = c(""), selected = NULL)),
                                        column(width = 2, selectInput("FRONT", "FRONT", choices = c(""), selected = NULL))
                                         ))
                     ),
            fluidRow(
              column(width = 12 , column(width = 12, align = "right",
                  actionButton("submit", "Submit"),
                  actionButton("new", "New"),
                  actionButton("delete", "Delete")))
              ),
              br(),
              br(),
            fluidRow(
              column(width = 12, column(width = 12, DT::dataTableOutput("responses"))))
            ),
    tabItem(tabName = "drive_summary",
            fluidRow(
              column(width = 3, uiOutput('drive_list'))
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
                                            plotlyOutput("od_coverage_dist")),
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
                                           uiOutput("od_form_list"),
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
                                           uiOutput("oo_form_list"),
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
                                       uiOutput("operf_pers_list"),
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
                                              uiOutput("operf_form_list"),
                                              fluidRow(column(width = 3, valueBoxOutput("operf_run_form")),
                                                       column(width = 3, valueBoxOutput("operf_pass_form")),
                                                       column(width = 3, valueBoxOutput("operf_run_avg_form")),
                                                       column(width = 3, valueBoxOutput("operf_pass_avg_form"))
                                              ),
                                              column(width = 6, plotlyOutput("operf_form_form"),
                                                     plotlyOutput("operf_form_blitzes")),
                                              column(width = 6, plotlyOutput("operf_form_coverage"),
                                                     plotlyOutput("operf_form_front")))),
                              tabPanel("Play",
                                       column(width =12,
                                             uiOutput("operf_play_list"),
                                              fluidRow(column(width = 3, valueBoxOutput("operf_play_type")),
                                                       column(width = 3, valueBoxOutput("operf_play_ran")),
                                                       column(width = 3, valueBoxOutput("operf_play_yards")),
                                                       column(width = 3, valueBoxOutput("operf_play_avg"))
                                              ),
                                              column(width = 6, plotlyOutput("operf_play_form"),
                                                                plotlyOutput("operf_play_blitzes")),
                                              column(width = 6, plotlyOutput("operf_play_coverage"),
                                                                plotlyOutput("operf_play_front"))))
                            )
                     )
            )
    # tabItem(tabName = "d_summary",
    #         h1("Summary"),
    #         tabsetPanel(
    #           tabPanel("Oceanside",fluidRow(
    #             column(width = 9, br(),column(width = 12, plotlyOutput("os_rp")),
    #                    column(width = 12, plotlyOutput("os_top_plays")),
    #                    column(width = 12, plotlyOutput("os_top_pers")),
    #                    column(width = 12, plotlyOutput("os_top_forms"))
    #             ),
    #             column(width = 3, br(),br(),br(),br(),
    #                    column(width = 12, valueBoxOutput("os_first_downs")),
    #                    column(width = 12, valueBoxOutput("os_total_yards")),
    #                    column(width = 12, valueBoxOutput("os_total_plays")),
    #                    column(width = 12, valueBoxOutput("os_yards_play")),
    #                    column(width = 12, valueBoxOutput("os_run_yards")),
    #                    column(width = 12, valueBoxOutput("os_pass_yards")),
    #                    column(width = 12, valueBoxOutput("os_run_yards_play")),
    #                    column(width = 12, valueBoxOutput("os_pass_yards_play")),
    #                    column(width = 12, valueBoxOutput("os_completion_pct")),
    #                    column(width = 12, valueBoxOutput("os_drives")),
    #                    column(width = 12, valueBoxOutput("os_third_conv")),
    #                    column(width = 12, valueBoxOutput("os_fourth_conv"))
    #             )
    #           )),
    #           tabPanel("Defense",fluidRow(
    #             column(width = 12, br(),plotlyOutput("os_def_form"),
    #                    plotlyOutput("os_coverage"),
    #                    plotlyOutput("os_front"),
    #                    plotlyOutput("os_blitz")
    #             )
    #           )
    #           ))
    #         )
    )
) 


dashboardPage(header,sidebar,body)
