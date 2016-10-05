header <- dashboardHeader(title = "Gridiron") 

sidebar <- dashboardSidebar(
  sidebarUserPanel("Ocenside",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # Image file should be in www/ subdir
                   image = "images/o.jpg"
  ),
  sidebarMenu(
    menuItem("Play Entry",tabName = "play_entry", icon=icon("pencil")),
    menuItem("Drive Summary", tabName = "drive_summary", icon = icon("road", lib = "glyphicon")),
    menuItem("Offense", icon = icon("dollar"),
             menuSubItem("Summary", icon = icon("pie-chart"), tabName = "o_summary"),
             menuSubItem("Down", icon = icon("bar-chart"), tabName = "o_down"),
             menuSubItem("Formation", icon = icon("th-large"), tabName = "o_formation"),
             menuSubItem("Performance", icon = icon("bullseye"), tabName = "o_performance")
    ),
    menuItem("Defense", icon = icon("ambulance"),
             menuSubItem("Summary", icon = icon("pie-chart"), tabName = "d_summary"),
             menuSubItem("Down", icon = icon("bar-chart"), tabName = "d_down"),
             menuSubItem("Formation", icon = icon("th-large"), tabName = "d_formation"),
             menuSubItem("Performance", icon = icon("bullseye"), tabName = "d_performance")
    ),
    menuItem("Field Map", icon = icon("th"), tabName = "field_map"),
    menuItem("Film Room", tabName = "film_room", icon = icon("beer"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "icon", type = "image/png", href = "images/helmet.jpg")),
  #fluidRow(column(width = 12, offset = 0,
  #tags$img(src='images/mcfLong3.png', alt = 'MCF Capital Management')))
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  tabItems(
    tabItem(tabName = "play_entry",
            tabsetPanel(
              tabPanel("Scoreboard",
                       fluidRow(column(width = 2, offset = 10, shinyjs::disabled(textInput("id", "PLAY", "0")))),
                       fluidRow(column(width=12,h3("SCOREBOARD")),
                                column(width = 3, column(width = 6, numericInput("DRIVE", "DRIVE", 0, min=1)),
                                       column(width = 6, selectInput("QTR","QTR", choices = c(1,2,3,4,5), selected = 1))),
                                column(width = 3, column(width = 3, radioButtons("SIDE",label = "", choices = c("-"="-","+"="+"), selected = "+")),
                                       column(width = 6, numericInput("YARD_LN","YDLN", 0, min =0, max= 50)),
                                       column(width = 3,  radioButtons("ODK","SIDE",choices = c("O" = "O","D" = "D")))
                                ),
                                column(width = 3, column(width = 6, numericInput("OPP_SCORE","OPP", 0)),
                                       column(width = 6, numericInput("O_SCORE","OSIDE", 0), selected = "O")),
                                column(width =3, column(width = 6, selectInput("DN","DOWN", choices = c(1,2,3,4), selected = 1)),
                                       column(width = 6, numericInput("DIST","DIST", 0, min = 0)))
                       ),
                       fluidRow(column(width=12,h3("PLAY")),
                         column(width =12, 
                                column(width = 3 ,selectInput("HASH","HASH", choices = c("L","M","R"), selected = NULL)),
                                column(width = 3, selectInput("PLAY_TYPE","PLAY TYPE", choices = c("RUN","PASS","SPECIAL"), selected = NULL)),
                                column(width = 3, selectInput("RESULT", "RESULT", choices = c("RUSH","COMPLETE","INCOMPLETE","FUMBLE","INTERCEPTION","SPECIAL"), selected = NULL)),
                                column(width = 3, numericInput("GN_LS", "GN LS", 0))
                                )),
                       fluidRow(column(width=12,column(width=12 ,
                                         actionButton("submit_s", "Submit"),
                                         actionButton("new_s", "New"),
                                         actionButton("delete_s", "Delete")))),
                       fluidRow(column(width = 12,  DT::dataTableOutput("scoreboard")))
                       ),
              tabPanel("Offense", column(width = 2, offset = 9, shinyjs::disabled(textInput("id_o", "PLAY", "0"))),
                       column(width = 12, 
                       fluidRow(h3("BOTH"),
                                column(width = 2, radioButtons("ODK_O","SIDE",choices = c("O" = "O","D" = "D"),inline=TRUE)),
                                column(width = 3, selectInput("OFF_FORM","OFF FORM", choices = c(""), selected = NULL)),
                                column(width = 3, selectInput("OFF_PLAY","OFF PLAY", choices = c(""), selected = NULL)),
                                column(width = 3, selectInput("PERSONNEL", "PERSONNEL", choices = c(""), selected = NULL))),
                       fluidRow(h3("OCEANSIDE"),
                                column(width = 3, selectInput("OLINE","O-LINE", choices = c(""), selected = NULL))),
                       fluidRow(column(width =12 ,
                                       actionButton("submit_o", "Submit"),
                                       actionButton("new_o", "New"),
                                       actionButton("delete_o", "Delete")))),
                       fluidRow(column(width = 12, column(width = 12, DT::dataTableOutput("offense"))))
                       ),
              tabPanel("Defense", column(width = 2, offset = 9, shinyjs::disabled(textInput("id_d", "PLAY", "0"))),
                       column(width =12 ,
                       fluidRow(h3("BOTH"),
                                column(width = 2, radioButtons("ODK_D","SIDE",choices = c("O" = "O","D" = "D"),inline=TRUE)),
                                column(width = 3, selectInput("DEF_FORM", "DEF FORM", choices = c(""), selected = NULL))),
                       fluidRow(h3("OCEANSIDE"),
                                column(width = 3, selectInput("DEF_PLAY", "DEF PLAY", choices = c(""), selected = NULL))),
                       fluidRow(h3("OPPONENT"),
                                column(width = 2, selectInput("COVERAGE", "COVERAGE", choices = c(""), selected = NULL)),
                                column(width = 2, selectInput("BLITZ", "BLITZ", choices = c(""), selected = NULL)),
                                column(width = 2, selectInput("FRONT", "FRONT", choices = c(""), selected = NULL))),
                       fluidRow(column(width =12 ,
                                actionButton("submit_d", "Submit"),
                                actionButton("new_d", "New"),
                                actionButton("delete_d", "Delete")))),
                       fluidRow(column(width = 12, column(width = 12, DT::dataTableOutput("defense"))))
                       )
            )
            # column(width = 12, fluidRow(column(width = 2, offset = 10, shinyjs::disabled(textInput("id", "PLAY", "0")))),
            #        column(width =12 ,
            # 
            #               fluidRow(#h3("POST-PLAY"),
            #                 
            #               ),
            #               fluidRow(#h3("PLAY INFO"),
            #               ),
            #               fluidRow(
            #                 column(width = 9 , column(width = 12, align = "right",
            #                                           
            #               )
            #        )
            # ),
            # br(),
            # br(),
            # fluidRow(
            #   column(width = 12, column(width = 12, DT::dataTableOutput("responses"))))
    ),
    tabItem(tabName = "drive_summary",
            fluidRow(
              column(width = 3, uiOutput('drive_list')),
              column(width = 1, radioButtons("drive_odk", "Side", choices = c("O","D"), selected = "O"))
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
                                       br(),
                                       plotlyOutput("od_def_form_dn"),
                                       selectInput("def_form_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("od_def_form_dist")),
                              tabPanel("Coverage",
                                       br(),
                                       plotlyOutput("od_coverage_dn"),
                                       selectInput("coverage_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("od_coverage_dist")),
                              tabPanel("Front",
                                       br(),
                                       plotlyOutput("od_front_dn"),
                                       selectInput("front_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("od_front_dist")),
                              tabPanel("Blitz",
                                       br(),
                                       plotlyOutput("od_blitz_dn"),
                                       selectInput("blitz_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("od_blitz_dist")),
                              tabPanel("Offense",
                                       br(),
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
                       tabPanel("Summary",
                                br(),
                                fluidRow(column(width = 3, valueBoxOutput("operf_run_s")),
                                         column(width = 3, valueBoxOutput("operf_pass_s")),
                                         column(width = 3, valueBoxOutput("operf_run_avg_s")),
                                         column(width = 3, valueBoxOutput("operf_pass_avg_s"))
                                ),
                                column(width = 12, plotlyOutput("operf_top_pers")),
                                column(width = 12, plotlyOutput("operf_top_forms")),
                                column(width = 12, plotlyOutput("operf_top_olines")),
                                column(width = 12, plotlyOutput("operf_top_plays"))
                       ),
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
                                       br(),
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
                                       br(),
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
                       tabPanel("O-Line",
                                column(width =12,
                                       br(),
                                       uiOutput("operf_oline_list"),
                                       fluidRow(column(width = 3, valueBoxOutput("operf_oline_type")),
                                                column(width = 3, valueBoxOutput("operf_oline_ran")),
                                                column(width = 3, valueBoxOutput("operf_oline_yards")),
                                                column(width = 3, valueBoxOutput("operf_oline_avg"))
                                       ),
                                       column(width = 6, plotlyOutput("operf_oline_form"),
                                              plotlyOutput("operf_oline_blitzes")),
                                       column(width = 6, plotlyOutput("operf_oline_coverage"),
                                              plotlyOutput("operf_oline_front")))
                       ),
                       tabPanel("Play",
                                column(width =12,
                                       br(),
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
    ),
    tabItem(tabName = "d_summary",
            h1("Summary"),
            tabsetPanel(
              tabPanel("Offense",fluidRow(
                column(width = 9, br(),column(width = 12, plotlyOutput("des_rp_sum")),
                       column(width = 12, plotlyOutput("des_top_plays")),
                       column(width = 12, plotlyOutput("des_top_pers")),
                       column(width = 12, plotlyOutput("des_top_forms"))
                ),
                column(width = 3, br(),br(),br(),br(),
                       column(width = 12, valueBoxOutput("des_first_downs")),
                       column(width = 12, valueBoxOutput("des_total_yards")),
                       column(width = 12, valueBoxOutput("des_total_plays")),
                       column(width = 12, valueBoxOutput("des_yards_play")),
                       column(width = 12, valueBoxOutput("des_run_yards")),
                       column(width = 12, valueBoxOutput("des_pass_yards")),
                       column(width = 12, valueBoxOutput("des_run_yards_play")),
                       column(width = 12, valueBoxOutput("des_pass_yards_play")),
                       column(width = 12, valueBoxOutput("des_completion_pct")),
                       column(width = 12, valueBoxOutput("des_drives")),
                       column(width = 12, valueBoxOutput("des_third_conv")),
                       column(width = 12, valueBoxOutput("des_fourth_conv"))
                )
              )),
              tabPanel("Oceanside",fluidRow(
                column(width = 12, br(),plotlyOutput("des_plays"),
                       plotlyOutput("des_rp_play"),
                       plotlyOutput("des_def_form"),
                       plotlyOutput("des_rp_def_form")
                )
              )
              )
            )
    ),
    tabItem(tabName = "d_down",
            fluidRow(column(width = 12, h1("Down and Distance Analysis"),
                            tabsetPanel(
                              tabPanel("Formation",
                                       br(),
                                       plotlyOutput("oo_off_form_dn"),
                                       selectInput("off_form_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("oo_off_form_dist")),
                              tabPanel("Personnel",
                                       br(),
                                       plotlyOutput("oo_pers_dn"),
                                       selectInput("pers_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("oo_pers_dist")),
                              tabPanel("Play",
                                       br(),
                                       plotlyOutput("oo_play_dn"),
                                       selectInput("play_dn", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("oo_play_dist")),
                              tabPanel("Run Pass", 
                                       br(),
                                       plotlyOutput("oo_rp_dn"),
                                       selectInput("oo_rp_dn_a", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("oo_rp_dist")),
                              tabPanel("Defense",
                                       br(),
                                       plotlyOutput("oo_avg_yds_dn"),
                                       selectInput("oo_rp_yds_dn_a", "SELECT A DOWN", choices = c("1","2","3","4")),
                                       plotlyOutput("oo_avg_yds_dist"))                                          
                            )
            )
            )
            
    ),
    tabItem(tabName = "d_formation",
            fluidRow(column(width =12, h1("Formation Analysis"),
                            tabsetPanel(
                              tabPanel("Offense",
                                       uiOutput("do_form_list"),
                                       fluidRow(column(width = 3, valueBoxOutput("dfo_formation_count")),
                                                column(width = 3, valueBoxOutput("dfo_formation_yards")),
                                                column(width = 3, valueBoxOutput("dfo_formation_ryards")),
                                                column(width = 3, valueBoxOutput("dfo_formation_pyards"))
                                       ),
                                       column(width = 6, plotlyOutput("dfo_rp")), #Runpass breakdown
                                       column(width = 6, plotlyOutput("dfo_plays")) #Plays ran
                                       
                              ),
                              tabPanel("Deffense",
                                       uiOutput("dd_form_list"),
                                       fluidRow(column(width = 3, valueBoxOutput("dfd_formations_count")),
                                                column(width = 3, valueBoxOutput("dfd_formation_yards")),
                                                column(width = 3, valueBoxOutput("dfd_formation_ryards")),
                                                column(width = 3, valueBoxOutput("dfd_formation_pyards"))
                                       ),
                                       column(width = 6, plotlyOutput("dfd_rp"), #Run pass breakdown
                                              plotlyOutput("dfd_formations")), #top 5 formations
                                       column(width = 6, plotlyOutput("dfd_pers"), #personnel
                                              plotlyOutput("dfd_plays")) #top 5 plays Ran
                              )
                            )
            ))
            
            
    ),
    tabItem(tabName = "d_performance",
            fluidRow(column(width = 12, h1("Performance Analysis")),
                     tabsetPanel(
                       tabPanel("Summary",
                                br(),
                                fluidRow(column(width = 3, valueBoxOutput("dperf_run_s")),
                                         column(width = 3, valueBoxOutput("dperf_pass_s")),
                                         column(width = 3, valueBoxOutput("dperf_run_avg_s")),
                                         column(width = 3, valueBoxOutput("dperf_pass_avg_s"))
                                ),
                                column(width = 12, plotlyOutput("dperf_top_pers")),
                                column(width = 12, plotlyOutput("dperf_top_forms")),
                                column(width = 12, plotlyOutput("dperf_top_plays")),
                                column(width = 12, plotlyOutput("dperf_top_dplays"))
                       ),
                       tabPanel("Run Pass",
                                br(),
                                fluidRow(column(width = 3, valueBoxOutput("dperf_run")),
                                         column(width = 3, valueBoxOutput("dperf_pass")),
                                         column(width = 3, valueBoxOutput("dperf_run_avg")),
                                         column(width = 3, valueBoxOutput("dperf_pass_avg"))
                                ),
                                column(width = 6, plotlyOutput("dperf_rp_form")),
                                column(width = 6, plotlyOutput("dperf_rp_play"))),
                       tabPanel("Personnel",
                                column(width =12,
                                       br(),
                                       uiOutput("dperf_pers_list"),
                                       fluidRow(column(width = 3, valueBoxOutput("dperf_run_pers")),
                                                column(width = 3, valueBoxOutput("dperf_pass_pers")),
                                                column(width = 3, valueBoxOutput("dperf_run_avg_pers")),
                                                column(width = 3, valueBoxOutput("dperf_pass_avg_pers"))
                                       ),
                                       column(width = 6, plotlyOutput("dperf_pers_form")),
                                       column(width = 6, plotlyOutput("dperf_pers_play")))),
                       tabPanel("Formation",
                                column(width =12,
                                       br(),
                                       uiOutput("dperf_form_list"),
                                       fluidRow(column(width = 3, valueBoxOutput("dperf_run_form")),
                                                column(width = 3, valueBoxOutput("dperf_pass_form")),
                                                column(width = 3, valueBoxOutput("dperf_run_avg_form")),
                                                column(width = 3, valueBoxOutput("dperf_pass_avg_form"))
                                       ),
                                       column(width = 6, plotlyOutput("dperf_form_form")),
                                       column(width = 6, plotlyOutput("dperf_form_play")))),
                       tabPanel("Play",
                                column(width =12,
                                       br(),
                                       uiOutput("dperf_play_list"),
                                       fluidRow(column(width = 3, valueBoxOutput("dperf_play_type")),
                                                column(width = 3, valueBoxOutput("dperf_play_ran")),
                                                column(width = 3, valueBoxOutput("dperf_play_yards")),
                                                column(width = 3, valueBoxOutput("dperf_play_avg"))
                                       ),
                                       column(width = 6, plotlyOutput("dperf_play_form")),
                                       column(width = 6, plotlyOutput("dperf_play_play")))),
                       tabPanel("Defense",
                                column(width =12,
                                       br(),
                                       uiOutput("dperf_dplay_list"),
                                       fluidRow(column(width = 3, valueBoxOutput("dperf_run_dplay")),
                                                column(width = 3, valueBoxOutput("dperf_pass_dplay")),
                                                column(width = 3, valueBoxOutput("dperf_run_avg_dplay")),
                                                column(width = 3, valueBoxOutput("dperf_pass_avg_dplay"))
                                       ),
                                       column(width = 6, plotlyOutput("dperf_dplay_form")),
                                       column(width = 6, plotlyOutput("dperf_dplay_pers")),
                                       column(width = 12, plotlyOutput("dperf_dplay_play"))))
                       
                     )
            )
    ),
    tabItem(tabName = "field_map",
            tabsetPanel(
              tabPanel("Oceanside",
                       h1("Run Pass Breakdown by Hash"),
                       br(),
                       fluidRow(
                         column(width = 12, plotlyOutput("oside_b"),
                                            plotlyOutput("oside_m"),
                                            plotlyOutput("oside_r"))
                         )
                       ),
              tabPanel("Opponent",
                       h1("Run Pass Breakdown by Hash"),
                       br(),
                       fluidRow(
                         column(width = 12, plotlyOutput("opp_b"),
                                            plotlyOutput("opp_m"),
                                            plotlyOutput("opp_r"))
                       )
                       )
              )
            ),
    tabItem(tabName = "film_room",
            tabsetPanel(
              tabPanel("Data",
                       fluidRow(
                         column(width = 12, column(width = 12, fluidRow(h1("Play Data")))),
                         column(width = 5,
                                sidebarPanel(
                                  h3("Current Play Data"),
                                  downloadButton('downloadCurrent_plays', 'Download'),
                                  h3("Play Data Template"),
                                  downloadButton('downloadTemplate_plays', 'Download'),
                                  br(),
                                  h3("Upload Play Data"),
                                  fluidRow(
                                    column(width = 6, fileInput('pd', 'Choose CSV File',
                                                                accept=c('text/csv', 
                                                                         'text/comma-separated-values,text/plain', 
                                                                         '.csv'))),
                                    column(width = 1, br(),actionButton("pd_submit","Submit"))),
                                  column(width = 12, fluidRow(textOutput("pd_success"))),
                                  fluidRow(
                                    column(width = 12,
                                           h3("Start a New Game"))),
                                  fluidRow(column(width = 6, passwordInput("password", "Password:")),
                                           column(width = 1, br(),actionButton("new_game","New Game"))
                                  ),
                                  fluidRow(column(width =8, br(),textOutput("pass_text")))
                                )
                         ),
                         column(width = 7, column(width =12 ,
                                                  fluidRow(DT::dataTableOutput("currentPlays")))
                         ))
              ),
              tabPanel("Drop Downs",
                       fluidRow(
                         column(width = 12, column(width = 12, fluidRow(h1("Custom Lists")))),
                         column(width = 5,
                                sidebarPanel(
                                  h3("Current Drop Downs"),
                                  downloadButton('downloadCurrent', 'Download'),
                                  h3("Drop Down Template"),
                                  downloadButton('downloadTemplate', 'Download'),
                                  br(),
                                  h3("Custom Drop Down File"),
                                  fluidRow(
                                    column(width = 6, fileInput('dds', 'Choose CSV File',
                                                                accept=c('text/csv', 
                                                                         'text/comma-separated-values,text/plain', 
                                                                         '.csv'))),
                                    column(width = 1, br(),actionButton("dd_submit","Submit"))),
                                  fluidRow(column(width=12,br(),textOutput("dd_success")))
                                )
                         ),
                         column(width = 7,column(width =12, 
                                                 fluidRow(DT::dataTableOutput("customLists")))
                         )
                         
                       ))
            )
    )
  )
) 


dashboardPage(header,sidebar,body)
