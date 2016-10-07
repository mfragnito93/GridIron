
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output, session) {
  ###Timers###
  refreshDataTables <- reactiveTimer(1000,session)
  
  ##Data###
  data <- reactivePoll(1000,session,GetMaxStamp,GetData)
  
  ###PRE-GAMe ENTRIES
  output$downloadCurrent  <- downloadHandler(
    filename = function() { 
      paste('Dropdown Current', '.csv', sep='') 
    },
    content = function(file) {
      out <- preSetDDs
      colnames(out) <- preSetHeaderFriendly
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  
  output$downloadTemplate <- downloadHandler(
    filename = function() { 
      paste('DropDown Template', '.csv', sep='') 
    },
    content = function(file) {
      out <- preSetDDsTemplate
      colnames(out) <- preSetHeaderFriendly
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$customLists <- DT::renderDataTable({
    
    inFile <- input$dds
    
    if (is.null(inFile))
      return(preSetDDs)
    
    read.csv(inFile$datapath, stringsAsFactors = FALSE)
    
  }, colnames = preSetHeaderFriendly, server = FALSE, selection = "single",options = list(scrollX = TRUE, autoWidth =TRUE), rownames = FALSE)
  
  observeEvent(input$dd_submit,{
    inFile <- input$dds
    preSetDDs <<- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    colnames(preSetDDs)<<-preSetHeader
    write.csv(preSetDDs,file = preSetDDPath, row.names = FALSE)
    output$dd_success <- renderText({
      req(input$dd_submit)
      "Your Dropdowns have been succefully uploaded"
    })
  }, priority = 1)
  
  #Export and Import data
  
  output$downloadCurrent_plays <- downloadHandler(
    filename = function() { 
      paste('Current Plays', '.csv', sep='') 
    },
    content = function(file) {
      write.csv(GetData(), file, row.names = FALSE)
    }
  )
  
  
  output$downloadTemplate_plays <- downloadHandler(
    filename = function() { 
      paste('Plays Template', '.csv', sep='') 
    },
    content = function(file) {
      write.csv(preSetPDTemplate, file, row.names = FALSE)
    }
  )
  
  output$currentPlays <- DT::renderDataTable({
    input$submit
    input$delete
    input$new_game
    
    inFile <- input$pd
    
    if (is.null(inFile))
      return(data())
    
    read.csv(inFile$datapath,stringsAsFactors = FALSE)
    
  }, server = FALSE, selection = "single",options = list(order = list(0, 'asc'), scrollX = TRUE, autoWidth =TRUE), rownames = FALSE)
  
  
  observeEvent(input$pd_submit,{
    inFile <- input$pd
    DataSplit(read.csv(inFile$datapath, stringsAsFactors = FALSE))
    output$pd_success <- renderText({
      req(input$pd_submit)
      "Your Plays have been succefully uploaded"
    })
  }, priority = 1)
  
  #New Game
  observeEvent(input$new_game,{
    output$pass_text <- renderText({
      if(isolate(input$password)=="oceanside"){
        #Create archive
        write.csv(GetData(), file =paste(playArchive,paste(gsub(":","-",Sys.time()),".csv", sep=""),sep=""), row.names = FALSE)
        DataSplit(read.csv(preSetPDTemplatePath, stringsAsFactors = FALSE))
        UpdateScoreboard(CreateDefaultRecord(scoreboardDefault,"scoreboard"),session)
        return("Successfully created a new game")
      } else return("Password is wrong try again")
    })
  })
  
  
  ###PLAY ENTRY
  observe({
    input$submit_s
    input$submit_o
    input$submit_d
    input$delete_s
    input$delete_o
    input$delete_d
    refreshDataTables()
    UpdateNextPlay(session)  
  })
  
  #scoreboard
  formData_s <- reactive({
    entry<-sapply(GetMetadataNames(scoreboardMeta), function(x) input[[x]])
    default <- CreateDefaultRecord(scoreboardDefault,"scoreboard")[,!colnames(CreateDefaultRecord(scoreboardDefault,"scoreboard")) %in% names(entry)]
    c(entry,default)
  })
  
  observeEvent(input$submit_s, {
    if (input$id != "0") {
      UpdateData(formData_s(),"scoreboard")
      UpdateScoreboard(ScoreBoardCalc(), session)
    } else {
      CreateData(formData_s(),"scoreboard")
      UpdateScoreboard(ScoreBoardCalc(), session)
    }
  }, priority = 1)
  
  observeEvent(input$new_s, {
     UpdateScoreboard(ScoreBoardCalc(), session)
  })
  
  observeEvent(input$delete_s, {
    DeleteData(formData_s(),"scoreboard")
    UpdateScoreboard(ScoreBoardCalc(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$scoreboard_rows_selected, {
    if (length(input$scoreboard_rows_selected) > 0) {
      data <- ReadData("scoreboard")[input$scoreboard_rows_selected, ]
      UpdateScoreboard(data,session)
    }
  })
  
  scoreboard <- reactivePoll(1000,session,GetSStamp,ReadScore)
  
  
  # display table
  output$scoreboard <- DT::renderDataTable({
    input$submit_s
    input$delete_s
    input$pd_submit
    input$new_game
    scoreboard()
  }, server = FALSE, selection = "single",
  colnames = GetMetadataValues(scoreboardMeta),options=list(order = list(0, 'desc'), scrollX = TRUE, sDom  = '<"top">rt<"bottom">ifp'),rownames = FALSE
  ) 

  UpdateScoreboard(ScoreBoardCalc(),session)
  UpdateOffenseForm("D",session)
  UpdateDefenseForm(updateODK(),session)
  

  
  ######offense####
  observe({
    input$pd_submit
    input$dd_submit
    input$offense_rows_selected
    if(length(input$offense_rows_selected) > 0) UpdateOffenseTable(ReadData("offense")[input$offense_rows_selected, ],session) else UpdateOffenseForm(input$ODK_O,session)
  })

  
  formData_o <- reactive({
    entry<-sapply(GetMetadataNames(offenseMeta), function(x) input[[x]])
    default <- CreateDefaultRecord(offenseDefault,"offense")[,!colnames(CreateDefaultRecord(offenseDefault,"offense")) %in% names(entry)]
    c(entry,default)
  })
  
  observeEvent(input$submit_o, {
    if (input$id_o != "0") {
      UpdateData(formData_o(),"offense")
      UpdateOffenseForm(updateODK(),session)
    } else {
      CreateData(formData_o(),"offense")
      UpdateOffenseForm(updateODK(),session)
    }
  }, priority = 1)
  
  observeEvent(input$new_o, {
    UpdateOffenseTable(CreateDefaultRecord(offenseDefault,"offense"),session)
    UpdateOffenseForm(updateODK(),session)
  })
  
  observeEvent(input$delete_o, {
    DeleteData(formData_o(),"offense")
    UpdateOffenseForm(updateODK(),session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$offense_rows_selected, {
    if (length(input$offense_rows_selected) > 0) {
      data <- ReadData("offense")[input$offense_rows_selected, ]
      UpdateOffenseTable(data,session)
    }
  })
  
  offense <- reactivePoll(1000,session,GetOStamp,ReadO)
  
  # display table
  output$offense <- DT::renderDataTable({
    input$submit_o
    input$delete_o
    input$pd_submit
    input$new_game
    offense()
  }, server = FALSE, selection = "single",
  colnames = GetMetadataValues(offenseMeta),options=list(order = list(0, 'desc'), scrollX = TRUE, sDom  = '<"top">rt<"bottom">ifp'),rownames = FALSE
  ) 
  
  
  
  
  ####Defense###
  
  observe({
    input$pd_submit
    input$dd_submit
    input$defense_rows_selected
    if(length(input$defense_rows_selected) > 0) UpdateDefenseTable(ReadData("defense")[input$defense_rows_selected, ],session) else UpdateDefenseForm(input$ODK_D,session)
  })
  
  
  formData_d <- reactive({
    entry<-sapply(GetMetadataNames(defenseMeta), function(x) input[[x]])
    default <- CreateDefaultRecord(defenseDefault,"defense")[,!colnames(CreateDefaultRecord(defenseDefault,"defense")) %in% names(entry)]
    c(entry,default)
  })
  
  observeEvent(input$submit_d, {
    if (input$id_d != "0") {
      UpdateData(formData_d(),"defense")
      UpdateDefenseForm(updateODK(),session)
    } else {
      CreateData(formData_d(),"defense")
      UpdateDefenseForm(updateODK(),session)
    }
  }, priority = 1)
  
  observeEvent(input$new_d, {
    UpdateDefenseTable(CreateDefaultRecord(defenseDefault,"defense"),session)
    UpdateDefenseForm(updateODK(),session)
  })
  
  observeEvent(input$delete_d, {
    DeleteData(formData_d(),"defense")
    UpdateDefenseForm(updateODK(),session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$defense_rows_selected, {
    if (length(input$defense_rows_selected) > 0) {
      data <- ReadData("defense")[input$defense_rows_selected, ]
      UpdateDefenseTable(data,session)
    }
  })
  
  defense <- reactivePoll(1000,session,GetDStamp,ReadD)
  
  # display table
  output$defense <- DT::renderDataTable({
    input$submit_d
    input$delete_d
    input$pd_submit
    input$new_game
    defense()
  }, server = FALSE, selection = "single",
  colnames = GetMetadataValues(defenseMeta),options=list(order = list(0, 'desc'), scrollX = TRUE, sDom  = '<"top">rt<"bottom">ifp'),rownames = FALSE
  ) 
  
  
  
  ###DRIVE SUMMARY
  output$drive_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("drive", "SELECT A DRIVE", choices = sort(unique(filter(data(),ODK == input$drive_odk)$DRIVE),TRUE))
  })
  
  
  drive_summary <- reactive({
    input$submit
    input$delete
    input$pd_submit
    driveSummary(data(),input$drive,input$drive_odk)
  })
  
  theDrive <- reactive({
    input$submit
    input$delete
    input$pd_submit
    drive(data(),input$drive)
  })
  
  
  metaSelect <- function(odk){
    if(odk=="O") return(GetMetadataValues(driveSummaryMetaO)) else return(GetMetadataValues(driveSummaryMetaD))
  }
  
  output$drive_sum <- DT::renderDataTable({
    input$submit
    input$delete
    input$pd_submit
    input$drive_odk
    datatable(drive_summary(),  selection = "single",
              colnames = metaSelect(input$drive_odk), options=list(order = list(0, 'asc'), scrollX = FALSE, autoWidth = TRUE, sDom  = '<"top">rt<"bottom">ifp'),rownames = FALSE)
  }, server = FALSE
  ) 
  
  #number of first downs
  output$ds_first_downs <-renderValueBox({
    valueBox(
      countFactor(drive_summary(),"DN",1)-1, "FIRST DOWNS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ds_total_plays <- renderValueBox({
    valueBox(
      length(drive_summary()[,1]), "TOTAL PLAYS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ds_total_yards <- renderValueBox({
    valueBox(
      sum(theDrive()$GN_LS), "TOTAL YARDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ds_yards_play <- renderValueBox({
    valueBox(
      round(sum(theDrive()$GN_LS)/length(drive_summary()[,1]),2), "YARDS PER PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ds_rp <- renderPlotly({
    plot.donut(colSort(getTable(drive_summary(),"PLAY_TYPE"),"Var1"), title = "Run Pass Breakdown", showLegend = F)
  })
  
  output$drive_plot<-renderPlotly({
    plot.drive(theDrive(),input$drive)
  })
  
  #Oceanside
  oSide <- reactive({
    input$submit
    input$delete
    input$pd_submit
    rpOnly(filter(data(),ODK=="O"))
  })
  
  #Opponent
  oSideD <- reactive({
    input$submit
    input$delete
    input$pd_submit
    rpOnly(filter(data(),ODK == "D"))
  })
  
  output$oside <- reactive({
    rpOnly(oSide())
  })
  
  output$opp <- reactive({
    rpOnly(oSideD())
  })
  
  ########Offense Summary
  output$os_rp <- renderPlotly({
    plot.donut(colSort(getTable(oSide(),"PLAY_TYPE"),"Var1"), title = "Run Pass Breakdown", showLegend = T)
  })
  
  
  output$os_top_plays <- renderPlotly({
    plot.oneBar(getN(getTable(oSide(),"OFF_PLAY"),"value"), title = "Top 5 Plays Ran")
  })
  
  output$os_top_forms <- renderPlotly({
    plot.oneBar(getN(getTable(oSide(),"OFF_FORM"),"value"), title = "Top 5 Formations Ran")
  })
  
  output$os_top_pers <- renderPlotly({
    plot.donut(getTable(oSide(),"PERSONNEL"), title = "Personnel Breakdown", showLegend = T)
  })
  
  output$os_total_yards<-renderValueBox({
    valueBox(
      sum(oSide()$GN_LS), "TOTAL YARDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_first_downs<-renderValueBox({
    valueBox(
      countFactor(oSide(),"DN",1)-length(unique(oSide()$DRIVE)), "FIRST DOWNS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_total_plays <- renderValueBox({
    valueBox(
      length(oSide()[,1]), "TOTAL PLAYS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_yards_play <- renderValueBox({
    valueBox(
      round(sum(oSide()$GN_LS)/length(oSide()[,1]),2), "YARDS PER PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_run_yards <- renderValueBox({
    valueBox(
      sum(filter(oSide(),PLAY_TYPE=="RUN")$GN_LS), "RUN YARDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_pass_yards <- renderValueBox({
    valueBox(
      sum(filter(oSide(),PLAY_TYPE=="PASS")$GN_LS), "PASS YARDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$os_run_yards_play <- renderValueBox({
    valueBox(
      round(sum(filter(oSide(),PLAY_TYPE=="RUN")$GN_LS)/length(filter(oSide(),PLAY_TYPE == "RUN")[,1]),2), "RUN YARDS PER PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_pass_yards_play <- renderValueBox({
    valueBox(
      round(sum(filter(oSide(),PLAY_TYPE=="PASS")$GN_LS)/length(filter(oSide(),PLAY_TYPE == "PASS")[,1]),2), "RUN YARDS PER PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_completion_pct <- renderValueBox({
    valueBox(
      paste0(round(length(filter(oSide(),RESULT=="COMPLETE")$PLAY_TYPE)/length(filter(oSide(),PLAY_TYPE == "PASS")[,1]),2)*100,"%"), "COMPLETION PCT", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_drives <- renderValueBox({
    valueBox(
      length(unique(filter(oSide(),PLAY_TYPE=="PASS")$DRIVE)), "DRIVES", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_third_conv <- renderValueBox({
    valueBox(
      paste0(round(dnConv(oSide(),3),2)*100,"%"), "3rd DN CONVERSION", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_fourth_conv <- renderValueBox({
    valueBox(
      paste0(round(dnConv(oSide(),4),2)*100,"%"), "4th DN CONVERSION", icon = icon("list"),
      color = "black"
    )
  })
  
  output$os_def_form <- renderPlotly({
    plot.oneBar(getTable(oSide(),c("DEF_FORM")), title = "Formations Ran")
  })
  
  output$os_coverage <- renderPlotly({
    plot.oneBar(getTable(oSide(),c("COVERAGE")), title = "Coverages Ran")
  })
  
  output$os_front <- renderPlotly({
    plot.oneBar(getTable(oSide(),c("FRONT")), title = "Fronts Ran")
  })
  
  output$os_blitz <- renderPlotly({
    plot.oneBar(getTable(oSide(),c("BLITZ")), title = "Blitzes Ran")
  })
  
  ####################
  
  ################Down
  output$od_def_form_dn <- renderPlotly({
    plot.bars(getTable(oSide(),c("DEF_FORM","DN")),"stack", title = "Formations by Down")
  })
  
  #DN and Distance
  output$od_def_form_dist <- renderPlotly({
    plot.bars(getTable(addDistBucket(filter(oSide(),DN==input$def_form_dn)),c("DEF_FORM","DIST_BUCKET")),"stack", title = "Formations by Down & Dist")
  })
  
  
  output$od_coverage_dn <- renderPlotly({
    plot.bars(getTable(oSide(),c("COVERAGE","DN")),"stack", title = "Coverages by Down")
  })
  
  #DN and Distance
  output$od_coverage_dist <- renderPlotly({
    plot.bars(getTable(addDistBucket(filter(oSide(),DN==input$coverage_dn)),c("COVERAGE","DIST_BUCKET")),"stack", title = "Coverages by Down & Dist")
  })
  
  
  output$od_front_dn <- renderPlotly({
    plot.bars(getTable(oSide(),c("FRONT","DN")),"stack", title = "Fronts by Down")
  })
  
  #DN and Distance
  output$od_front_dist <- renderPlotly({
    plot.bars(getTable(addDistBucket(filter(oSide(),DN==input$front_dn)),c("FRONT","DIST_BUCKET")),"stack", title = "Fronts by Down & Dist")
  })
  
  output$od_blitz_dn <- renderPlotly({
    plot.bars(getTable(oSide(),c("BLITZ","DN")),"stack", title = "Blitzes by Down")
  })
  
  output$od_blitz_dist <- renderPlotly({
    plot.bars(getTable(addDistBucket(filter(oSide(),DN==input$blitz_dn)),c("BLITZ","DIST_BUCKET")),"stack", title = "Blitzes by Down & Dist")
  })
  
  
  output$od_avg_yds_dn <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"DN"), "stack", title = "Avg Yards Gained by Down")
  })
  
  output$od_avg_yds_dist <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(addDistBucket(filter(oSide(),DN==input$rp_dn)),"DIST_BUCKET"), "stack", title = "Avg Yards Gained by Down & Dist")
  })
  
  ######################
  
  ######################
  ##FORMATIONS
  output$od_form_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("od_formation", "SELECT A FORMATION", choices = unique(rpOnly(oSide())$DEF_FORM))
  })
  
  output$oo_form_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("oo_formation", "SELECT A FORMATION", choices = unique(rpOnly(oSide())$OFF_FORM))
  })
  
  
  output$ofd_formation_count <- renderValueBox({
    valueBox(
      length(defForm(oSide(),input$od_formation)$DRIVE), "PLAYS RUN", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ofd_formation_yards <- renderValueBox({
    valueBox(
      sum(defForm(oSide(),input$od_formation)$GN_LS), "YDS AGNST", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ofd_formation_pyards <- renderValueBox({
    valueBox(
      sum(filter(defForm(oSide(),input$od_formation), PLAY_TYPE == "PASS")$GN_LS), "PASS YDS AGNST", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$ofd_formation_ryards <- renderValueBox({
    valueBox(
      sum(filter(defForm(oSide(),input$od_formation), PLAY_TYPE == "RUN")$GN_LS), "RUN YDS AGNST", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ofo_formations_count <- renderValueBox({
    valueBox(
      length(offForm(oSide(),input$oo_formation)$DRIVE), "PLAYS RAN", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ofo_formation_yards <- renderValueBox({
    valueBox(
      sum(offForm(oSide(),input$oo_formation)$GN_LS), "TOTAL YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$ofo_formation_pyards <- renderValueBox({
    valueBox(
      sum(filter(offForm(oSide(),input$oo_formation), PLAY_TYPE == "PASS")$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$ofo_formation_ryards <- renderValueBox({
    valueBox(
      sum(filter(offForm(oSide(),input$oo_formation), PLAY_TYPE == "RUN")$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$ofd_coverages <- renderPlotly({
    plot.donut(getTable(defForm(oSide(),input$od_formation),c("COVERAGE")), title = "Coverage Breakdown", showLegend = T)
  })
  
  output$ofd_blitzes <- renderPlotly({
    plot.donut(getTable(defForm(oSide(),input$od_formation),c("BLITZ")), title = "Blitz Breakdwon", showLegend = T)
  })
  
  output$ofd_fronts <- renderPlotly({
    plot.donut(getTable(defForm(oSide(),input$od_formation),c("FRONT")), title = "Front Breakdown", showLegend = T)
  })
  
  output$ofo_formations <- renderPlotly({
    plot.donut(getTable(offForm(oSide(),input$oo_formation),c("DEF_FORM")), title = "Def Formation Breakdown", showLegend = T)
  })
  
  output$ofo_coverages<- renderPlotly({
    plot.donut(getTable(offForm(oSide(),input$oo_formation),c("COVERAGE")), title = "Coverage Breakdown", showLegend = T)
  })
  
  output$ofo_blitzes <- renderPlotly({
    plot.donut(getTable(offForm(oSide(),input$oo_formation),c("BLITZ")), title = "Blitz Breakdown", showLegend = T)
  })
  
  output$ofo_fronts <- renderPlotly({
    plot.donut(getTable(offForm(oSide(),input$oo_formation),c("FRONT")), title = "Front Breakdown", showLegend = T)
  })
  
  ######################
  
  #####################
  
  ###PERFORMANCE
  ##Summary
  output$operf_pass_s <- renderValueBox({
    valueBox(
      sum(filter(oSide(), PLAY_TYPE == "PASS")$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_run_s <- renderValueBox({
    valueBox(
      sum(filter(oSide(),PLAY_TYPE == "RUN")$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_pass_avg_s <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),PLAY_TYPE == "PASS")$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_run_avg_s <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),PLAY_TYPE == "RUN")$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  }) 
  
  #RUN PASS
  output$operf_pass <- renderValueBox({
    valueBox(
      sum(filter(oSide(), PLAY_TYPE == "PASS")$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_run <- renderValueBox({
    valueBox(
      sum(filter(oSide(),PLAY_TYPE == "RUN")$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_pass_avg <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),PLAY_TYPE == "PASS")$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_run_avg <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),PLAY_TYPE == "RUN")$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_rp_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"DEF_FORM"), title = "Average YDs Against Def Formations", stack = "stack")
  })
  
  output$operf_rp_coverage <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"COVERAGE"), title = "Average YDs Against Coverages", stack = "stack")
  })
  
  output$operf_rp_blitzes <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"BLITZ"), title = "Average YDs Against Blitzes", stack = "stack")
  })
  
  output$operf_rp_front <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"FRONT"), title = "Average YDs Against Fronts", stack = "stack")
  })
  
  ##Personnel
  output$operf_top_pers <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"PERSONNEL"), title = "Average YDs by Personnel", stack = "stack", showLegend = T)
  })
  
  output$operf_pers_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("operf_personnel", "SELECT A PERSONNEL", choices = unique(rpOnly(oSide())$PERSONNEL))
  })
  
  output$operf_pass_pers <- renderValueBox({
    valueBox(
      sum(filter(oSide(), PLAY_TYPE == "PASS", PERSONNEL == replaceNull(input$operf_personnel))$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_run_pers <- renderValueBox({
    valueBox(
      sum(filter(oSide(),PLAY_TYPE == "RUN", PERSONNEL == replaceNull(input$operf_personnel))$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_pass_avg_pers <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),PLAY_TYPE == "PASS", PERSONNEL == replaceNull(input$operf_personnel))$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_run_avg_pers <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),PLAY_TYPE == "RUN", PERSONNEL == replaceNull(input$operf_personnel))$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_pers_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), PERSONNEL == replaceNull(input$operf_personnel)),"DEF_FORM"), title = "Average YDs Against Def Formations", stack = "stack")
  })
  
  output$operf_pers_coverage <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), PERSONNEL == replaceNull(input$operf_personnel)),"COVERAGE"), title = "Average YDs Against Coverages", stack = "stack")
  })
  
  output$operf_pers_blitzes <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), PERSONNEL == replaceNull(input$operf_personnel)),"BLITZ"), title = "Average YDs Against Blitzes", stack = "stack")
  })
  
  output$operf_pers_front <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), PERSONNEL == replaceNull(input$operf_personnel)),"FRONT"), title = "Average YDs Against Fronts", stack = "stack")
  })
  
  ##FORMATION
  output$operf_top_forms <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"OFF_FORM"), title = "Average YDs by Formation", stack = "stack", showLegend = T)
  })
  
  output$operf_form_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("operf_form", "SELECT A FORMATION", choices = unique(rpOnly(oSide())$OFF_FORM))
  })
  
  
  output$operf_pass_form <- renderValueBox({
    valueBox(
      sum(filter(oSide(), PLAY_TYPE == "PASS", OFF_FORM == replaceNull(input$operf_form))$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_run_form <- renderValueBox({
    valueBox(
      sum(filter(oSide(),PLAY_TYPE == "RUN", OFF_FORM == replaceNull(input$operf_form))$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_pass_avg_form <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),PLAY_TYPE == "PASS", OFF_FORM == replaceNull(input$operf_form))$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_run_avg_form <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),PLAY_TYPE == "RUN", OFF_FORM == replaceNull(input$operf_form))$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_form_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_FORM == replaceNull(input$operf_form)),"DEF_FORM"), title = "Average YDs Against Def Formations", stack = "stack")
  })
  
  output$operf_form_coverage <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_FORM == replaceNull(input$operf_form)),"COVERAGE"), title = "Average YDs Against Coverages", stack = "stack")
  })
  
  output$operf_form_blitzes <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_FORM == replaceNull(input$operf_form)),"BLITZ"), title = "Average YDs Against Blitzes", stack = "stack")
  })
  
  output$operf_form_front <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_FORM == replaceNull(input$operf_form)),"FRONT"), title = "Average YDs Against Fronts", stack = "stack")
  })
  
  ##OLINE
  output$operf_top_olines <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"OLINE"), title = "Average YDs by Blocking Scheme", stack = "stack", showLegend = T)
  })
  
  output$operf_oline_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("operf_oline", "SELECT A BLOCKING SCHEME", choices = unique(rpOnly(oSide())$OLINE), selected = unique(rpOnly(oSide())$OLINE)[1])
  })
  
  
  output$operf_oline_type <- renderValueBox({
    valueBox(
      filter(oSide(), OLINE == replaceNull(input$operf_oline))[1,"PLAY_TYPE"], "PLAY TYPE", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_oline_ran <- renderValueBox({
    valueBox(
      length(filter(oSide(), OLINE == replaceNull(input$operf_oline))$GN_LS), "TOTAL RAN", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_oline_yards <- renderValueBox({
    valueBox(
      sum(filter(oSide(),OLINE == replaceNull(input$operf_oline))$GN_LS), "TOTAL YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_oline_avg <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),OLINE == replaceNull(input$operf_oline))$GN_LS),2), "YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_oline_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OLINE == replaceNull(input$operf_oline)),"DEF_FORM"), title = "Average YDs Against Def Formations", showLegend = F)
  })
  
  output$operf_oline_coverage <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OLINE == replaceNull(input$operf_oline)),"COVERAGE"), title = "Average YDs Against Coverages", showLegend = F)
  })
  
  output$operf_oline_blitzes <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OLINE == replaceNull(input$operf_oline)),"BLITZ"), title = "Average YDs Against Blitzes", showLegend = F)
  })
  
  output$operf_oline_front <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OLINE == replaceNull(input$operf_oline)),"FRONT"), title = "Average YDs Against Fronts", showLegend = F)
  })
  
  ##PLAY
  output$operf_top_plays <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSide(),"OFF_PLAY"), title = "Average YDs by Play", stack = "stack", showLegend = T)
  })
  
  
  output$operf_play_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("operf_play", "SELECT A PLAY", choices = unique(rpOnly(oSide())$OFF_PLAY), selected = unique(rpOnly(oSide())$OFF_PLAY)[1])
  })
  
  
  output$operf_play_type <- renderValueBox({
    valueBox(
      filter(oSide(), OFF_PLAY == replaceNull(input$operf_play))[1,"PLAY_TYPE"], "PLAY TYPE", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_play_ran <- renderValueBox({
    valueBox(
      length(filter(oSide(), OFF_PLAY == replaceNull(input$operf_play))$GN_LS), "TOTAL RAN", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_play_yards <- renderValueBox({
    valueBox(
      sum(filter(oSide(),OFF_PLAY == replaceNull(input$operf_play))$GN_LS), "TOTAL YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$operf_play_avg <- renderValueBox({
    valueBox(
      round(mean(filter(oSide(),OFF_PLAY == replaceNull(input$operf_play))$GN_LS),2), "YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$operf_play_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_PLAY == replaceNull(input$operf_play)),"DEF_FORM"), title = "Average YDs Against Def Formations", showLegend = F)
  })
  
  output$operf_play_coverage <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_PLAY == replaceNull(input$operf_play)),"COVERAGE"), title = "Average YDs Against Coverages", showLegend = F)
  })
  
  output$operf_play_blitzes <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_PLAY == replaceNull(input$operf_play)),"BLITZ"), title = "Average YDs Against Blitzes", showLegend = F)
  })
  
  output$operf_play_front <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_PLAY == replaceNull(input$operf_play)),"FRONT"), title = "Average YDs Against Fronts", showLegend = F)
  })
  
  ##################################
  
  
  #################################################################
  ###DEFENSE
  ################################################################
  
  ########Defense Summary
  output$des_rp_sum <- renderPlotly({
    plot.donut(colSort(getTable(oSideD(),"PLAY_TYPE"),"Var1"), title = "Run Pass Breakdown", showLegend = T)
  })
  
  
  output$des_top_plays <- renderPlotly({
    plot.oneBar(getN(getTable(oSideD(),"OFF_PLAY"),"value"), title = "Top 5 Plays Ran")
  })
  
  output$des_top_forms <- renderPlotly({
    plot.oneBar(getN(getTable(oSideD(),"OFF_FORM"),"value"), title = "Top 5 Formations Ran")
  })
  
  output$des_top_pers <- renderPlotly({
    plot.donut(getTable(oSideD(),"PERSONNEL"), title = "Personnel Breakdown", showLegend = T)
  })
  
  output$des_total_yards<-renderValueBox({
    valueBox(
      sum(oSideD()$GN_LS), "TOTAL YARDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_first_downs<-renderValueBox({
    valueBox(
      countFactor(oSideD(),"DN",1)-length(unique(oSideD()$DRIVE)), "FIRST DOWNS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_total_plays <- renderValueBox({
    valueBox(
      length(oSideD()[,1]), "TOTAL PLAYS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_yards_play <- renderValueBox({
    valueBox(
      round(sum(oSideD()$GN_LS)/length(oSideD()[,1]),2), "YARDS PER PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_run_yards <- renderValueBox({
    valueBox(
      sum(filter(oSideD(),PLAY_TYPE=="RUN")$GN_LS), "RUN YARDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_pass_yards <- renderValueBox({
    valueBox(
      sum(filter(oSideD(),PLAY_TYPE=="PASS")$GN_LS), "PASS YARDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$des_run_yards_play <- renderValueBox({
    valueBox(
      round(sum(filter(oSideD(),PLAY_TYPE=="RUN")$GN_LS)/length(filter(oSideD(),PLAY_TYPE == "RUN")[,1]),2), "RUN YARDS PER PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_pass_yards_play <- renderValueBox({
    valueBox(
      round(sum(filter(oSideD(),PLAY_TYPE=="PASS")$GN_LS)/length(filter(oSideD(),PLAY_TYPE == "PASS")[,1]),2), "RUN YARDS PER PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_completion_pct <- renderValueBox({
    valueBox(
      paste0(round(length(filter(oSideD(),RESULT=="COMPLETE")$PLAY_TYPE)/length(filter(oSideD(),PLAY_TYPE == "PASS")[,1]),2)*100,"%"), "COMPLETION PCT", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_drives <- renderValueBox({
    valueBox(
      length(unique(filter(oSideD(),PLAY_TYPE=="PASS")$DRIVE)), "DRIVES", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_third_conv <- renderValueBox({
    valueBox(
      paste0(round(dnConv(oSideD(),3),2)*100,"%"), "3rd DN CONVERSION", icon = icon("list"),
      color = "black"
    )
  })
  
  output$des_fourth_conv <- renderValueBox({
    valueBox(
      paste0(round(dnConv(oSideD(),4),2)*100,"%"), "4th DN CONVERSION", icon = icon("list"),
      color = "black"
    )
  })
  
  ##Defense 
  output$des_plays <- renderPlotly({
    plot.donut(getTable(oSideD(),c("DEF_PLAY")), title = "Def Play Breakdown", showLegend = TRUE)
  })
  
  output$des_rp_play <- renderPlotly({
    plot.bars(rpYardsByFactor(oSideD(),"DEF_PLAY"), title = "Total YDs Against Def Play", stack = "stack")
  })
  
  
  output$des_def_form <- renderPlotly({
    plot.donut(getTable(oSideD(),c("DEF_FORM")), title = "Def Formation Breakdown", showLegend = TRUE)
  })
  
  output$des_rp_def_form <- renderPlotly({
    plot.bars(rpYardsByFactor(oSideD(),"DEF_FORM"), title = "YDs Against Def Formations", stack = "stack")
  })
  
  #################
  
  ############DOWN
  output$oo_off_form_dn <- renderPlotly({
    plot.bars(getTable(oSideD(),c("OFF_FORM","DN")),"stack", title = "Off Formations by Down")
  })
  
  output$oo_off_form_dist <- renderPlotly({
    plot.bars(getTable(addDistBucket(filter(oSideD(),DN==input$off_form_dn)),c("OFF_FORM","DIST_BUCKET")),"stack", title = "Off Formations by Down & Dist")
  })
  
  
  output$oo_pers_dn <- renderPlotly({
    plot.bars(getTable(oSideD(),c("PERSONNEL","DN")),"stack", title = "Personnel by Down")
  })
  
  #DN and Distance
  output$oo_pers_dist <- renderPlotly({
    plot.bars(getTable(addDistBucket(filter(oSideD(),DN==input$pers_dn)),c("PERSONNEL","DIST_BUCKET")),"stack", title = "Personnel by Down & Dist")
  })
  
  
  output$oo_play_dn <- renderPlotly({
    plot.bars(getTable(oSideD(),c("OFF_PLAY","DN")),"stack", title = "Off Play by Down")
  })
  
  #DN and Distance
  output$oo_play_dist <- renderPlotly({
    plot.bars(getTable(addDistBucket(filter(oSideD(),DN==input$play_dn)),c("OFF_PLAY","DIST_BUCKET")),"stack", title = "Off Play by Down & Dist")
  })
  
  output$oo_rp_dn <- renderPlotly({
    plot.bars(getTable(oSideD(),c("PLAY_TYPE","DN")),"stack", title = "Run Pass by Down")
  })
  
  output$oo_rp_dist <- renderPlotly({
    plot.bars(getTable(addDistBucket(filter(oSideD(),DN==input$oo_rp_dn_a)),c("PLAY_TYPE","DIST_BUCKET")),"stack", title = "Run Pass by Down & Dist")
  })
  
  ##
  
  output$oo_avg_yds_dn <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSideD(),"DN"), "stack", title = "Avg Yards Given Up by Down")
  })
  
  output$oo_avg_yds_dist <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(addDistBucket(filter(oSideD(),DN==input$oo_rp_yds_dn_a)),"DIST_BUCKET"), "stack", title = "Average Yards Given Up by Down & Dist")
  })
  
  
  ##########################
  
  #####FORMATION
  output$do_form_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("do_formation", "SELECT A FORMATION", choices = unique(rpOnly(oSideD())$OFF_FORM))
  })
  
  output$dd_form_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("dd_formation", "SELECT A FORMATION", choices = unique(rpOnly(oSideD())$DEF_FORM))
  })
  
  
  output$dfo_formation_count <- renderValueBox({
    valueBox(
      length(offForm(oSideD(),input$do_formation)$DRIVE), "PLAYS RUN", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dfo_formation_yards <- renderValueBox({
    valueBox(
      sum(offForm(oSideD(),input$do_formation)$GN_LS), "TOTAL YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dfo_formation_pyards <- renderValueBox({
    valueBox(
      sum(filter(offForm(oSideD(),input$do_formation), PLAY_TYPE == "PASS")$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dfo_formation_ryards <- renderValueBox({
    valueBox(
      sum(filter(offForm(oSideD(),input$do_formation), PLAY_TYPE == "RUN")$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dfd_formations_count <- renderValueBox({
    valueBox(
      length(defForm(oSideD(),input$dd_formation)$DRIVE), "PLAYS RAN", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dfd_formation_yards <- renderValueBox({
    valueBox(
      sum(defForm(oSideD(),input$dd_formation)$GN_LS), "YDS AGNST", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dfd_formation_pyards <- renderValueBox({
    valueBox(
      sum(filter(defForm(oSideD(),input$dd_formation), PLAY_TYPE == "PASS")$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dfd_formation_ryards <- renderValueBox({
    valueBox(
      sum(filter(defForm(oSideD(),input$dd_formation), PLAY_TYPE == "RUN")$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dfo_rp <- renderPlotly({
    plot.donut(getTable(offForm(oSideD(),input$do_formation),c("PLAY_TYPE")), title = "Run Pass Breakdown", showLegend = T)
  })
  
  output$dfo_plays <- renderPlotly({
    plot.oneBar(getN(getTable(offForm(oSideD(),input$do_formation),c("OFF_PLAY")),"value"), title = "Top 5 Plays Ran")
  })
  
  ##
  
  output$dfd_formations <- renderPlotly({
    plot.oneBar(getN(getTable(defForm(oSideD(),input$dd_formation),c("OFF_FORM")),"value"), title = "Top 5 Formations Ran")
  })
  
  output$dfd_plays<- renderPlotly({
    plot.oneBar(getN(getTable(defForm(oSideD(),input$dd_formation),c("OFF_PLAY")), "value"), title = "Top 5 Plays Ran")
  })
  
  output$dfd_rp <- renderPlotly({
    plot.donut(getTable(defForm(oSideD(),input$dd_formation),c("PLAY_TYPE")), title = "Run Pass Breakdown", showLegend = T)
  })
  
  output$dfd_pers <- renderPlotly({
    plot.donut(getTable(defForm(oSideD(),input$dd_formation),c("PERSONNEL")), title = "Personnel Breakdown", showLegend = T)
  })
  
  #####################
  
  #####################
  ###PERFORMANCE
  
  ###PERFORMANCE
  ##Summary
  output$dperf_pass_s <- renderValueBox({
    valueBox(
      sum(filter(oSideD(), PLAY_TYPE == "PASS")$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_s <- renderValueBox({
    valueBox(
      sum(filter(oSideD(),PLAY_TYPE == "RUN")$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_pass_avg_s <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "PASS")$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_avg_s <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "RUN")$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  #RUN PASS
  output$dperf_pass <- renderValueBox({
    valueBox(
      sum(filter(oSideD(), PLAY_TYPE == "PASS")$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run <- renderValueBox({
    valueBox(
      sum(filter(oSideD(),PLAY_TYPE == "RUN")$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_pass_avg <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "PASS")$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_avg <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "RUN")$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_rp_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSideD(),"DEF_FORM"), title = "Average YDs Against Def Formations", stack = "stack")
  })
  
  output$dperf_rp_play <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSideD(),"DEF_PLAY"), title = "Average YDs Against Def Play", stack = "stack")
  })
  
  
  ##Personnel
  output$dperf_top_pers <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSideD(),"PERSONNEL"), title = "Average YDs by Personnel", stack = "stack", showLegend = T)
  })
  
  output$dperf_pers_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("dperf_personnel", "SELECT A PERSONNEL", choices = unique(rpOnly(oSideD())$PERSONNEL))
  })
  
  output$dperf_pass_pers <- renderValueBox({
    valueBox(
      sum(filter(oSideD(), PLAY_TYPE == "PASS", PERSONNEL == replaceNull(input$dperf_personnel))$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_pers <- renderValueBox({
    valueBox(
      sum(filter(oSideD(),PLAY_TYPE == "RUN", PERSONNEL == replaceNull(input$dperf_personnel))$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_pass_avg_pers <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "PASS", PERSONNEL == replaceNull(input$dperf_personnel))$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_avg_pers <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "RUN", PERSONNEL == replaceNull(input$dperf_personnel))$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_pers_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), PERSONNEL == replaceNull(input$dperf_personnel)),"DEF_FORM"), title = "Average YDs Against Def Formations", stack = "stack")
  })
  
  output$dperf_pers_play <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), PERSONNEL == replaceNull(input$dperf_personnel)),"DEF_PLAY"), title = "Average YDs Against Def Play", stack = "stack")
  })
  
  ##FORMATION
  output$dperf_top_forms <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSideD(),"OFF_FORM"), title = "Average YDs by Formation", stack = "stack", showLegend = T)
  })
  
  output$dperf_form_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("dperf_form", "SELECT A FORMATION", choices = unique(rpOnly(oSideD())$OFF_FORM))
  })
  
  
  output$dperf_pass_form <- renderValueBox({
    valueBox(
      sum(filter(oSideD(), PLAY_TYPE == "PASS", OFF_FORM == replaceNull(input$dperf_form))$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_form <- renderValueBox({
    valueBox(
      sum(filter(oSideD(),PLAY_TYPE == "RUN", OFF_FORM == replaceNull(input$dperf_form))$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_pass_avg_form <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "PASS", OFF_FORM == replaceNull(input$dperf_form))$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_avg_form <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "RUN", OFF_FORM == replaceNull(input$dperf_form))$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_form_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), OFF_FORM == replaceNull(input$dperf_form)),"DEF_FORM"), title = "Average YDs Against Def Formations", stack = "stack")
  })
  
  output$dperf_form_play <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), OFF_FORM == replaceNull(input$dperf_form)),"DEF_PLAY"), title = "Average YDs Against Def Play", stack = "stack")
  })
  
  ##DPLAY
  output$dperf_top_dplays <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSideD(),"DEF_PLAY"), title = "Average YDs Agaisnt Def Play", stack = "stack", showLegend = T)
  })
  
  
  output$dperf_dplay_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("dperf_dplay", "SELECT A PLAY", choices = unique(rpOnly(oSideD())$DEF_PLAY), selected = unique(rpOnly(oSideD())$DEF_PLAY)[1])
  })
  
  
  output$dperf_pass_dplay <- renderValueBox({
    valueBox(
      sum(filter(oSideD(), PLAY_TYPE == "PASS", DEF_PLAY == replaceNull(input$dperf_dplay))$GN_LS), "PASS YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_dplay <- renderValueBox({
    valueBox(
      sum(filter(oSideD(),PLAY_TYPE == "RUN", DEF_PLAY == replaceNull(input$dperf_dplay))$GN_LS), "RUN YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_pass_avg_dplay <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "PASS", DEF_PLAY == replaceNull(input$dperf_dplay))$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_run_avg_dplay <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),PLAY_TYPE == "RUN", DEF_PLAY == replaceNull(input$dperf_dplay))$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_dplay_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), DEF_PLAY == replaceNull(input$dperf_dplay)),"OFF_FORM"), title = "Average YDs Given Up by Formation", stack = "stack", showLegend = T)
  })
  
  output$dperf_dplay_pers <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), DEF_PLAY == replaceNull(input$dperf_dplay)),"PERSONNEL"), title = "Average YDs Given Up by Personnel", stack = "stack", showLegend = T)
  })
  
  output$dperf_dplay_play <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), DEF_PLAY == replaceNull(input$dperf_dplay)),"OFF_PLAY"), title = "Average YDs Given Up by Off Play", stack = "stack", showLegend = T)
  })
  
  
  
  
  #OPLAY
  output$dperf_top_plays <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(oSideD(),"OFF_PLAY"), title = "Average YDs by Play", stack = "stack", showLegend = T)
  })
  
  
  output$dperf_play_list <- renderUI({
    input$submit
    input$delete
    input$pd_submit
    selectInput("dperf_play", "SELECT A PLAY", choices = unique(rpOnly(oSideD())$OFF_PLAY), selected = unique(rpOnly(oSideD())$OFF_PLAY)[1])
  })
  
  
  output$dperf_play_type <- renderValueBox({
    valueBox(
      filter(oSideD(), OFF_PLAY == replaceNull(input$dperf_play))[1,"PLAY_TYPE"], "PLAY TYPE", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_play_ran <- renderValueBox({
    valueBox(
      length(filter(oSideD(), OFF_PLAY == replaceNull(input$dperf_play))$GN_LS), "TOTAL RAN", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_play_yards <- renderValueBox({
    valueBox(
      sum(filter(oSideD(),OFF_PLAY == replaceNull(input$dperf_play))$GN_LS), "TOTAL YDS", icon = icon("list"),
      color = "black"
    )
  })
  
  output$dperf_play_avg <- renderValueBox({
    valueBox(
      round(mean(filter(oSideD(),OFF_PLAY == replaceNull(input$dperf_play))$GN_LS),2), "YDS/PLAY", icon = icon("list"),
      color = "black"
    )
  })
  
  
  output$dperf_play_form <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), OFF_PLAY == replaceNull(input$dperf_play)),"DEF_FORM"), title = "Average YDs Against Def Formations", showLegend = F)
  })
  
  output$dperf_play_play <- renderPlotly({
    plot.bars(rpYardsAvgByFactor(filter(oSideD(), OFF_PLAY == replaceNull(input$dperf_play)),"DEF_PLAY"), title = "Average YDs Against Def Play", showLegend = F)
  })
  
  ###############
  
  ##############
  ##FIELD MAP
  ##oside
  output$oside_b <- renderPlotly({
    plot.bars(getTable(filter(addFieldBucket(oSide()),FIELD_BUCKET=="BACKED_UP"),c("PLAY_TYPE","HASH")), title = "Backed Up", stack = "stack", showLegend = T)
  })
  
  output$oside_m <- renderPlotly({
    plot.bars(getTable(filter(addFieldBucket(oSide()),FIELD_BUCKET=="MIDDLE"),c("PLAY_TYPE","HASH")), title = "Middle of the Field", stack = "stack", showLegend = T)
  })
  
  output$oside_r <- renderPlotly({
    plot.bars(getTable(filter(addFieldBucket(oSide()),FIELD_BUCKET=="REDZONE"),c("PLAY_TYPE","HASH")), title = "Redzone", stack = "stack", showLegend = T)
  })
  
  #opp
  output$opp_b <- renderPlotly({
    plot.bars(getTable(filter(addFieldBucket(oSideD()),FIELD_BUCKET=="BACKED_UP"),c("PLAY_TYPE","HASH")), title = "Backed Up", stack = "stack", showLegend = T)
  })
  
  output$opp_m <- renderPlotly({
    plot.bars(getTable(filter(addFieldBucket(oSideD()),FIELD_BUCKET=="MIDDLE"),c("PLAY_TYPE","HASH")), title = "Middle of the Field", stack = "stack", showLegend = T)
  })
  
  output$opp_r <- renderPlotly({
    plot.bars(getTable(filter(addFieldBucket(oSideD()),FIELD_BUCKET=="REDZONE"),c("PLAY_TYPE","HASH")), title = "Redzone", stack = "stack", showLegend = T)
  })
  

  
})   

