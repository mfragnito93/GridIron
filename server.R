
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinysky)
library(DT)
library(shinydashboard)
library(plotly)

shinyServer(function(input, output, session) {
  
    # table <- observe({
    #   print(hot.to.df(input$hot))
    #   hot.to.df(input$hot)
    # })
    # 
  
    ###PLAY ENTRY
    # UpdateTable <- function(data){
    #   output$hot <- renderRHandsontable({
    #     if(input$ODK == "O") rhandsontable(data[,names(GetMetadata(tableMeta)$fields)], stretchH = "all", height = 65) else rhandsontable(data[,names(GetMetadata(d_tableMeta)$fields)], stretchH = "all", height = 65)
    #   })
    # }
    # 
    # UpdateForm <- function()({
    #   entry <- table()
    #   print(length(entry))
    #   colnames(entry) <- if(input$ODK =="O") names(GetMetadata(tableMeta)$fields) else names(GetMetadata(d_tableMeta)$fields)
    #   entry
    # })
    # 
    # 
    responses <- CreateDefaultRecord()
    
    # input fields are treated as a group -- the row
    formData <- reactive({
      entry<-sapply(names(GetMetadata(meta)$fields), function(x) input[[x]])
      default <- CreateDefaultRecord()[,!colnames(CreateDefaultRecord()) %in% c(entry)]
      c(entry,default)
    })
    
    # Click "Submit" button -> save data
    observeEvent(input$submit, {
      if (input$id != "0") {
        UpdateData(formData())
      } else {
        CreateData(formData())
        UpdateScoreboard(ScoreBoardCalc(), session)
        UpdateTable(CreateDefaultRecord(),session)
      }
      
    }, priority = 1)
    
    # Press "New" button -> display empty record
    observeEvent(input$new, {
      UpdateScoreboard(ScoreBoardCalc(), session)
      UpdateTable(CreateDefaultRecord(), session)
    })
    
    # Press "Delete" button -> delete from data
    observeEvent(input$delete, {
      DeleteData(formData())
      UpdateScoreboard(ScoreBoardCalc(), session)
      UpdateTable(CreateDefaultRecord(), session)
    }, priority = 1)
    
    # Select row in table -> show details in inputs
    observeEvent(input$responses_rows_selected, {
      if (length(input$responses_rows_selected) > 0) {
        data <- ReadData()[input$responses_rows_selected, ]
        print(data)
        UpdateScoreboard(data,session)
        UpdateTable(data,session)
      }
      
    })
    
    # display table
    output$responses <- DT::renderDataTable({
      #update after submit is clicked
      input$submit
      #update after delete is clicked
      input$delete
      ReadData()
    }, server = FALSE, selection = "single",
    colnames = unname(GetMetadata(meta)$fields),options=list(order = list(0, 'desc'), scrollX = TRUE, autoWidth =TRUE),rownames = FALSE
    ) 
    

    #initialize handsome table
    UpdateTable(CreateDefaultRecord(), session)
    UpdateScoreboard(ScoreBoardCalc(), session)
    
    ###DRIVE SUMMARY
    drive_summary <- reactive({
      driveSummary(ReadData(),input$drive)
    })
    
    theDrive <- reactive({
      drive(ReadData(),input$drive)
    })
    
    
    output$drive_sum <- DT::renderDataTable({
      drive_summary()
    }, server = FALSE, selection = "single",
    colnames = unname(GetMetadata(driveSummaryMeta)$fields),options=list(order = list(0, 'asc'), scrollX = FALSE, autoWidth = TRUE),rownames = FALSE
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
    oSide <- function(){
      rpOnly(filter(ReadData(),ODK=="O"))
    }
    
    #Opponent
    oSideD <- function(){
      rpOnly(filter(ReadData(),ODK == "D"))
    }
    
    output$oside <- reactive({
      rpOnly(oSide())
    })
    
    output$opp <- reactive({
      print(rpOnly(oSideD()))
    })
    
    ##Offense Summary
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
      plot.oneBar(getTable(oSide(),c("DEF_FORM")), title = "Formations")
    })
    
    output$os_coverage <- renderPlotly({
      plot.oneBar(getTable(oSide(),c("COVERAGE")), title = "Coverages")
    })
    
    output$os_front <- renderPlotly({
      plot.oneBar(getTable(oSide(),c("FRONT")), title = "Fronts")
    })
    
    output$os_blitz <- renderPlotly({
      plot.oneBar(getTable(oSide(),c("BLITZ")), title = "Blitzes")
    })
    
    # 
    # output$os_def_form <- renderPlotly({
    #   plot.donut(getTable(oSide,c("DEF_FORM")))
    # })
    
    ##Down
    # output$od_rp_1 <- renderPlotly({
    #   plot.donut(colSort(getTable(filter(oSide(),DN=="1"),"PLAY_TYPE"),"Var1"))
    # })
    
    output$od_def_form_dn <- renderPlotly({
      plot.bars(getTable(oSide(),c("DEF_FORM","DN")),"stack")
    })
    
    #DN and Distance
    output$od_def_form_dist <- renderPlotly({
      plot.bars(getTable(addDistBucket(filter(oSide(),DN==input$def_form_dn)),c("DEF_FORM","DIST_BUCKET")),"stack")
    })
    
    
    output$od_coverage_dn <- renderPlotly({
      plot.bars(getTable(oSide(),c("COVERAGE","DN")),"stack")
    })
    
    #DN and Distance
    output$od_coverage_dist <- renderPlotly({
      plot.bars(getTable(addDistBucket(filter(oSide(),DN==input$coverage_dn)),c("COVERAGE","DIST_BUCKET")),"stack")
    })
    
    
    output$od_front_dn <- renderPlotly({
      plot.bars(getTable(oSide(),c("FRONT","DN")),"stack")
    })
    
    #DN and Distance
    output$od_front_dist <- renderPlotly({
      plot.bars(getTable(addDistBucket(filter(oSide(),DN==input$front_dn)),c("FRONT","DIST_BUCKET")),"stack")
    })
    
    output$od_blitz_dn <- renderPlotly({
      plot.bars(getTable(oSide(),c("BLITZ","DN")),"stack")
    })
    
    output$od_blitz_dist <- renderPlotly({
      plot.bars(getTable(addDistBucket(filter(oSide(),DN==input$blitz_dn)),c("BLITZ","DIST_BUCKET")),"stack")
    })
    
    
    output$od_avg_yds_dn <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(oSide(),"DN"), "stack")
    })
    
    output$od_avg_yds_dist <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(addDistBucket(filter(oSide(),DN==input$rp_dn)),"DIST_BUCKET"), "stack")
    })
    
    
    ##FORMATIONS
    output$ofd_formation_count <- renderValueBox({
      valueBox(
        length(filter(oSide(),DEF_FORM == input$od_formation)$DRIVE), "PLAYS RUN", icon = icon("list"),
        color = "black"
      )
    })
    
    output$ofd_formation_yards <- renderValueBox({
      valueBox(
        sum(filter(oSide(),DEF_FORM == input$od_formation)$GN_LS), "YDS AGNST", icon = icon("list"),
        color = "black"
      )
    })
    
    output$ofd_formation_pyards <- renderValueBox({
      valueBox(
        sum(filter(oSide(),DEF_FORM == input$od_formation, PLAY_TYPE == "PASS")$GN_LS), "PASS YDS AGNST", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$ofd_formation_ryards <- renderValueBox({
      valueBox(
        sum(filter(oSide(),DEF_FORM == input$od_formation, PLAY_TYPE == "RUN")$GN_LS), "RUN YDS AGNST", icon = icon("list"),
        color = "black"
      )
    })
    
    output$ofo_formations_count <- renderValueBox({
      valueBox(
        length(filter(oSide(),OFF_FORM == input$oo_formation)$DRIVE), "PLAYS RAN", icon = icon("list"),
        color = "black"
      )
    })
    
    output$ofo_formation_yards <- renderValueBox({
      valueBox(
        sum(filter(oSide(),OFF_FORM == input$oo_formation)$GN_LS), "TOTAL YDS", icon = icon("list"),
        color = "black"
      )
    })
    
    output$ofo_formation_pyards <- renderValueBox({
      valueBox(
        sum(filter(oSide(),OFF_FORM == input$oo_formation, PLAY_TYPE == "PASS")$GN_LS), "PASS YDS", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$ofo_formation_ryards <- renderValueBox({
      valueBox(
        sum(filter(oSide(),OFF_FORM == input$oo_formation, PLAY_TYPE == "RUN")$GN_LS), "RUN YDS", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$ofd_coverages <- renderPlotly({
      plot.donut(getTable(filter(oSide(),DEF_FORM == input$od_formation),c("COVERAGE")), title = "Coverages", showLegend = T)
    })
    
    output$ofd_blitzes <- renderPlotly({
      plot.donut(getTable(filter(oSide(),DEF_FORM == input$od_formation),c("BLITZ")), title = "Blitzes", showLegend = T)
    })
    
    output$ofd_fronts <- renderPlotly({
      plot.donut(getTable(filter(oSide(),DEF_FORM == input$od_formation),c("FRONT")), title = "Fronts", showLegend = T)
    })
    
    output$ofo_formations <- renderPlotly({
      plot.donut(getTable(filter(oSide(),OFF_FORM == input$oo_formation),c("DEF_FORM")), title = "Defensive Formations", showLegend = T)
    })
    
    output$ofo_coverages<- renderPlotly({
      plot.donut(getTable(filter(oSide(),OFF_FORM == input$oo_formation),c("COVERAGE")), title = "Coverages", showLegend = T)
    })
    
    output$ofo_blitzes <- renderPlotly({
      plot.donut(getTable(filter(oSide(),OFF_FORM == input$oo_formation),c("BLITZ")), title = "Blitzes", showLegend = T)
    })
    
    output$ofo_fronts <- renderPlotly({
      plot.donut(getTable(filter(oSide(),OFF_FORM == input$oo_formation),c("FRONT")), title = "Fronts", showLegend = T)
    })
    
    
    ###PERFORMANCE
    ##RUN PASS
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
    output$operf_pass_pers <- renderValueBox({
      valueBox(
        sum(filter(oSide(), PLAY_TYPE == "PASS", PERSONNEL == input$operf_personnel)$GN_LS), "PASS YDS", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$operf_run_pers <- renderValueBox({
      valueBox(
        sum(filter(oSide(),PLAY_TYPE == "RUN", PERSONNEL == input$operf_personnel)$GN_LS), "RUN YDS", icon = icon("list"),
        color = "black"
      )
    })
    
    output$operf_pass_avg_pers <- renderValueBox({
      valueBox(
        round(mean(filter(oSide(),PLAY_TYPE == "PASS", PERSONNEL == input$operf_personnel)$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$operf_run_avg_pers <- renderValueBox({
      valueBox(
        round(mean(filter(oSide(),PLAY_TYPE == "RUN", PERSONNEL == input$operf_personnel)$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
        color = "black"
      )
    })
    
    output$operf_pers_form <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), PERSONNEL == input$operf_personnel),"DEF_FORM"), title = "Average YDs Against Def Formations", stack = "stack")
    })
    
    output$operf_pers_coverage <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), PERSONNEL == input$operf_personnel),"COVERAGE"), title = "Average YDs Against Coverages", stack = "stack")
    })
    
    output$operf_pers_blitzes <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), PERSONNEL == input$operf_personnel),"BLITZ"), title = "Average YDs Against Blitzes", stack = "stack")
    })
    
    output$operf_pers_front <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), PERSONNEL == input$operf_personnel),"FRONT"), title = "Average YDs Against Fronts", stack = "stack")
    })
    
    ##FORMATION
    output$operf_pass_form <- renderValueBox({
      valueBox(
        sum(filter(oSide(), PLAY_TYPE == "PASS", OFF_FORM == input$operf_form)$GN_LS), "PASS YDS", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$operf_run_form <- renderValueBox({
      valueBox(
        sum(filter(oSide(),PLAY_TYPE == "RUN", OFF_FORM == input$operf_form)$GN_LS), "RUN YDS", icon = icon("list"),
        color = "black"
      )
    })
    
    output$operf_pass_avg_form <- renderValueBox({
      valueBox(
        round(mean(filter(oSide(),PLAY_TYPE == "PASS", OFF_FORM == input$operf_form)$GN_LS),2), "PASS YDS/PLAY", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$operf_run_avg_form <- renderValueBox({
      valueBox(
        round(mean(filter(oSide(),PLAY_TYPE == "RUN", OFF_FORM == input$operf_form)$GN_LS),2), "RUN YDS/PLAY", icon = icon("list"),
        color = "black"
      )
    })
    
    output$operf_form_form <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_FORM == input$operf_form),"DEF_FORM"), title = "Average YDs Against Def Formations", stack = "stack")
    })
    
    output$operf_form_coverage <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_FORM == input$operf_form),"COVERAGE"), title = "Average YDs Against Coverages", stack = "stack")
    })
    
    output$operf_form_blitzes <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_FORM == input$operf_form),"BLITZ"), title = "Average YDs Against Blitzes", stack = "stack")
    })
    
    output$operf_form_front <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_FORM == input$operf_form),"FRONT"), title = "Average YDs Against Fronts", stack = "stack")
    })
    
    ##PLAY
    output$operf_play_type <- renderValueBox({
      valueBox(
        filter(oSide(), OFF_PLAY == input$operf_play)[1,"PLAY_TYPE"], "PLAY TYPE", icon = icon("list"),
        color = "black"
      )
    })
    
    output$operf_play_ran <- renderValueBox({
      valueBox(
        length(filter(oSide(), OFF_PLAY == input$operf_play)$GN_LS), "TOTAL RAN", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$operf_play_yards <- renderValueBox({
      valueBox(
        sum(filter(oSide(),OFF_PLAY == input$operf_play)$GN_LS), "TOTAL YDS", icon = icon("list"),
        color = "black"
      )
    })
    
    output$operf_play_avg <- renderValueBox({
      valueBox(
        round(mean(filter(oSide(),OFF_PLAY == input$operf_play)$GN_LS),2), "YDS/PLAY", icon = icon("list"),
        color = "black"
      )
    })
    
    
    output$operf_play_form <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_PLAY == input$operf_play),"DEF_FORM"), title = "Average YDs Against Def Formations", showLegend = F)
    })
    
    output$operf_play_coverage <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_PLAY == input$operf_play),"COVERAGE"), title = "Average YDs Against Coverages", showLegend = F)
    })
    
    output$operf_play_blitzes <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_PLAY == input$operf_play),"BLITZ"), title = "Average YDs Against Blitzes", showLegend = F)
    })
    
    output$operf_play_front <- renderPlotly({
      plot.bars(rpYardsAvgByFactor(filter(oSide(), OFF_PLAY == input$operf_play),"FRONT"), title = "Average YDs Against Fronts", showLegend = F)
    })
    
    
    ###DEFENSE VIEWS
    
})   

