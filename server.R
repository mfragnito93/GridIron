
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
  
    ###PLAY ENTRY
    UpdateTable <- function(data){
      output$hot <- renderRHandsontable({
        rhandsontable(data[,names(GetMetadata(tableMeta)$fields)], stretchH = "all", height = 65)
      })
    }
    
    UpdateForm <- function()({
      entry <- hot.to.df(input$hot)
      colnames(entry) <- names(GetMetadata(tableMeta)$fields)
      entry
    })
    
  
    responses <- CreateDefaultRecord()
    
    # input fields are treated as a group -- the row
    formData <- reactive({
      table<-MakeEntry(UpdateForm())
      scoreboard<-sapply(names(GetMetadata(scoreboardMeta)$fields), function(x) input[[x]])
      c(scoreboard,table)
    })
    
    # Click "Submit" button -> save data
    observeEvent(input$submit, {
      if (input$id != "0") {
        UpdateData(formData())
      } else {
        CreateData(formData())
        UpdateScoreboard(ScoreBoardCalc(), session)
        UpdateTable(CreateDefaultRecord())
      }
    }, priority = 1)
    
    # Press "New" button -> display empty record
    observeEvent(input$new, {
      UpdateScoreboard(ScoreBoardCalc(), session)
      UpdateTable(CreateDefaultRecord())
    })
    
    # Press "Delete" button -> delete from data
    observeEvent(input$delete, {
      DeleteData(formData())
      UpdateScoreboard(ScoreBoardCalc(), session)
      UpdateTable(CreateDefaultRecord())
    }, priority = 1)
    
    # Select row in table -> show details in inputs
    observeEvent(input$responses_rows_selected, {
      if (length(input$responses_rows_selected) > 0) {
        data <- ReadData()[input$responses_rows_selected, ]
        UpdateScoreboard(data,session)
        UpdateTable(data)
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
    colnames = unname(GetMetadata(meta)$fields),options=list(order = list(0, 'desc'), scrollX = TRUE),rownames = FALSE
    ) 
    

    #initialize handsome table
    UpdateTable(CreateDefaultRecord())
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
      plot.donut(colSort(getTable(drive_summary(),"PLAY_TYPE"),"Var1"))
    })
    
    output$drive_plot<-renderPlotly({
      plot.drive(theDrive(),input$drive)
    })
    
    #Oceanside
    oSide <- function(){
      filter(ReadData(),ODK=="O")
    }
    
    #Opponent
    oSideD <- function(){
      filter(ReadData(),ODK == "D")
    }
    
    ##Offense Summary
    output$os_rp <- renderPlotly({
      plot.donut(colSort(getTable(oSide(),"PLAY_TYPE"),"Var1"))
    })

    
    output$os_top_plays <- renderPlotly({
      plot.oneBar(getN(getTable(oSide(),"OFF_PLAY"),"value"))
    })
    
    output$os_top_forms <- renderPlotly({
      plot.oneBar(getN(getTable(oSide(),"OFF_FORM"),"value"))
    })
    
    output$os_top_pers <- renderPlotly({
      plot.donut(getTable(oSide(),"PERSONNEL"))
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
      plot.oneBar(getTable(oSide(),c("DEF_FORM")))
    })
    
    output$os_coverage <- renderPlotly({
      plot.oneBar(getTable(oSide(),c("COVERAGE")))
    })
    
    output$os_front <- renderPlotly({
      plot.oneBar(getTable(oSide(),c("FRONT")))
    })
    
    output$os_blitz <- renderPlotly({
      plot.oneBar(getTable(oSide(),c("BLITZ")))
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
    
    output$od_front_dn <- renderPlotly({
      plot.bars(getTable(oSide(),c("FRONT","DN")),"stack")
    })
    
    #DN and Distance
    
    output$od_blitz_dn <- renderPlotly({
      plot.bars(getTable(oSide(),c("BLITZ","DN")),"stack")
    })
    
    
    
})   

