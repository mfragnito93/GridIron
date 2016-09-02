
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
      plot.donut(getTable(drive_summary(),"PLAY_TYPE"))
    })
    
    output$drive_plot<-renderPlotly({
      plot.drive(theDrive(),input$drive)
    })
    
    
    
})   

