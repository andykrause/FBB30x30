#*****************************************************************************************
#                                                                                        *
#   Platform AVM Dashboard                                                               *
#                                                                                        *
# #*****************************************************************************************
# 
#   library(FBB30x30)
#   library(DT)
#   
#   data(players_df, package = 'FBB30x30')
#   ap <- players_df[1:120,]
#   
#  # ## Set Libraries
#  # 
#  #  pacman::p_load(c('shiny', 'ggmap', 'raster', 'rgdal'),
#  #                 character.only = TRUE)
#  #  
#  # ## Set Directory and Source Files
#  # 
#  #  code.dir <- "U:/andy/code/platformavm"
#  #  source(file.path(code.dir, 'initiatePAVM.R'))
#  # 
#  # ## Inital setup and modify defaults  
#  #  
#  #  initSetup(code.dir, show=TRUE)
#  #  initChangeOptions(list(name='control', code.dir=code.dir))
#  #  initChangeOptions(list(name='basic', run.type='retro.test'))
#  # 
#  #  # Custom Raster Map Extractor: courtesy R Lovelace
#  #  ggmap_rast <- function(map){
#  #    map_bbox <- attr(map, 'bb') 
#  #    .extent <- extent(as.numeric(map_bbox[c(2, 4, 1, 3)]))
#  #    my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
#  #    rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red', 'green', 'blue'))
#  #    red <- my_map
#  #    values(red) <- rgb_cols[['red']]
#  #    green <- my_map
#  #    values(green) <- rgb_cols[['green']]
#  #    blue <- my_map
#  #    values(blue) <- rgb_cols[['blue']]
#  #    stack(red, green, blue)
#  #  }
#   
# #*****************************************************************************************
# # Shiny App
# #*****************************************************************************************
#   
# ## User Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
# ui <- fluidPage(theme = 'bootstrap.css',
#   
#   # In order to get the 'live' AVM updates
#   shinyjs::useShinyjs(),
#   
#    # Application title
#    titlePanel("30x30 Draft Board"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          actionButton('setupdraft', "   Setup Draft", 
#                       icon("gears"), 
#                       style=paste0("color: #fff; background-color: #1FA050;",
#                                    "border-color: #1FA050")),
#          
#          hr(),
#          textInput('player.drafted',
#                    'Player to Draft',
#                     value='xxx'),
#          hr(),         
#          actionButton('makepick', "   Make Pick", 
#                       icon("gears"), 
#                       style=paste0("color: #ffff; background-color: #1FA050;",
#                                    "border-color: #1FA050"))
#        ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#         tabsetPanel(
#           tabPanel('Available Players', tableOutput('avail')),
#           tabPanel('By Position', DTOutput('pos')),
#           tabPanel('By Team', DTOutput('team'))
#       )
#      )
#    )
# )
# 
# ## Shiny Server ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# server <- function(input, output, session) {
# 
#   aa <- reactive({data.frame(id = input$player.drafted)})
#   dd <- reactive({ap})
#   
#   values <- reactiveValues()
#   values$df <- data.frame(Player_id = 'test')
#   
#   newEntry <- observeEvent(input$makepick, {
#       newLine <- data.frame(Player_id = input$player.drafted)
#       values$df <- rbind(values$df, newLine)
#     }
#   )
#   
#   output$pos <- renderTable(values$df)
#   
#   output$avail <- renderTable({
#     
#     dd()
#     
#   })
#   
#   # output$pos <- renderTable({
#   #   
#   #   aa()
#   #   
#   # })
#   
#   output$team <- renderDT({
#     
#     DT::datatable(dd(),
#                   options = list(lengthMenu = c(25, 50, 100), pageLength = 25))
# 
# 
#   })
#   
#   ## Change the Run Type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   makePick <- eventReactive(input$makepick,{
#     
#     ap <- ap[!ap$player_id %in% input$player.drafted, ]
#     assign('ap', ap, envir = .GlobalEnv)
#     assign('ip', input$player.drafted, envir=.GlobalEnv)
#     
#   })
# 
# } # Ends Server
# 
# # datadata <- data
# # makeReactiveBinding("datadata")
# # 
# # newData <- reactive({
# #   
# #   input$Button
# #   isolate({
# #     
# #     datadata <- data
# #     
# #     datadata <- subset(datadata, rating %in% input$checkGroups)
# #     
# #     
# #   })
# 
# # values <- reactiveValues()
# # values$df <- data.frame(Column1 = NA, Column2 = NA)
# # newEntry <- observe({
# #   if(input$update > 0) {
# #     newLine <- isolate(c(input$text1, input$text2))
# #     isolate(values$df <- rbind(values$df, newLine))
# #   }
# # })
# 
# 
# 
# ## Execute the Application ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# shinyApp(ui = ui, server = server)
# 
# #*****************************************************************************************
# #*****************************************************************************************