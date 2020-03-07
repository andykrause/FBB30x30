# #*****************************************************************************************
# #                                                                                        *
# #   Platform AVM Dashboard                                                               *
# #                                                                                        *
# #*****************************************************************************************
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
#          hr()#,
#          
#       #    # Enter FIPS
#       #    numericInput("fips",
#       #                 "County to Value (FIPS CODE)",
#       #                 11001,
#       #                 min=1001, max=57000),
#       #    hr(),
#       #    
#       #    # Enter Type of Run to Do
#       #    selectInput('runtype',
#       #                'Select Type of Run',
#       #                c('Retro Test' = 'retro.test',
#       #                  'Retro Test + Bulk Run' = 'retro.bulk',
#       #                  'Bulk Run Only' = 'bulk.value',
#       #                  'Solo Run (coming)' = 'solo')),
#       #    
#       #    # If Bulk Value
#       #    conditionalPanel(
#       #      condition = "input.runtype == 'bulk.value'",
#       #      textInput('value.date',
#       #                'Date of value (YYYY-MM-DD',
#       #                value='2017-08-01') 
#       #    ),
#       #    
#       #    # If Solo
#       #    conditionalPanel(
#       #      condition = "input.runtype == 'solo'",
#       #      textInput('value.date',
#       #                'Date of value (YYYY-MM-DD',
#       #                value='2017-08-01') 
#       #    ),
#       #    
#       #    # If Retro.Test
#       #    conditionalPanel(
#       #      condition = "input.runtype == 'retro.test'",
#       #      textInput('start.date',
#       #                'Date to start test (YYYY-MM-DD',
#       #                value=as.Date('2017-07-01')),
#       #      textInput('end.date',
#       #                'Date to end test (YYYY-MM-DD',
#       #                value=as.Date('2017-07-31'))
#       #    ),
#       #    
#       #    # If both retro and bulk
#       #    conditionalPanel(
#       #      condition = "input.runtype == 'retro.bulk'",
#       #      textInput('start.date',
#       #                'Date to start test (YYYY-MM-DD',
#       #                value='2017-06-30'),
#       #      textInput('end.date',
#       #                'Date to end test (YYYY-MM-DD',
#       #                value='2017-07-31'),
#       #      textInput('value.date',
#       #                'Date of bulk value (YYYY-MM-DD',
#       #                value='2017-08-01')
#       #    ),
#       #    
#       #    # Select with group of options to update
#       #     selectInput('optgroup',
#       #                 'Select Option Group',
#       #                 c('choose one'= 'none',
#       #                   'Pre-Defined' = 'preset',
#       #                   'Market' = 'market',
#       #                   'Models' = 'models',
#       #                   'Model Selection' = 'modsel',
#       #                   'Accuracy Analysis' = 'accr',
#       #                   'Fields'='fields',
#       #                   'Control'='control')
#       #     ),
#       #    
#       #     # If using a preset option group
#       #     conditionalPanel(
#       #       condition = "input.optgroup == 'preset'",
#       #       selectInput('preset',
#       #                   "Run as:",
#       #                   c('Quantum AVM' = 'qavm',
#       #                     'Greenfield AVM' = 'gavm',
#       #                     'Your Amazing AVM??' = 'aavm'))
#       #     ),
#       #    
#       #     # If updating the market options
#       #     conditionalPanel(
#       #       condition = "input.optgroup == 'market'",
#       #       selectInput('sm.type',
#       #                   'Submarketing Type',
#       #                   c('All Props' = 'global',
#       #                     'By a Field' = 'field',
#       #                     'One by One' = 'knn'))
#       #     ),
#       #    
#       #    # If updating manually
#       #    conditionalPanel(
#       #      condition = "input.optgroup != 'preset",
#       #      textInput('other.opts', 'Other Options',
#       #              value='Ex: "accr" | "fields" | "SitusZip"')
#       #    )
#        ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#         tabsetPanel(
#           tabPanel('Available Players', textOutput('text')),
#           tabPanel('By Position', textOutput('text')),
#           tabPanel('By Team', textOutput('text')),
#           tabPanel("My Team", textOutput("text"))
#       )
#      )
#    )
# )
# 
# ## Shiny Server ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# server <- function(input, output) {
# 
#   ## Change the Run Type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   chngRunType <- eventReactive(input$runtype,{
#     
#     if(input$runtype == 'retro.test'){
#       initChangeOptions(list(name='basic', run.type='retro.test'))
#     }
#     
#     if(input$runtype == 'bulk.value' | input$runtype == 'solo'){
#       initChangeOptions(list(name='basic', run.type='retro.test'))
#     }
#     
#   })
#   
#   ## Change the value date ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#   
#   chngValueDate <- eventReactive(input$value.date, {
#     
#     initChangeOptions(list(name='basic', value.date=as.Date(input$value.date)))
#     
#   })
#   
#   ## Change the start date for the retro. test run ~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   # chngRetroStart <- eventReactive(input$start.date, {
#   #   initChangeOptions(list(name='market', subname='retro.day', 
#   #                          start.date=as.Date(input$start.date)))
#   # })
#   # 
#   # ## Change the end date for the retro. test run ~~~~~~~~~~~~~~~~~~~~~~~~
#   # 
#   # chngRetroEnd <- eventReactive(input$end.date, {
#   #   initChangeOptions(list(name='market', subname='retro.day', 
#   #                          end.date=as.Date(input$end.date)))
#   # })
#   
#   ## Run the AVM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   runAVM <- eventReactive(input$runavm, {
#     
#     initChangeOptions(list(name='market', subname='retro.day', 
#                            start.date=as.Date(input$start.date)))
#     initChangeOptions(list(name='market', subname='retro.day', 
#                            end.date=as.Date(input$end.date)))
#     
#     try(platformAVM(fips=input$fips),
#         silent=TRUE)
#     
#   })
#   
#  ## Make the distribution plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#   
#   output$distPlot <- renderPlot({
#      
#      # Run Model
#       mod.ls <- runAVM()
#       
#      # Extract Values  
#       values.df <- mod.ls$values
#       
#      # Make Plot  
#       ggplot(values.df, aes(x=final_value)) + 
#         geom_density(color='navy', fill='blue', alpha=.3) +
#         xlab('Reconciled Value') + 
#         scale_x_log10(breaks=c(0,200000, 350000, 500000, 1000000, 5000000),
#                       labels=c('$0', '$200k', '350k', '$500k', '$1m', '$5m'))+
#         ylab('Frequency') +
#         theme(axis.text.y=element_blank(),
#               axis.ticks.y=element_blank())
#       
#    })
#   
#  ## Make the error distribution plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#   
#   output$errordistPlot <- renderPlot({
#     
#     # Run Model
#     mod.ls <- runAVM()
#     
#     # Extract value fields
#     values.df <- mod.ls$values %>% 
#       dplyr::select(uniq_id, sale_price, ols_value,  psf_value, rrm_value,
#                     tav_value, final_value)
#     
#     # Convert into tidy dataset
#     values.tdf <- reshape2::melt(values.df, id.vars=c('uniq_id', 'sale_price'))
#     
#     # Rename Factors
#     values.tdf <- values.tdf %>%
#       mutate(variable = fct_recode(variable,
#                                   "OLS"    = "ols_value",
#                                   "PPSF" = 'psf_value',
#                                   "RR"      = "rrm_value",
#                                   "Tax" = "tav_value",
#                                   "Reconciled" = "final_value"))
#     
#     # Calculate Error and build plot
#     v.plot <- values.tdf %>% 
#       dplyr::mutate(error=(sale_price - value) / sale_price) %>%
#       ggplot(aes(x=variable, y=abs(error), fill=variable)) +
#         geom_boxplot(alpha=.4) + 
#         scale_fill_manual(values=c('navy', 'green', 'orange', 'gray50', 'red'))+
#         coord_cartesian(ylim=c(0, .5)) +
#         xlab('\nModel Type\n') +
#         ylab('\nMedian Absolute Error\n') +
#         theme(legend.position='none')
#     
#     # Plot
#     v.plot
#     
#   }, height=600)
#   
#  ## Make Map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#   
#   output$map <- renderPlot({
# 
#     # Get Map boundaries
#     xx <- stash.box$bound.sf
#     
#     # Get map from web
#     x.map <- get_map(unname(st_bbox(xx)), scale=1, maptype='toner-2010', source='stamen')
# 
#     # Conver to Raster
#     x.rast <- ggmap_rast(map = x.map) # convert google map to raster object
#     crs(x.rast) <- st_crs(xx)$proj4string
#     
#     # Clip to county boundary
#     x.only <- mask(x.rast, as(xx, 'Spatial')) # clip to bounds of census tracts
# 
#     # Prep raster as a data frame for printing with ggplot
#     x.df <- data.frame(rasterToPoints(x.only))
#     
#     # Get the scoring data points
#     score.df <- stash.box$pst.obj$score.df
#     
#     # Make Plot
#     ggplot(x.df)+
#       geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255)), size=.9) +
#       scale_color_identity() +
#       geom_point(data=score.df, aes(x=long, y=lat, color='purple')) +
#       xlab('')+
#       ylab('')+
#       theme(axis.text.y=element_blank(),
#             axis.ticks.y=element_blank(),
#             axis.text.x=element_blank(),
#             axis.ticks.x=element_blank())
# 
#   }, height=600, execOnResize=TRUE)
#     
#  ## Retrieve AVM debugging output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#   
#   observeEvent(input$runavm, {
#      withCallingHandlers({
#        shinyjs::html("text", "")
#        runAVM()
#      },
#      message = function(m) {
#        shinyjs::html(id = "text", 
#                      html = gsub(" ", "&nbsp", 
#                                  paste0('<font color="#1FA050">', '<strong>', 
#                                         m$message,
#                                         '</strong>','</font>', '<br/>')),
#                      add = TRUE)
#      })
#    })
#   
# } # Ends Server
# 
# ## Execute the Application ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# shinyApp(ui = ui, server = server)
# 
# #*****************************************************************************************
# #*****************************************************************************************