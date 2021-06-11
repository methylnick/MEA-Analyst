###############################################################################
# Create a module to load and display a table rendering accepts a table call
# from outTable

displayDT_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      DTOutput(ns("outTable"))
    )
  )
}

##################################
# Module server side requires std parameters plus
# dat which is the object you want to present

displayDT_server <- function(id, dat) { 
  moduleServer(id, function(input, output, session) {
    output$outTable <- renderDT({DT::datatable(dat, options = list(scrollX = TRUE))})
                                                    }
  )}

###############################################################################
# Create a module to extract and select the appropriate features for plotting
# histograms
extractMeasurementColumns_UI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        uiOutput(ns("datTable"))
      )
    )
  )
}


##################################
# Server side actions have the session id and also the data
# object dat where you want to extract the columns from.
extractMeasurementColumns_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    output$datTable <- renderUI({
      ns <- session$ns
      coi <- dat %>% 
        dplyr::select(where(is.numeric))
      cols <- colnames(coi)

      selectInput(
        ns("dat"),
        "Select measurement to plot:",
        choices = cols,
        selected = "Spike Count",
        multiple = FALSE
      )
    })
    return(reactive({
      validate(need(input$dat, FALSE))
      input$dat
    }))
  })
}

###############################################################################
# Create a module to select the treatment and their order of selection for 
# scatter plots
extractScatter_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      uiOutput(ns("sampOrder")),
      uiOutput(ns("doseLabel"))
    )
  )
}


##################################
# Server side action
#
extractScatter_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$sampOrder <- renderUI({
      data_available <- dat %>% 
        select(`Compound ID`) %>% 
        pull()
      
      data_levels <- unique(data_available)
      
      data_available <- factor(data_available,levels = data_levels)
      
      selectizeInput(inputId = ns("sampleOrder"),
                     label = "Group Select / Order :", 
                     choices = data_available,
                     multiple = T, 
                     selected = NULL)
    })
    output$doseLabel <- renderUI({
      data_available <- dat %>% 
        select(`Dose Label`) %>% 
        distinct() %>% 
        pull()
      
      data_available <- unique(data_available)
      
      selectInput(inputId = ns("doseLabelSelect"),
                  label = "Select Dose Group :", 
                  choices = data_available,
                  multiple = F, 
                  selected = NULL)
    })
    return(list(
      sampleOrder = reactive({
        validate(need(input$sampleOrder, FALSE))
        input$sampleOrder}),
      doseLabelSelect = reactive({
        validate(need(input$doseLabelSelect, FALSE))
        input$doseLabelSelect})
    ))
  })
}



###############################################################################
# Create a module to load the data and then add the appropriate keys for 
# downstream wrangling



##################################
# Server side actions
#



###############################################################################
# Create a module to load the data and then add the appropriate keys for 
# downstream wrangling



##################################
# Server side actions
#