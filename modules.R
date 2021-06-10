###############################################################################
# Create a module to load and display a table rendering accepts a table call
#

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
  shiny::moduleServer(id, function(input, output, session) {
    output$outTable <- renderDT({DT::datatable(dat, options = list(scrollX = TRUE))})
                                                    }
)}