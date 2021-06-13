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
# scatter plots one dose label at a time.
extractScatter_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(
      uiOutput(ns("sampOrder")),
      uiOutput(ns("doseLabel"))
    )
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
      
      data_levels <- data_levels[order(data_levels)]
      
      data_levels <- factor(data_levels,levels = data_levels)
      
      selectizeInput(inputId = ns("sampleOrderSelect"),
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
      doseLabelSelect = reactive({
        validate(need(input$doseLabelSelect, FALSE))
        input$doseLabelSelect
      }),
      sampleOrderSelect = reactive({
        validate(need(input$sampleOrderSelect, FALSE))
        input$sampleOrderSelect
      })
    )
    )
  })
}



###############################################################################
# Create a module to wrangle a data table into a PCA object for downstream 
# PCA plotting include UI dropdowns to subset the data object before it is
# processed into a PCA object for downstream plotting. Make the groups
# user selectable with selectize
pca_dat_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
    box(
      h3("Select Groups and Treatments"),
      uiOutput(ns("doseLabel")),
      uiOutput(ns("sampOrder")),
      h3("Circle Configuration"),
      checkboxInput(inputId = ns("pca.circle"),
                    "Draw circles",
                    FALSE),
      sliderInput(inputId = ns("pca.circle.line"),
                  step = 0.25,
                  label = "Circle line size:",
                  min = 0.1,
                  max = 5,
                  value = 0.25
      ),
      actionButton(ns("processPCA"), "Select Groups and Plot PCA")
    ),
    box(h3("Confidence Ellipse Configuration"),
        checkboxInput(inputId = ns("pca.ellipse"),
                      "Draw confidence ellipses",
                      FALSE),
        sliderInput(ns("pca.ellipse.conf"), "Set confidence interval: ", min = 0.5, max = 1, value = 0.95),
        h3("PCA Loadings Configuration"),
        checkboxInput(inputId = ns("pca.loadings"),
                      "Show Component Loading",
                      FALSE),
        sliderInput(inputId = ns("pca.n.loadings"),
                    step = 1,
                    label = "Select number of loadings : ",
                    min = 2,
                    max = 15,
                    value = 5)
    )
    ),
    fluidRow(
      box(
        plotOutput(ns("screePlot"))
          ),
      box(
        plotOutput(ns("pcaPlot"))
          )
    )
  )
}


##################################
# Server side actions for PCA
#
pca_dat_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$sampOrder <- renderUI({
      data_available <- dat %>% 
        select(`Compound ID`) %>% 
        pull()
      
      data_levels <- unique(data_available)
      data_levels <- data_levels[order(data_levels)]
      
      data_available <- factor(data_levels,levels = data_levels)
      
      selectizeInput(inputId = ns("sampleOrderSelect"),
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
      
      selectizeInput(inputId = ns("doseLabelSelect"),
                  label = "Select Dose Group :", 
                  choices = data_available,
                  multiple = T, 
                  selected = data_available[1:2])
    })
    
    pcaDat <- eventReactive(input$processPCA, {
      pc <- dat %>%
        mutate(across(everything(), ~replace_na(.x, 0)))
      
      pc <- pc %>% 
        filter(`Dose Label` %in% input$doseLabelSelect,
               `Compound ID` %in% input$sampleOrderSelect)
      
      pdat <- pc %>% 
        select(`Compound ID`:`Dose Label`) %>% 
        as.data.frame()
      
      pc <- pc %>%
        select(`Spike Count`:var_peakToPeak_pV) %>%
        as.matrix()
      
      rownames(pc) <- seq(1:nrow(pc))
      
      pc <- t(pc)
      
      pcaObj <- pca(pc, pdat, scale = TRUE, removeVar = 0.2)
    })
    
    output$screePlot <- renderPlot({
      screeplot(pcaDat())
    })
    
    output$pcaPlot <- renderPlot({
      biplot(pcaDat(),
             showLoadings = input$pca.loadings,
             ntopLoadings = input$pca.n.loadings,
             lab = NULL, 
             legendPosition = "right", 
             colby = "Dose Label", 
             shape = "Compound ID",
             encircle = input$pca.circle, 
             encircleFill = input$pca.circle,
             encircleLineSize = input$pca.circle.line,
             encircleLineCol = "black",
             ellipse = input$pca.ellipse,
             ellipseConf = input$pca.ellipse.conf,
             ellipseFill = input$pca.ellipse,
             ellipseLineSize = input$pca.circle.line)
      
    })
  })
}


###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
scatter_plot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Tweaks to the Charts",
        sliderInput(inputId = ns("point.size"),step = 0.5,
                    label = "Point Size :",
                    min = 0,
                    max = 8,
                    value = 3),
        sliderInput(inputId = ns("box.width"),step = 0.05,
                    label = "Box Width :",
                    min = 0.05,
                    max = 0.8,
                    value = 0.5),
        sliderInput(inputId = ns("ylim.rel"),step = 100,
                    label = "y limit %Rel :",
                    min = 200,
                    max = 2000,
                    value = 800),
        sliderInput(inputId = ns("text.size"),step = 1,
                    label = "Text Size :",
                    min = 0,
                    max = 30,
                    value = 6)
    ),
    box(title = " - Raw Data", plotlyOutput(outputId = ns("scatter_box"))
    )
  )
}


##################################
# Server side actions ingests PCA object for plotting
# Requires a yVarInput to select the appropriate y value for plotting
scatter_plot_server <- function(id, dat, yvarInput){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
      output$scatter_box <- renderPlotly({
        ggplotly(
          dat %>% 
            ggplot(aes(y = !!rlang::sym(yvarInput), #### server output
                       x = `Compound ID`,
                       col= `Compound ID`,
                       label = `key3`,
                       label2 = `plate`)) +
            geom_hline(yintercept = 0, alpha=0.5,linetype=2) +
            geom_jitter(position=position_jitter(width=0.3, height=0.2),size=input$point.size, alpha=0.9) +
            geom_boxplot(alpha = 0.5, show.legend = FALSE,col="black",width=input$box.width,lwd=0.8) +
            theme_classic() +
            labs(y=(yvarInput)) +
            theme(
              axis.text = element_text(size = input$text.size,face = "bold"),
              axis.title = element_text(size = input$text.size*1.3,face = "bold"),
              axis.title.x = element_blank(),
              legend.position = "none"
            )
        )
    })
  })
}

###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
# pca_plot_UI <- function(id) {
#   
# }


##################################
# Server side actions
#
# pca_plot_server <- function(id, dat){
#   moduleServer(id, function(input, output, session) {
#     
#   })
# }



###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
# pca_plot_UI <- function(id) {
#   
# }


##################################
# Server side actions
#
# pca_plot_server <- function(id, dat){
#   moduleServer(id, function(input, output, session) {
#     
#   })
# }



###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
# pca_plot_UI <- function(id) {
#   
# }


##################################
# Server side actions
#
# pca_plot_server <- function(id, dat){
#   moduleServer(id, function(input, output, session) {
#     
#   })
# }


###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
# pca_plot_UI <- function(id) {
#   
# }


##################################
# Server side actions
#
# pca_plot_server <- function(id, dat){
#   moduleServer(id, function(input, output, session) {
#     
#   })
# }


