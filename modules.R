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
                     choices = data_levels,
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
pca_dat_server <- function(id, dat, spikeIn){
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
               `Compound ID` %in% input$sampleOrderSelect) %>% 
        mutate(`Compound ID` = factor(`Compound ID`, levels = unique(`Compound ID`)))
      
      pdat <- pc %>% 
        select(`Compound ID`,`Dose Label`, plate) %>% 
        as.data.frame()
      
      if (spikeIn == TRUE){
        pc <- pc %>%
          select(`Spike Count`:var_peakToPeak_pV) %>%
          as.matrix()
      } else {
        pc <- pc %>%
          select(`Spike Count`:`Mean Network Interburst Interval [µs]`) %>%
          as.matrix()
      }
      
      rownames(pc) <- seq(1:nrow(pc))
      
      pc <- t(pc)
      
      pcaObj <- pca(pc, pdat, scale = TRUE, removeVar = 0.2)
    })
    
    output$screePlot <- renderPlot({
      screeplot(pcaDat())
    })
    
    output$pcaPlot <- renderPlot({
      PCAtools::biplot(pcaDat(),
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
             ellipseLevel = input$pca.ellipse.conf,
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
    box(title = "Select Features to Plot",
        uiOutput(ns("doseLabel")),
        uiOutput(ns("sampOrder")),
        uiOutput(ns("datTable")),
        actionButton(ns("makePlot"), "Select Features and Plot")
    ),
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
    box(title = "Scatter Plot", plotlyOutput(outputId = ns("scatter_box"))
    )
  )
}


##################################
# Server side actions ingests PCA object for plotting
# Requires variables from other modules which have extracted from table
# Include dose label and sample orders. 
#
scatter_plot_server <- function(id, dataIn){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$datTable <- renderUI({
      ns <- session$ns
      coi <- dataIn %>% 
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
    
    output$sampOrder <- renderUI({
      data_available <- dataIn %>% 
        select(`Compound ID`) %>% 
        pull()
      
      data_levels <- unique(data_available)
      data_levels <- data_levels[order(data_levels)]
      data_levels <- factor(data_levels,levels = data_levels)
      
      selectizeInput(inputId = ns("sampleOrderSelect"),
                     label = "Group Select / Order :", 
                     choices = data_levels,
                     multiple = T, 
                     selected = NULL)
    })
    output$doseLabel <- renderUI({
      data_available <- dataIn %>% 
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
    
    well.table <- eventReactive(input$makePlot, {
      scatter.test <- dataIn %>% 
        dplyr::filter(`Dose Label` == input$doseLabelSelect) %>% 
        dplyr::filter(`Compound ID` %in% input$sampleOrderSelect) %>% 
        mutate(`Compound ID` := factor(`Compound ID`, levels = input$sampleOrderSelect))
    })
    
    plot <- reactive({
                 p <- well.table() %>% 
                      ggplot(aes(y = !!rlang::sym(input$dat), #### server output
                                 x = `Compound ID`,
                                 col= `Compound ID`)) +
                      geom_hline(yintercept = 0, alpha=0.5,linetype=2) +
                      geom_jitter(position=position_jitter(width=0.3, height=0.2),size=input$point.size, alpha=0.9) +
                      geom_boxplot(alpha = 0.5, show.legend = FALSE,col="black",width=input$box.width,lwd=0.8) +
                      theme_classic() +
                      ggtitle(input$doseLabelSelect) +
                      labs(y = input$dat) +
                      theme(
                        axis.text = element_text(size = input$text.size,face = "bold"),
                        axis.title = element_text(size = input$text.size*1.3,face = "bold"),
                        axis.title.x = element_blank(),
                        legend.position = "none"
                      )
    })
    
      output$scatter_box <- renderPlotly({
        ggplotly(plot())
    })
  })
}

###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
read_testFile_UI <- function(id) {
  ns <- NS(id)
  fileInput(ns('file'), 'Load Treatment MEA File (UTF-8 .csv file)',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            ))
}

##################################
# Server side actions. Create a file reader so that files are read in and then
# processed to extract the plate ID and set up of unique keys and maybe file
# name too (for multiple files). Requires as input the dat file input, 
# takes in the static inputs to remove inactive wells and at which threshold
# @filter yes or no from UI
# @filterLevel the slider level input
#
read_testFile_server <- function(id, filter, filterLevel, readFile){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    info <- eventReactive(readFile, {
      inFile <- input$file
      req(inFile)
      f <- read_csv(inFile$datapath)
      f <- f %>% 
        filter(`Dose Label` != "Control") %>% # remove this data reading from drug 2 
        mutate(plate = str_extract(Experiment, "_[0-9][0-9][0-9][0-9]_")) %>% #Extract plate ID
        mutate(plate = as.numeric(gsub("_", "", plate))) %>% # reformat to numeric
        mutate(key = paste0(`Channel ID`, "_",
                            plate),
               key2 = paste0(`Channel ID`, "_",
                             `Dose Label`, "_", # need this for PCA
                             plate),
               key3 = paste0(plate, "_", `Well ID`, "_", `Dose Label`)) %>% 
        select(-`Active Channel`)
      
      ifelse(filter ==TRUE,
             return(f %>% filter(`Spike Rate [Hz]` >= filterLevel)),
             return(f))
    }) 

  })
}

###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
read_controlFile_UI <- function(id) {
  ns <- NS(id)
  fileInput(ns("file"), 'Load Control MEA File (UTF-8 .csv file)',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            ))
}

##################################
# Server side actions. Create a file reader so that files are read in and then
# processed to extract the plate ID and set up of unique keys and maybe file
# name too (for multiple files). Requires as input the dat file input, 
# takes in the static inputs to remove inactive wells and at which threshold
# @filter yes or no from UI
# @filterLevel the slider level input
#
read_controlFile_server <- function(id, filter, filterLevel, readFile){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    info <- eventReactive(readFile, {
      inFile <- input$file
      req(inFile)
      f <- read_csv(inFile$datapath)
      f <- f %>% 
        filter(`Dose Label` == "Control") %>% # remove this data reading from drug 2 
        mutate(plate = str_extract(Experiment, "_[0-9][0-9][0-9][0-9]_")) %>% #Extract plate ID
        mutate(plate = as.numeric(gsub("_", "", plate))) %>% # reformat to numeric
        mutate(key = paste0(`Channel ID`, "_",
                            plate),
               key2 = paste0(`Channel ID`, "_",
                             `Dose Label`, "_", # need this for PCA
                             plate),
               key3 = paste0(plate, "_", `Well ID`, "_", `Dose Label`)) %>% 
        select(-`Active Channel`)
      
      ifelse(filter ==TRUE,
             return(f %>% filter(`Spike Rate [Hz]` >= filterLevel)),
             return(f))
    }) 
    
  })
}

###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
read_testSpike_UI <- function(id) {
  ns <- NS(id)
  fileInput(ns("spike.test"), 'Load Treatment Spike Data (.zip file, compressed csv)',
            accept = '.zip')
}

##################################
# Server side actions. Create a file reader so that files are read in and then
# processed to extract the plate ID and set up of unique keys and maybe file
# name too (for multiple files). Requires as input the dat file input, 
# takes in the static inputs to remove inactive wells and at which threshold
# @filterFile the filtered measurement file to extract the channels of interest
# @filter use all the data or not filter yes or no
#
read_testSpike_server <- function(id, filterFile, filter, readFile){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    info <- eventReactive(readFile, {
      test <- filterFile
      input <- input$spike.test
      req(input)
      f <- read_csv(input$datapath)
      f <- f %>% 
        filter(`Dose Label` != "Control") %>% #select test data only
        mutate(plate = str_extract(Experiment, "_[0-9][0-9][0-9][0-9]_")) %>% #Extract plate ID
        mutate(plate = as.numeric(gsub("_", "", plate))) %>% # reformat to numeric
        mutate(key = paste0(`Channel ID`, "_",
                            plate),
               key2 = paste0(`Channel ID`, "_",
                             `Dose Label`, "_", # need this for PCA
                             plate),
               key3 = paste0(plate, "_", `Well ID`, "_", `Dose Label`)) #set up channel ID and experiment
      ifelse(filter==TRUE,
             return(f %>% filter(key %in% test$key)),
             return(f))

    }) 
    
  })
}

###############################################################################
# Create a module to visualise the PCA plot and also tweak it's appearance as 
# necessary with a particular data table going in
read_controlSpike_UI <- function(id) {
  ns <- NS(id)
  fileInput(ns("spike.baseline"), 'Load Baseline Spike Data (.zip file, compressed csv)',
            accept = '.zip')
}

##################################
# Server side actions. Create a file reader so that files are read in and then
# processed to extract the plate ID and set up of unique keys and maybe file
# name too (for multiple files). Requires as input the dat file input, 
# takes in the static inputs to remove inactive wells and at which threshold
# @filter yes or no from UI
# @filterLevel the slider level input
#
read_controlSpike_server <- function(id, filterFile, filter, readFile){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    info <- eventReactive(readFile, {
      base <- filterFile
      inFile <- input$spike.baseline
      req(inFile)
      f <- read_csv(inFile$datapath)
      f <- f %>% 
        filter(`Dose Label` == "Control") %>% # remove this data reading from drug 2 
        mutate(plate = str_extract(Experiment, "_[0-9][0-9][0-9][0-9]_")) %>% #Extract plate ID
        mutate(plate = as.numeric(gsub("_", "", plate))) %>% # reformat to numeric
        mutate(key = paste0(`Channel ID`, "_",
                            plate),
               key2 = paste0(`Channel ID`, "_",
                             `Dose Label`, "_", # need this for PCA
                             plate),
               key3 = paste0(plate, "_", `Well ID`, "_", `Dose Label`))
      
      ifelse(filter ==TRUE,
             return(f %>% filter(key %in% base$key)),
             return(f))
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


