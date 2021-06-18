# MEA Visualiser Take 3
# Modularise some parts of the app code so to reduce the load and confusion 
# with shiny object identifiers.

library(shiny)
library(tidyverse)
library(DT)
library(ggbeeswarm)
library(PCAtools)
library(ggpubr)
library(rstatix)
library(ggalt)
library(plotly)
library(shinydashboard)
library(colourpicker)

# Source Modules here
source("modules.R")
source("helperFunctions.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "MEA Visualiser v3"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Upload Files", tabName = "dataIn", icon = icon("database"),
                menuSubItem(
                    "Upload Raw MEA Files", tabName = "dataIn", icon = icon("database")
                ),
                # menuSubItem(
                #     "Upload Multiple MEA Files", tabName = "dataInMulti", icon = icon("database")
                #     ),
                menuSubItem(
                    "Preview Data Tables", tabName = "loadedTables", icon = icon("table")
                ),
                menuSubItem(
                    "Preview Spike Data Tables", tabName = "spikeTables", icon = icon("table")
                ),
                menuSubItem(
                    "Preview Spike Calculated Table", tabName = "spikeTablesMean", icon = icon("table")
                ) 
            ),
            menuItem(
                "Plots - By Channel", tabName = "plots", icon = icon("chart-bar"),
                menuSubItem(
                    "Histogram Plots", tabName = "plots", icon = icon("chart-bar")
                ),
                menuSubItem(
                    "Scatter Plots", tabName = "scatPlots", icon = icon("braille")
                ),
                menuSubItem(
                    "PCA Plots", tabName = "pcaPlots", icon = icon("blackberry")
                ),
                menuSubItem(
                    "Epilepiform Plots (PCA)", tabName = "epiPlots", icon = icon("user")
                )
            ),
            menuItem("Statistical Testing - By Channel", tabName = "stats", icon = icon("ruler-combined"),
                     menuSubItem(
                         "Plotting Statsticial Significance", tabName = "stats", icon = icon("ruler")
                     )
            ),
            menuItem(
                "Plots - By Well", tabName = "Wplots", icon = icon("chart-bar"),
                menuSubItem(
                    "Scatter Plots - Raw", tabName = "WscatPlots", icon = icon("braille")
                ),
                menuSubItem(
                    "Scatter Plots - Normalised", tabName = "WscatPlotsNorm", icon = icon("braille")
                ),
                menuSubItem(
                    "PCA Plots", tabName = "WpcaPlots", icon = icon("blackberry")
                ),
                menuSubItem(
                    "Epilepiform Plots (PCA)", tabName = "WepiPlots", icon = icon("user")
                )
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dataIn",
                    fluidRow(box(read_testFile_UI("testIn"),
                                 read_controlFile_UI("controlIn"),
                                 read_testSpike_UI("testSpike"),
                                 read_controlSpike_UI("controlSpike")
                    ),
                    box(title = "Formatting and Filtering",
                        checkboxInput('rm.inactive', 'Remove inactive channels - Test', FALSE),
                        sliderInput(inputId = "sp.level",
                                    label = "Minimum Spike Rate Threshold (Hz) - Test :",
                                    min = 0,
                                    max = 10,
                                    value = 1),
                        checkboxInput('rm.inactive.base', 'Remove inactive channels - Baseline', FALSE),
                        sliderInput(inputId = "sp.level.baseline",
                                    label = "Minimum Spike Rate Threshold (Hz) - Baseline :",
                                    min = 0,
                                    max = 10,
                                    value = 1),
                        h4("Spike Data"),
                        checkboxInput("includeSpike", "Include Spike Data Files", FALSE),
                        actionButton("choice", "Click To Import Data")
                    )
                    )
                    
            ),
            tabItem(tabName = "loadedTables",
                    title = "Tables Loaded and Ready to Plot",
                    fluidRow(h3("Test - MEA file"), displayDT_ui("test")),
                    fluidRow(h3("Baseline - MEA file"), displayDT_ui("base"))
            ),
            tabItem(tabName = "spikeTables",
                    title = "Spike Data Tables Loaded",
                    fluidRow(h3("Test Spike Data"), displayDT_ui("s.test")),
                    fluidRow(h3("Baseline Spike Data"), displayDT_ui("s.base"))
            ),
            tabItem(tabName = "spikeTablesMean",
                    title = "Calculated Data Tables for App",
                    fluidRow(h3("Summarised Spike Table"), displayDT_ui("s.table")),
                    fluidRow(h3("Table for Scatter Plots"), displayDT_ui("sc.table")),
                    fluidRow(h3("Uber Table"), displayDT_ui("u.table")),
                    fluidRow(h3("Normalised Uber Table"), displayDT_ui("n.u.table")),
                    fluidRow(h3("Epi Table"), displayDT_ui("e.table")),
                    fluidRow(h3("Well normalised Table"), displayDT_ui("w.scat"))
            ),
            tabItem(tabName = "plots",
                    title = "Histogram Plots",
                    fluidRow(box(title = "Histogram Options",
                                 extractMeasurementColumns_UI("histCols"),
                                 sliderInput(inputId = "bins",
                                             label = "Histogram - No# of bins :",
                                             min = 1,
                                             max = 50,
                                             value = 30)),
                             box(plotOutput(outputId = "distPlot"))),
                    fluidRow(box(plotOutput(outputId = "distPlot2")),
                             box(plotOutput(outputId = "distPlot3"))
                    )
            ),
            tabItem(tabName = "scatPlots",
                    outputId = "scatter_box_text",
                    fluidRow(box(title = "Scatter Plot Configuration",
                                 extractScatter_UI("channelScatter"),
                                 extractMeasurementColumns_UI("colsScatter"),
                                 actionButton("plotScatter", "Select Groups and Plot")
                    ),
                    box(title = "Tweaks to the Charts",
                        sliderInput(inputId = "point.size",step = 0.5,
                                    label = "Point Size :",
                                    min = 0,
                                    max = 8,
                                    value = 3),
                        sliderInput(inputId = "box.width",step = 0.05,
                                    label = "Box Width :",
                                    min = 0.05,
                                    max = 0.8,
                                    value = 0.5),
                        sliderInput(inputId = "ylim.rel",step = 100,
                                    label = "y limit %Rel :",
                                    min = 200,
                                    max = 2000,
                                    value = 800),
                        sliderInput(inputId = "text.size",step = 1,
                                    label = "Text Size :",
                                    min = 0,
                                    max = 30,
                                    value = 6),
                        downloadButton("downloadData", "Download Normalised Data")
                    )
                    
                    ),
                    fluidRow(box(title = " - Raw Data", plotlyOutput(outputId = "scatter_box")),
                             box(title = " - Relative to Baseline ( % ) - 'jitter'", plotlyOutput(outputId = "scatter_box_rel"))
                    ),
                    fluidRow(box(title =" - Relative to Baseline ( % ) - 'beeswarm'", plotlyOutput(outputId = "swarm_box_rel")),
                             box(displayDT_ui("n.table.out"))
                    )
                    
            ),
            tabItem(tabName = "stats",
                    title = "Statstical Analysis from Scatter Plots",
                    fluidRow(box(title = "Statistical Significance Plots Parameters",
                                 sliderInput(inputId = "stat.y.adj",step = 0.001,
                                             label = "Adjust signif marks y-position :",
                                             min = 0.001,
                                             max = 1,
                                             value = 1),
                                 checkboxInput('stats.compare.all', 'Table: Show all contrasts?', FALSE),
                                 checkboxInput('sort.by.pval', 'Table: Sort by p value', FALSE),
                                 downloadButton("down.swarm.box.rel.stat", "Export plot")),
                             box(plotOutput(outputId = "stats"))),
                    fluidRow(h3("Table of Significance Values"),
                             tableOutput(outputId = "table_stats")
                    )
            ),
            tabItem(tabName = "pcaPlots",
            title = "PCA Plot",
            pca_dat_UI("uberTable")
            ),
            tabItem(tabName = "epiPlots",
                    title = "Epileptiform Plot",
                    pca_dat_UI("epiTable")
                    ),
            tabItem(tabName = "WscatPlots",
                    title = "Scatter Plots - Raw",
                        box(
                            extractScatter_UI("wellChScatter"),
                            extractMeasurementColumns_UI("wellcolsScatter"),
                            actionButton("WplotScatter", "Select Groups and Plot")
                        ),
                    fluidRow(
                    scatter_plot_UI("wellTable")
                    )
            ),
            tabItem(tabName = "WscatPlotsNorm",
                    title = "Scatter Plots - Normalised",
                    box(
                        extractScatter_UI("wellChScatterNorm"),
                        extractMeasurementColumns_UI("wellcolsScatterNorm"),
                        actionButton("WplotScatter2", "Select Groups and Plot")
                    ),
                    fluidRow(
                        scatter_plot_UI("wellTableNorm")
                    )
            ),
            tabItem(tabName = "WpcaPlots",
                    title = "PCA Plot",
                    pca_dat_UI("wellTable")
            ),
            tabItem(tabName = "WepiPlots",
                    title = "Epileptiform Plot",
                    pca_dat_UI("epiWellTable")
            )
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
    ###############################################################################
    options(shiny.maxRequestSize=30*1024^2)
    
    ##############################################################################
    # Read in the test data file measurements
    info <- read_testFile_server("testIn", filter = input$rm.inactive, 
                                 filterLevel = input$sp.level, 
                                 readFile = input$choice)
    
    ##############################################################################
    # Read in the control data file measurements
    info.base <- read_controlFile_server("controlIn", filter = input$rm.inactive.base, 
                                         filterLevel = input$sp.level.baseline, 
                                         readFile = input$choice)
    
    ##############################################################################
    # This is the baseline spike table
    # channels
    sp.test <- read_testSpike_server("testSpike", filterFile = info(), 
                                     filter = input$rm.inactive, 
                                     readFile = input$choice)
    
    ##############################################################################
    # This is the baseline table and is set to either include or remove inactive
    # channels
    sp.base <- read_controlSpike_server("controlSpike", filterFile = info.base(), 
                                        filter = input$rm.inactive.base,
                                        readFile = input$choice)
    
    ##############################################################################
    # Make a cleaned and filtered data table for downstream analysis and plotting 
    # This is the raw spike and time course data across all channels and wells 
    # and it will contain only data that have been filtered for spike rate. 
    # 
    # Calculate the mean and variance min/max peak and peak to peak data for
    # inclusion into the PCA plots.
    # 
    spike.table <- eventReactive(input$choice, {
        if (input$includeSpike == TRUE) {
            scatter.test <- sp.test()
            ctrl <- sp.base()
            
            scatter.test <- scatter.test %>% 
                filter(key %in% ctrl$key) 
            
            scatter.test <- bind_rows(scatter.test, ctrl)
            
            scatter.test <- scatter.test %>% 
                group_by(key2) %>% 
                summarize(mean_maxAmp_pV = mean(`Maximum Amplitude [pV]`), var_maxAmp_pV = var(`Maximum Amplitude [pV]`),
                          mean_minAmp_pV = mean(`Minimum Amplitude [pV]`), var_minAmp_pV = var(`Minimum Amplitude [pV]`),
                          mean_peakToPeak_pV = mean(`Peak-to-peak Amplitude [pV]`), 
                          var_peakToPeak_pV = var(`Peak-to-peak Amplitude [pV]`)) %>% 
                tibble()
        } else {
            NULL
        }
        
        
        
    })
    
    ##############################################################################
    # Make a cleaned and filtered data table for downstream analysis and plotting 
    # This will collect and apply the filters for spike rate between the two 
    # data tables and then be used to present and work with plotting
    # 
    scat.table <- eventReactive(input$choice, {
        scatter.test <- info()
        ctrl <- info.base()
        spike.test <- spike.table()
        
        scatter.test <- scatter.test %>% 
            filter(key %in% ctrl$key) 
        
        scatter.test <- bind_rows(scatter.test, ctrl)
        
        #insert check if we have spike data to include or not
        if (input$includeSpike == TRUE){
            scatter.test <- left_join(scatter.test, spike.test, by = "key2")
        } else{
            return(scatter.test)
        }
        
        #need to include test for spike data here too
        
        if (input$includeSpike == TRUE) {
            scatter.test <- scatter.test %>% 
                dplyr::select(`Channel ID`:`Dose Label`, plate:key3, `Spike Count`:`Mean Network Interburst Interval [µs]`,
                              mean_maxAmp_pV:var_peakToPeak_pV)
        } else {
            scatter.test <- scatter.test %>% 
                dplyr::select(`Channel ID`:`Dose Label`, plate:key3, 
                              `Spike Count`:`Mean Network Interburst Interval [µs]`)
        }

        
    })
    
    ############################################################################## 
    # Lets try and modularise this for output into the UI
    #
    displayDT_server(id ="test", dat = info())
    displayDT_server(id ="base", dat = info.base())
    displayDT_server(id ="s.base", dat = sp.base())
    displayDT_server(id ="s.test", dat = sp.test())
    displayDT_server(id ="s.table", dat = spike.table())
    displayDT_server(id ="u.table", dat = uber.table())
    displayDT_server(id ="sc.table", dat = scat.table())          # raw data table for downstream wrangling
    displayDT_server(id ="n.u.table", dat = norm.uber.table())    # all variables normalised to control
    displayDT_server(id ="n.table.out", dat = norm.table.out())   # selected variable for table output
    displayDT_server(id ="e.table", dat = epi.table())     # epileptiform table (normalised for PCA)
    displayDT_server(id ="w.scat", dat = well.scat())      # well average table

    ############################################################################## 
    # Lets try and modularise measurement column extraction for selection in 
    # graphs then extract out the value for plotting
    hcols <- extractMeasurementColumns_server(id = "histCols", dat = scat.table())
    
    
    ##############################################################################
    output$distPlot <- renderPlot({
        f <- scat.table()
        f %>% 
            select(!!rlang::sym(hcols())) %>% 
            drop_na() %>% 
            ggplot(aes(x = !!rlang::sym(hcols()))) +
            geom_histogram(bins = input$bins) +
            ggtitle(paste0("Histogram - ",hcols())) +
            theme(axis.title.x = element_blank())
    })
    
    ##############################################################################
    output$distPlot2 <- renderPlot({
        f <- scat.table()
        f %>% 
            select(!!rlang::sym(hcols()), `Dose Label`) %>% 
            drop_na() %>% 
            ggplot(aes(x = !!rlang::sym(hcols()), colour = `Dose Label`)) +
            geom_histogram(bins = input$bins, fill = "white", position = "dodge") +
            ggtitle(paste0("Histogram - ",hcols())) +
            theme(axis.title.x = element_blank())
    })
    
    ##############################################################################
    output$distPlot3 <- renderPlot({
        f <- scat.table()
        f %>% 
            select(!!rlang::sym(hcols()), `Compound ID`) %>% 
            drop_na() %>% 
            ggplot(aes(x = !!rlang::sym(hcols()), colour = `Compound ID`)) +
            geom_histogram(bins = input$bins, fill = "white", position = "dodge") +
            ggtitle(paste0("Histogram - ",hcols())) +
            theme(axis.title.x = element_blank())  
    })
    
    ############################################################################## 
    # Modularise selection columns extraction for 
    # graphs then extract out the value for plotting
    scatterChannel <- extractScatter_server(id = "channelScatter", dat = scat.table())
    scatterCols <- extractMeasurementColumns_server(id = "colsScatter", dat = scat.table())
    
    ##############################################################################
    # With the uber dataset, now create the data structure for plotting of the 
    # scatter and PCA's after filtering
    uber.table <- eventReactive(input$plotScatter, {
        
        scatter.test <- scat.table() %>% 
            dplyr::filter(`Dose Label` == scatterChannel$doseLabelSelect()) %>% 
            dplyr::filter(`Compound ID` %in% scatterChannel$sampleOrder()) %>% 
            mutate(`Compound ID` := factor(`Compound ID`, levels = scatterChannel$sampleOrder()))
        
        return(scatter.test)
    })
    
    
    ##############################################################################
    # Create a plotly scatter object by channel
    output$scatter_box <- renderPlotly({
        ggplotly(
            uber.table() %>% 
                ggplot(aes(y = !!rlang::sym(scatterCols()), 
                           x = `Compound ID`,
                           col= `Compound ID`,
                           label = `Channel ID`,
                           label2 = `Channel Label`,
                           label3 = `Well ID`,
                           label4 = plate)) +
                geom_hline(yintercept = 0, alpha=0.5,linetype=2) +
                geom_jitter(position=position_jitter(width=0.3, height=0.2),size=input$point.size, alpha=0.9) +
                geom_boxplot(alpha = 0.5, show.legend = FALSE,col="black",width=input$box.width,lwd=0.8) +
                theme_classic() +
                labs(y=(scatterCols())) +
                theme(
                    axis.text = element_text(size = input$text.size,face = "bold"),
                    axis.title = element_text(size = input$text.size*1.3,face = "bold"),
                    axis.title.x = element_blank(),
                    legend.position = "none"
                )
        )
    })
    
    ##############################################################################
    # Create a normalised table of measurements from the uber table that has been
    # selected and filtered for the variables of interest. Calculate the normalised
    # values for all measures and columns of interest. 
    # 
    norm.uber.table <- eventReactive(input$plotScatter, {
        scatter.test <- scat.table()
        
        ctrl <- scatter.test %>% 
            filter(`Dose Label` == "Control")
        
        scatter.test <- left_join(ctrl, scatter.test, by = "key")
        
        colnames(scatter.test) <- gsub("\\.y", "", colnames(scatter.test))
        
        #need to incorporate an if statement here for the spike data for input$includeSpike
        
        if (input$includeSpike == TRUE) {
            scatter.test <- scatter.test %>% 
                mutate(across(`Spike Count`:`var_peakToPeak_pV`, 
                              ~ .x / scatter.test[[paste0(cur_column(), ".x")]] * 100)) %>% 
                select(-(`Spike Count.x`:`var_peakToPeak_pV.x`))
        } else {
            scatter.test <- scatter.test %>% 
                mutate(across(`Spike Count`:`Mean Network Interburst Interval [µs]`, 
                              ~ .x / scatter.test[[paste0(cur_column(), ".x")]] * 100)) %>% 
                select(-(`Spike Count.x`:`Mean Network Interburst Interval [µs].x`))
        }
    
        
        
        scatter.test <- scatter.test %>% 
            dplyr::filter(`Dose Label` == scatterChannel$doseLabelSelect()) %>% 
            dplyr::filter(`Compound ID` %in% scatterChannel$sampleOrder()) %>% 
            mutate(`Compound ID` := factor(`Compound ID`, levels = scatterChannel$sampleOrder()))
        
    })
    
    ##############################################################################
    # Makes the normalised jitter plot to the control wells 
    # 
    output$scatter_box_rel <- renderPlotly({
        ggplotly(
            norm.uber.table() %>% 
                ggplot(aes(y = !!rlang::sym(scatterCols()), 
                           x = `Compound ID`,
                           col = `Compound ID`,
                           label = `Channel ID`,
                           label2 = `Channel Label`,
                           label3 = `Well ID`,
                           label4 = plate)) +
                geom_hline(yintercept = 100, alpha=0.5,linetype=2) +
                geom_jitter(position=position_jitter(width=0.3, height=0.2),size=input$point.size, alpha=0.9) +
                geom_boxplot(alpha = 0.5, show.legend = FALSE,col="black",width=input$box.width,lwd=0.8) +
                theme_classic() +
                labs(y=paste0((scatterCols()),"\n% Relative to Baseline\n")) +
                theme(axis.text = element_text(size = input$text.size,face = "bold"),
                      axis.title = element_text(size = input$text.size*1.3,face = "bold"),
                      axis.title.x = element_blank(),
                      legend.position = "none") +
                coord_cartesian(ylim=c(0,input$ylim.rel))
        )
    })
    
    ##############################################################################
    # This is the same plot as above however in a beeswarm.
    output$swarm_box_rel <- renderPlotly({
        ggplotly(
            norm.uber.table() %>% 
                ggplot(aes(y = !!rlang::sym(scatterCols()), 
                           x = `Compound ID`,
                           col = `Compound ID`,
                           label = `Channel ID`,
                           label2 = `Channel Label`,
                           label3 = `Well ID`,
                           label4 = plate)) +
                geom_hline(yintercept = 100, alpha=0.5,linetype=2) +
                geom_beeswarm(priority = c("ascending"),size=input$point.size, alpha=0.5,cex = 0.8, groupOnX = TRUE) +
                geom_boxplot(alpha = 0.6, show.legend = FALSE,col="black",fill=NA,width=input$box.width,lwd=0.8) +
                theme_classic() +
                labs(y=paste0((scatterCols()),"\n% Relative to Baseline\n")) +
                theme(axis.text = element_text(size = input$text.size,face = "bold"),
                      axis.title = element_text(size = input$text.size*1.3,face = "bold"),
                      axis.title.x = element_blank(),
                      legend.position = "none") +
                coord_cartesian(ylim=c(0,input$ylim.rel))
        )
        
    })
    
    ##############################################################################
    # Create a normalised table for export and display in the app
    norm.table.out <- reactive({
        f <- norm.uber.table() %>% 
            select(`Channel ID`:`Dose Label`, !!rlang::sym(scatterCols())) %>% 
            rename(`Normalised Value` = !!rlang::sym(scatterCols()))
    })
    
    ##############################################################################
    # Export the normalised table for visualisation
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "_", !!rlang::sym(scatterCols()), "_normalised", ".csv")
        },
        content = function(file) {
            write.csv(norm.table.out(), file, row.names = FALSE)
        }
    )
    
    ##############################################################################
    # stats calculations for output and plotting by the app. 
    # modified again from the original code, we are taking selected columns that
    # have the same name within the normalised uber table. 
    output$stats <- renderPlot({
        dat <- norm.uber.table()
        
        colnames(dat) <- gsub(" ", "_", colnames(dat))
        
        stat.test <- t_test(data = dat,
                            formula = as.formula(paste(str_replace((scatterCols()), " ", "_"), "~", "Compound_ID")), 
                            ref.group = scatterChannel$sampleOrder()[1])
        
        stat.test <- stat.test %>% 
            add_xy_position(x = "Compound_ID") %>% 
            mutate(y.position = y.position * input$stat.y.adj)
        
        
        ggplot(dat, aes_string(y = str_replace((scatterCols()), " ", "_"), x = "Compound_ID",
                               col= "Compound_ID")) +
            geom_hline(yintercept = 100, alpha=0.5,linetype=2) +
            geom_beeswarm(priority = c("ascending"),size=input$point.size, alpha=0.5,cex = 0.8) +
            geom_boxplot(alpha = 0.6, show.legend = FALSE,col="black",fill=NA,width=input$box.width,lwd=0.8) +
            theme_classic() +
            labs(y=paste0((scatterCols()),"\n% Relative to Baseline\n")) +
            theme(
                axis.text = element_text(size = input$text.size,face = "bold"),
                axis.title = element_text(size = input$text.size*1.3,face = "bold"),
                axis.title.x = element_blank(),
                legend.position = "none") +
            coord_cartesian(ylim=c(0,max(stat.test$y.position))) +
            stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01*input$stat.y.adj)
        
    }) 
    
    ##############################################################################
    output$swarm_box_rel.stat <- renderPlot({
        
        Funct_swarm_box_rel.stat(uber.table, uber.table() %>% 
                                     filter(`Dose Label` == "Control"))
        
    },height = function(){input$stat.plot.h})
    
    
    ############################################################################## 
    output$down.swarm_box_rel.stat <- downloadHandler(
        filename = function() {
            paste0("MEA_statplot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            pdf(file = file,width = 10,height = 6)
            print(Funct_swarm_box_rel.stat(info(),info.base()))
            dev.off()
        }
    )
    
    ############################################################################## 
    output$table_stats <- renderTable({
        dat <- norm.uber.table() 
        colnames(dat) <- gsub(" ", "_", colnames(dat))
        
        stat.columns <- c("group1",	"n1", "estimate1", "group2", "n2", 
                          "estimate2", "estimate", "conf.low", "conf.high","p",	
                          "p.adj","p.adj.signif")
        
        tab.res <-if(input$stats.compare.all==TRUE) {
            t_test(data = dat,
                   formula = as.formula(paste(str_replace((scatterCols()), " ", "_"), "~",
                                                         "Compound_ID")),
                   conf.level = 0.95,detailed = T)[,stat.columns]
        } else { 
            t_test(data = dat,formula = as.formula(paste(str_replace((scatterCols()), " ", "_"), "~",
                                                         "Compound_ID")), 
                   ref.group = scatterChannel$sampleOrder()[1],conf.level = 0.95,detailed = T)[,stat.columns]
        }
        colnames(tab.res) <- c("group1",	"n1", "Mean1", "group2",	"n2",	"Mean2",
                               "Diff_Means_(DM)", "95%_CI_Low_DM", "95%_CI_high_DM","p",	"p.adj","p.adj.signif")
        
        if(input$sort.by.pval==TRUE) {
            tab.res[order(tab.res$p,decreasing = F),]
        } else { 
            tab.res[order(tab.res$group2,decreasing = F),]
        }
        
    }, digits = 4
    )
    
    ##############################################################################
    # PCA Plotting of tables generated
    pca_dat_server(id = "uberTable", dat = scat.table(), spikeIn = input$includeSpike)
    
    ##############################################################################
    # Create a normalised table of measurements from the uber table for the 
    # epileptiform plot, to contain all entries except for the control then plot
    # onto a PCA to determine where the samples are sitting. 
    # 
    epi.table <- reactive({
        scatter.test <- scat.table()
        
        ctrl <- scatter.test %>% 
            filter(`Dose Label` == "Control")
        
        scatter.test <- left_join(ctrl, scatter.test, by = "key")
        
        colnames(scatter.test) <- gsub("\\.y", "", colnames(scatter.test))
        
        # need to include a test here for spike data input$includeSpike
        if (input$includeSpike == TRUE) {
            scatter.test <- scatter.test %>% 
                mutate(across(`Spike Count`:`var_peakToPeak_pV`, 
                              ~ .x / scatter.test[[paste0(cur_column(), ".x")]] * 100)) %>% 
                select(-(`Spike Count.x`:`var_peakToPeak_pV.x`))
        } else {
            scatter.test <- scatter.test %>% 
                mutate(across(`Spike Count`:`Mean Network Interburst Interval [µs]`, 
                              ~ .x / scatter.test[[paste0(cur_column(), ".x")]] * 100)) %>% 
                select(-(`Spike Count.x`:`Mean Network Interburst Interval [µs].x`))
        }
        
        
        scatter.test <- scatter.test %>% 
            filter(`Dose Label` != "Control")
        
    }) %>% 
        bindCache(scat.table())
    
    pca_dat_server(id = "epiTable", dat = epi.table(), spikeIn = input$includeSpike)
    
    ##############################################################################
    # Make a summarised table from the scatter.test which is by channel aggregate
    # by well and plate here to calculate the mean and variance of each well 
    # within the experiment then this will feed into new scatter plots where only
    # datapoints by well are presented
    
    well.scat <- reactive({
        dat <- scat.table()
        
        #need to include a test here for spike data
        if (input$includeSpike == TRUE) {
            dat <- dat %>% 
                group_by(key3,`Dose Label`, `Compound ID`, plate) %>% 
                summarise(across(`Spike Count`:var_peakToPeak_pV, list(mean = mean))) %>% 
                tibble()
            colnames(dat) <- gsub("_mean", "", colnames(dat))
            return(dat)
        } else {
            dat <- dat %>% 
                group_by(key3,`Dose Label`, `Compound ID`, plate) %>% 
                summarise(across(`Spike Count`:`Mean Network Interburst Interval [µs]`, list(mean = mean))) %>% 
                tibble()
            colnames(dat) <- gsub("_mean", "", colnames(dat))
            return(dat)
        }
        
    }) %>% 
        bindCache(scat.table())
    
    pca_dat_server(id = "wellTable", dat = well.scat(), spikeIn = input$includeSpike)
    
    ##############################################################################
    # With the uber dataset, now create the data structure for plotting of the 
    # scatter and PCA's after filtering
    well.uber <- reactive({
        scatter.test <- well.scat() %>% 
            filter(`Dose Label` != "Control")
        
    })
    
    pca_dat_server(id = "epiWellTable", dat = well.uber(), spikeIn = input$includeSpike)
    
    
    ##############################################################################
    # Plot with the scatter module and give it a go
    
    wellGroups <- extractScatter_server(id = "wellChScatter", dat = well.scat())
    wellCols <- extractMeasurementColumns_server(id = "wellcolsScatter", dat = well.scat())
    
    scatter_plot_server(id = "wellTable", dataIn = well.scat(), groupIn = wellGroups(),
                        colsIn = wellcols(), makePlot = input$WplotScatter)
    
    ##############################################################################
    # Create a normalised table of measurements from the uber well data that has been
    # selected and filtered for the variables of interest. Calculate the normalised
    # values for all measures and columns of interest for the scatter plot
    # 
    
    n.well.table <- reactive( {
        scatter.test <- well.scat()
        ## Create a key containing plate and well ID for merging with control and norm
        scatter.test <- scatter.test %>% 
            mutate(key4 = key3) %>% 
            separate(key4, into=(c("a", "b", "c")), sep = "_") %>% 
            mutate(key = paste0(a, "_", b))
        
        ctrl <- scatter.test %>% 
            filter(`Dose Label` == "Control")
        
        scatter.test <- left_join(ctrl, scatter.test, by = "key")
        
        colnames(scatter.test) <- gsub("\\.y", "", colnames(scatter.test))
        
        # need to include test here for spike data input$includeSpike
        if (input$includeSpike == TRUE) {
            scatter.test <- scatter.test %>% 
                mutate(across(`Spike Count`:`var_peakToPeak_pV`, 
                              ~ .x / scatter.test[[paste0(cur_column(), ".x")]] * 100)) %>% 
                select(-(`Spike Count.x`:`var_peakToPeak_pV.x`))
        } else {
            scatter.test <- scatter.test %>% 
                mutate(across(`Spike Count`:`Mean Network Interburst Interval [µs]`, 
                              ~ .x / scatter.test[[paste0(cur_column(), ".x")]] * 100)) %>% 
                select(-(`Spike Count.x`:`Mean Network Interburst Interval [µs].x`))
        }
        
    }) 
    
    ##############################################################################
    # Plot with the scatter module and give it a go
    
    wellGroupsN <- extractScatter_server(id = "wellChScatterNorm", dat = n.well.table())
    wellColsN <- extractMeasurementColumns_server(id = "wellcolsScatterNorm", dat = n.well.table())
    
    scatter_plot_server(id = "wellTableNorm", dataIn = n.well.table(), groupIn = wellGroupsN(),
                        colsIn = wellColsN(), makePlot = input$WplotScatter2)

    #cat(file=stderr(), "This is the object emitted from dat server", class(pca.uber$rotated()), "\n")
}

# Run the application 
shinyApp(ui = ui, server = server)
