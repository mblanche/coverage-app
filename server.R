library(shinyIncubator)
library(shiny)

source("helpers.R")

shinyServer(function(input, output, session) {

    data <- withProgress(session, {
        setProgress(message = "Loading coverage data, please be patient",
                    detail = "This may take a few moments...")
        readRDS("data/DRB_A1_feat_covs.rds")
    })
    
    samples <- reactive({input$samples})
    cntl <- reactive({input$cntl})
    gene <- reactive({input$geneID})
    tx <- reactive({ input$txID})

    ## Render samples selectora
    output$samplesSelector <- renderUI({
        ## do a case selection...
        if(!is.null(data)){
            samples <-names(data)
            checkboxGroupInput("samples", "Choose samples to display",samples, selected=samples)
        }
    })
    
    ## Render control selector
    output$controlSelector <- renderUI({
        if(!is.null(samples())){
            samples <- samples()
            radioButtons("cntl",
                         "Choose the control (will appear at the bottom of the stack)",
                         samples,
                         selected=samples[length(samples)])
        }
    })
    
    observe({
        if(!is.null(samples())){
            ## Provide an autocomplete selectable gene list to the user
            output$geneSelector <- renderUI({
                selectizeInput('geneID',
                               label="Type or select your genes:",
                               choices=c("Genes"=""))
            })
            updateSelectizeInput(session, 'geneID', choices = geneNames, server = TRUE)
        }
    })


    ## Render Transcript selector
    observe({
        if (!is.null(gene())){
            if (gene() != ''){
                txs <- getTx(gene())
                if (!is.null(txs)) {
                    output$txSelector <- renderUI({
                        radioButtons("txID",label="Select a transcript:",choice=txs,selected=txs[1])
                    })
                } else {
                    output$text1 <- renderText({
                        paste("No transcript for",gene(),"were significantly detected")
                    })
                }
            }
        }
    })

    ## Render the plot if everything is fine!
    observe({
        if (!is.null(tx())){
            if (length(samples()) == 0){
                output$plot <- renderPlot({ return(NULL) })
                output$button <- renderUI({ return(NULL) })
            } else {
                withProgress(session, {
                    setProgress(message = paste("Rendering the coverages over",tx()),
                                detail = "This may take a few moments...")
                    p <- plotCovs(data,tx(),samples(),cntl())
                    if (is.null(p)){
                        output$plot <- renderPlot({ return(NULL) })
                        output$button <- renderUI({ return(NULL) })
                        output$text1 <- renderText({ paste("No coverage was found for",tx()) })
                    } else {
                        output$plot <- renderPlot({print(p)})
                        ## Add a button to download a pdf of the figure
                        output$button <- renderUI({
                            downloadButton('downloadData', 'Save as PDF')
                        })
                    }
                })
            }
        }
    })


    
    
    ## Function to download the pdf
    output$downloadData <- downloadHandler(
        
        ## This function returns a string which tells the client
        ## browser what name to use when saving the file.
        filename = function() { paste0(tx(),".pdf") },
        
        ## This function should write data to a file given to it by
        ## the argument 'file'.
        content = function(file) {
            pdf(file)
            print(plotCovs(data,tx(),samples(),cntl()))
            dev.off()
        }
        )



})
