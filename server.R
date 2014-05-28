library(shiny)

source("helpers.R")

shinyServer(function(input, output, session) {

    data <- reactive({
        if(!is.null(input$exps)){
            data <- readRDS(input$exps)
            return(data)
        }
    })
    
    samples <- reactive({input$samples})
    cntl <- reactive({input$cntl})
    gene <- reactive({input$geneID})
    tx <- reactive({ input$txID})
    
    ## Render experiments selector
    output$experimentSelector <- renderUI({
        selectInput("exps", "Choose Experiments",exps )
        })

    ## Render samples selector
    output$samplesSelector <- renderUI({
        ## do a case selection...
        if(!is.null(data())){
            samples <-names(data())
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

    ## Provide an autocomplete selectable gene list to the user
    updateSelectizeInput(session, 'geneID', choices = geneNames, server = TRUE)

    ## Render Transcript selector
    output$txSelector <- renderUI({
        if (!gene() == ''){
            txs <- getTx(gene())
            radioButtons("txID",label="Select a transcript:",choice=txs,selected=txs[1])
        }
    })

    ## Render the plot if everything is fine!
    output$plot <- renderPlot({
        if (!is.null(tx())){
            print(plotCovs(data(),tx(),samples(),cntl() ))
        }
    })

})
