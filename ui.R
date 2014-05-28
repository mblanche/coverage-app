library(shiny)

## ui.R
shinyUI(fluidPage(
    titlePanel("coverageVis"),
    sidebarLayout(
        
        sidebarPanel(
            helpText(h5("Display coverages plot for your favorite gene")),

            uiOutput("experimentSelector"),
            uiOutput("samplesSelector"),
            uiOutput("controlSelector"),

            selectizeInput('geneID',label="Type or select your genes:",c("Genes"="")),

            uiOutput("txSelector")
            ## uiOutput("geneInput"),
            
        ),
        
        mainPanel(
            plotOutput('plot'),
            textOutput('text1')
            ## textOutput('text2')
            )
        )
    ))

