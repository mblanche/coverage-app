library(shinyIncubator)
library(shiny)

## ui.R
shinyUI(fluidPage(
    progressInit(),
    titlePanel("coverageVis"),
    sidebarLayout(
        
        sidebarPanel(
            helpText(h5("Display coverages plot for your favorite gene")),

            uiOutput("samplesSelector"),
            uiOutput("controlSelector"),
            uiOutput("geneSelector"),
            uiOutput("txSelector")
                        
        ),
        
        mainPanel(
            plotOutput('plot'),
            uiOutput('button'),
            textOutput('text1')
            )
        )
    ))

