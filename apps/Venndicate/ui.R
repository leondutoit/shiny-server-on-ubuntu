
# This is the user-interface definition of the Shiny web application.
# need to set bg to #314050

require(shiny)          # Main Shiny Package
require(shinythings)    # Better Action Buttons
require(shinyIncubator) # Progress Bar

shinyUI(fluidPage(theme = "stylesheet.css",
  
  ##### Application title #####
  div(h1("Venndicate"), style="width: 100%"),
  br(),
  #### Tabbed Display Panel #####
  tabsetPanel(id='tab',
              ##### Filter & Aggregate #####
              tabPanel("Filter & Aggregate Data",
                       fluidRow(
                         div( wellPanel( div(
                           fileInput('data', label=h4('Upload Data File'), style='primary'),
                           hr(),
                           h4('Filter Data'),
                           textInput('filterText', label="", value="Text to filter"),
                           fluidRow(
                             column(6, actionButton('remove', label='Remove', icon='minus', style='danger', block=T)),
                             column(6, actionButton('add', label='Add', icon='plus', style='success', block=T))
                           ),
                           hr(),
                           selectizeInput('tagSelect', label="Subset by Tag", 
                                          choices=c("All"), selected="All"),
                           hr(),
                           actionButton('dataReset', label="Reset Data", block=TRUE, icon='undo', style='primary'),
                           hr(),
                           fluidRow(
                             column(6, downloadButton('links', label='Links', class='btn-block btn-link')),
                             column(6, downloadButton('users', label='Users', class='btn-block btn-link'))
                           )),
                         style="height: 600px;"),
                         class="span4",
                         style="max-width: 280px;"
                         ),
                         column(8, fluidRow(
                                       column(6, plotOutput('hist', height='500px')),
                                       column(6, fluidRow(
                                         column(6, h4('Tag Summary'),
                                               tableOutput('tagList')),
                                         column(6, h4('Type Summary'),
                                                tableOutput('typeList'))
                                         ),
                                         fluidRow(
                                           column(6, h4('Agg Summary'),
                                                  tableOutput('aggStat')),
                                           column(6, h4('Rank Summary'),
                                                  tableOutput('rankStat'))
                                           ))
                                     )
                                )
                       )
              ),
              ##### Prepare & Explore #####
              tabPanel("Prepare & Explore Text",
                       fluidRow(
                         div( wellPanel( div(
                           h4('Process Text Parameters'),
                           selectizeInput('typeSelect', label="Data Type to Process", 
                                          choices=NULL, selected=NULL, width='220px'),
                           p('Sparse Value', style="margin-bottom: 0px;"),
                           fluidRow(
                             column(2, div(actionButton('sparseMinus', label='', icon='minus', style='link'), style="margin-top: 13px;")),
                             column(8, sliderInput('sparse', label='', min=0, max=1, step=0.01, value=0.9)),
                             column(2, div(actionButton('sparsePlus', label='', icon='plus', style='link'), style="margin-top: 13px;"))
                           ),
                           br(),
                           numericInput('sample', label='Sample Size', value=1000),
                           p('Token Size', style="margin-bottom: 0px;"),
                           fluidRow(
                             column(2, div(actionButton('nMinus', label='', icon='minus', style='link'), style="margin-top: 13px;")),
                             column(8, sliderInput('n', label='', min=0, max=5, step=1, value=3)),
                             column(2, div(actionButton('nPlus', label='', icon='plus', style='link'), style="margin-top: 13px;"))
                           ),
                           br(),
                           tags$label(`for`="addList", "Terms to Add to Stopwords"),
                           tags$textarea(id="addList", rows=2, cols=40, "Default value"),
                           fluidRow(
                             column(6, actionButton('process', label='Update', style='primary', block=TRUE, icon="refresh")),
                             column(6, actionButton('processReset', label='Reset', style='primary', block=TRUE, icon='undo'))
                           ),
                           hr(),
                           div(
                             downloadButton('getHeatmap', label="Heatmap", class="btn-link"),
                             downloadButton('getNetwork', label="Term Network", class="btn-link"),
                             downloadButton('getWordCloud', label="Word Cloud", class="btn-link"),
                             style="padding-left: 20px;"
                           ),
                         style="min-height: 600px;")),
                         class="span4",
                         style="max-width: 280px;"
                         ),
                         div(
                           fluidRow(
                            column(8,  plotOutput('heatmap', height='648px') ),
                            column(4,  plotOutput('termNetwork', height='324px'),
                                       plotOutput('wordCloud', height='324px') )
                           ),
                           class="span8", style="height: 648px; background-color: #314050")
                       )
              ),
              ##### Cluster & Plot #####
              tabPanel("Cluster & Plot",
                       fluidRow(
                         div( wellPanel(div(
                           h4('Clustering Parameters'),
                           selectizeInput('algo', label='Algorithm', 
                                          choices=list("k.med"=1, "pamkCBI"= 2, "kmeansCBI"=3),
                                          selected=1, width='220px'),
                           p('Number of Clusters'),
                           p('(Only for pamkCBI and kmeansCBI)'),
                           fluidRow(
                             column(2, div(actionButton('kMinus', label='', icon='minus', style='link'), style="margin-top: 13px;")),
                             column(8, sliderInput('k', label='', min=0, max=10, step=1, value=0)),
                             column(2, div(actionButton('kPlus', label='', icon='plus', style='link'), style="margin-top: 13px;"))
                           ),
                           hr(),
                           selectizeInput('clSelect', label="Remove Cluster", choices=NULL, selected="Choose a Cluster", width='220px'),
                           fluidRow(
                             column(6, actionButton('rmCluster', label="Remove", icon="minus", style="danger", block=TRUE)),
                             column(6, actionButton('resetPlot', label="Reset", icon="undo", style="primary", block=TRUE))
                           ),
                           hr(),
                           radioButtons('sizing', label="Cluster Sizing Metric", inline=TRUE,
                                        choices=c('Reach', 'Volume'), selected='Volume'),
                           hr(),
                           div(
                             downloadButton('getCluster', label='Cluster Plot', class="btn-link"),
                             downloadButton('summCluster', label='Summarize Clusters',class="btn-link"),
                             style="padding-left: 20px;"
                           ),
                         style="height: 600px;")),
                         class="span4",
                         style="max-width: 280px;"
                         ),
                         column(8, plotOutput('clustPlot', height='auto'))
                       )
              ),
              ##### Explore Features #####
              tabPanel("Explore Features",
                       fluidRow(
                         div( wellPanel( div(
                           fileInput('termScrape', label=h4('Upload Data File'), style='primary'),
                           hr(),
                           radioButtons('direction', label="Direction", choices=c("Positive", "Negative"), inline=TRUE),
                           p("Sparse"),
                           fluidRow(
                              column(2, div(actionButton('tsSparseMinus', label='', icon='minus', style='link'), style="margin-top: 13px;")),
                              column(8, sliderInput('tsSparse', label="", min=0, max=0.99, step=0.01, value=0.9)),
                              column(2, div(actionButton('tsSparsePlus', label='', icon='plus', style='link'), style="margin-top: 13px;"))
                           ),
                           p("Number to Return"),
                           fluidRow(
                             column(2, div(actionButton('tsNMinus', label='', icon='minus', style='link'), style="margin-top: 13px;")),
                             column(8, sliderInput('tsN', label="", min=1, max=30, step=1, value=15)),
                             column(2, div(actionButton('tsNPlus', label='', icon='plus', style='link'), style="margin-top: 13px;"))
                           ),
                           style="height: 600px;")),
                         class="span4",
                         style="max-width: 280px"
                         ),
                         column(8, plotOutput('termScrapePlot', height='648px'))
                       )
              )
              
  ),
  div( class="container", style="height: 100px"),
  
  ##### Header Tags #####
  tags$head(
    tags$style('
      .well{
        max-width: 219px;
      }
      input[type="file"]{
        width: 100%;
      }
      .shiny-progress {
        border-radius: 0px !important;
        font-family: "Open Sans", Calibri, Candara, Arial, sans-serif;
      }
      
      ')
  ),
  
  progressInit()
  
))
