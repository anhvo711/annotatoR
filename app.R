library(shiny)
library(DT)
library(biomaRt)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(openxlsx)

#CSS for display attributes in columns
tweaks <- 
  list(tags$div(tags$style(HTML("
                                 .multicol { 
                                      -webkit-column-width: 300px;
                                      -moz-column-width: 300px;
                                      column-width: 300px;
                                 }"))),
       tags$style(HTML("
                                @import url('https://fonts.googleapis.com/css?family=Cairo:700&display=swap');
                                .navbar-brand {
                                  font-family: 'Cairo', sans-serif;
                                  font-weight: 700;
                                  line-height: 1;
                                  color: #48ca3b;
                                }"))
       
  )

# Define UI for application
ui <- navbarPage(theme = shinythemes::shinytheme("flatly"),
                 title = "annotatoR",
                 
                 #Main tab
                 tabPanel("Data Upload", shinyjs::useShinyjs(), tags$head(tags$style(".shiny-notification {position: fixed; top: 40% ;left: 30%; width:40vw; height:9vh; font-size:3vh}")),
                          wellPanel(style="background-color: #ffb42d; color:black",
                                    tags$h4(tags$u(tags$strong("File Upload"))),
                                    fileInput("rawFile", "Choose a File", FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                    radioButtons("sep", "Choose the Separator used by your File", choices = c(Comma = ",",Semicolon = ";",Tab = "\t"), selected = "\t"),
                                    tags$hr(),
                                    tags$strong("Preview uploaded data"),
                                    tags$br(),tags$br(),
                                    dataTableOutput("filePreview")
                          ),
                          wellPanel(style="background-color: #ffb42d; color:black",
                                    tags$h4(tags$u(tags$strong("Search Properties"))),
                                    
                                    #Each list's choices depends on the input prior to it
                                    selectInput("dataset", "Choose a Dataset", choices = c("",listMarts()$biomart), selected = NULL),
                                    selectInput('speciesList', 'Choose a Species', choices = NULL, selected = NULL),
                                    selectInput('filtersList', 'Choose the type of Identifier', choices = NULL, selected = NULL),
                                    selectInput('columnList', 'Choose the Column containing the Identifier', choices = NULL, selected = NULL),
                                    tags$strong("Choose the Annotations you would like to retrieve"),
                                    bsCollapse(id = "Attributes",
                                               bsCollapsePanel("Features", tweaks, 
                                                               list(
                                                                 tags$div(
                                                                   align = 'left', class = 'multicol', 
                                                                   checkboxGroupInput(inputId  = 'featuresGroup', 
                                                                                      label    = NULL, 
                                                                                      choices  = NULL,
                                                                                      selected = NULL,
                                                                                      inline   = FALSE) 
                                                                 )
                                                               ) 
                                               ),
                                               bsCollapsePanel("Structures", tweaks,
                                                               list(
                                                                 tags$div(
                                                                   align = 'left', class = 'multicol', 
                                                                   checkboxGroupInput(inputId  = 'structuresGroup', 
                                                                                      label    = NULL, 
                                                                                      choices  = NULL,
                                                                                      selected = NULL,
                                                                                      inline   = FALSE)
                                                                 )
                                                               ) 
                                               ),
                                               bsCollapsePanel("Homologues",tweaks,
                                                               list(
                                                                 tags$div(
                                                                   align = 'left', class = 'multicol', 
                                                                   checkboxGroupInput(inputId  = 'homologuesGroup', 
                                                                                      label    = NULL, 
                                                                                      choices  = NULL,
                                                                                      selected = NULL,
                                                                                      inline   = FALSE)
                                                                 )
                                                               )
                                               )
                                    ),
                                    tags$br(),
                                    actionButton("annotateButton","Annotate My File", width = "200px", style = "background-color: #2c3e50; color:white"),
                                    tags$br(),tags$br(),
                                    conditionalPanel(condition = "output.annotateButtonClicked",
                                                     tags$hr(),
                                                     tags$h4(tags$u(tags$strong("File Download"))),
                                                     tags$span("Remember to click the Annotate button before downloading your files!"),
                                                     tags$br(), tags$br(),
                                                     radioButtons("resolveMatches", "If there are multiple matches for each identifier, what would you like to do?", choices = c("Keep only first match, discard the rest","Keep all matches"), selected = "Keep all matches"),
                                                     splitLayout(
                                                       div(
                                                         radioButtons("downloadModifiedExtension", "What do you want to download your file as?", choices = c("csv (Comma separated)" = "csv", "excel" = "xlsx", "txt (Tab separated)" = "txt"), selected = "csv"),
                                                         downloadButton("downloadModified","Download Original File with Annotations", width = "250px", style = "background-color: #2c3e50; color:white")
                                                       ),
                                                       div(
                                                         radioButtons("downloadAnnotationsExtension", "What do you want to download your file as?", choices = c("csv (Comma separated)" = "csv", "excel" = "xlsx", "txt (Tab separated)" = "txt"), selected = "csv"),
                                                         downloadButton("downloadAnnotations","Download Only Annotations", width = "250px", style = "background-color: #2c3e50; color:white")
                                                       )
                                                     )
                                    )
                          )
                 )
)

# Define server logic
server <- function(input, output,session) {
  
  #Global variables for server
  values = reactiveValues(
    ensembl = NULL,
    datasets = NULL,
    filters = NULL,
    attributes = NULL,
    species = NULL,
    selectedFilter = NULL,
    data = NULL,
    annotationSymbols = NULL,
    annotatedData = NULL,
    ids = NULL,
    annotationSymbolsFinal = NULL
  )

  #Reads data from file and Displays data in table form for preview ----------------------------------------
  output$filePreview <- DT::renderDataTable({
    req(input$rawFile)
    values$data = read.csv(input$rawFile$datapath, sep = input$sep, row.names = NULL)
  })
  
  #Once file or separator is changed, show list of column headers ---------------------------------------
  observeEvent( {
    input$rawFile
    input$sep
  }, {
    req(input$rawFile)
    updateSelectInput(session, 'columnList', choices = names(values$data))
  })
  
  #Shows species according to selected dataset ---------------------------------------------------------
  getSpeciesList = reactive({
    req(input$dataset)
    mirrors = c("useast.ensembl.org","uswest.ensembl.org","asia.ensembl.org","apr2019.archive.ensembl.org", "ensembl.org","jan2019.archive.ensembl.org", "oct2018.archive.ensembl.org")
    print(paste("Chosen Dataset:",input$dataset))
    
    #try different hosts in case some are down
    for (m in mirrors){
      tryRet = tryCatch( 
        {
          values$ensembl = useMart(input$dataset, host = m)
          print(paste("Using host:",m))
        }, error =  function(e) {
          return(NULL)
        }
      )
      if (is.null(tryRet)) {
        next
      } else {
        break
      }
    }
    
    tryCatch(
      {
        values$datasets = listDatasets(values$ensembl)
      }, error = function(e) {
        showModal(modalDialog("No database is currently available. Please try again."))
      },warning = function(e) {
        showModal(modalDialog("No database is currently available. Please try again."))
      }
    )
  })
  
  observeEvent(input$dataset,{
    req(input$dataset)
    
    #if dataset is ENSEMBL_MART_ENSEMBL, show common species at the top of list
    if (input$dataset == "ENSEMBL_MART_ENSEMBL") {
      updateSelectInput(session, 'speciesList', choices = list(
        "Common Species" = c("","hsapiens_gene_ensembl","mmusculus_gene_ensembl","rnorvegicus_gene_ensembl","cfamiliaris_gene_ensembl","sscrofa_gene_ensembl","dmelanogaster_gene_ensembl"),
        "Species" = isolate(getSpeciesList())
      ), selected = NULL)
    } else {
      updateSelectInput(session, 'speciesList', choices = isolate(getSpeciesList()), selected = NULL)
    }
  })
  
  #Shows filters according to selected species --------------------------------------------------------
  getFiltersList = reactive({
    req(input$speciesList)
    print(paste("Chosen Species:",input$speciesList))
    tryCatch(
      {
        values$ensembl = useDataset(input$speciesList,mart=values$ensembl)
        values$filters = listFilters(values$ensembl)
      }, error = function(e) {
        showModal(modalDialog("No database is currently available. Please try again."))
      },warning = function(e) {
        showModal(modalDialog("No database is currently available. Please try again."))
      }
    )
  })
  
  observeEvent(input$speciesList, {
    req(input$speciesList)
    updateSelectInput(session, "filtersList", choices = c("",isolate(getFiltersList()$name)))
    
    #populates homologue attributes without showing selected species in list
    values$species = regmatches(input$speciesList, regexpr("^([a-z])*", input$speciesList))
    sList = list(mouse="mmusculus",human="hsapiens",rat="rnorvegicus",dog="cfamiliaris",pig="sscrofa",fly="dmelanogaster")
    
    #find homologs attributes according to selected species
    homologuesVector = NULL
    for (elem in sList) {
      if (values$species != elem) {
        homologuesVector = c(homologuesVector,searchAttributes(mart = values$ensembl, pattern = paste0(elem,"_homolog")))
      }
    }
    
    updateCollapse(session, "Attributes", close = "Homologues", style = NULL)
    updateCollapse(session, "Attributes", close = "Features", style = NULL)
    updateCollapse(session, "Attributes", close = "Structures", style = NULL)
    updateCheckboxGroupInput(session, "homologuesGroup", choiceNames = homologuesVector$description, choiceValues = homologuesVector$name)
  })
  
  #Shows available attributes (not homologues) based on selected filter -------------------------------------------------
  observeEvent(input$filtersList, {
    req(input$filtersList)
    print(paste("Chosen Filter: ",input$filtersList))
    tryCatch(
      {
        pages = attributePages(values$ensembl)
      }, error = function(e) {
        showModal(modalDialog("No database is currently available. Please try again."))
      },warning = function(e) {
        showModal(modalDialog("No database is currently available. Please try again."))
      }
    )
    featuresVector = NULL
    structuresVector = NULL
    otherVector = NULL
    sequencesVector = NULL
    for(p in pages){
      tempList = unique(listAttributes(values$ensembl, page = p))
      if (p == "feature_page"){
        featuresVector = c(featuresVector,tempList)
      } else if (p == "structure"){
        structuresVector = c(structuresVector,tempList)
      } else if (p == "sequences"){
        sequencesVector = c(sequencesVector,tempList)
      } else {
        otherVector = c(otherVector,tempList)
      }
    }
    
    #finding and preselecting the filter as an attribute
    tryCatch(
      {
        values$selectedFilter = searchAttributes(mart = values$ensembl, pattern = input$filtersList)[1,]
      }, error = function(e) {
        showModal(modalDialog("No database is currently available. Please try again."))
      },warning = function(e) {
        showModal(modalDialog("No database is currently available. Please try again."))
      }
    )
    updateCollapse(session, "Attributes", close = "Features", style = NULL)
    updateCheckboxGroupInput(session, "featuresGroup", choiceNames = featuresVector$description, choiceValues = featuresVector$name, selected = values$selectedFilter$name)
    updateCollapse(session, "Attributes", close = "Structures", style = NULL)
    updateCheckboxGroupInput(session, "structuresGroup", choiceNames = structuresVector$description, choiceValues = structuresVector$name, selected = values$selectedFilter$name)
  })
  
  #disable preselected attributes -------------------------------------------------------------------------------
  observeEvent(input$featuresGroup, {
    shinyjs::disable(selector = paste0("#featuresGroup input[value=\'",values$selectedFilter$name,"\']"))
    shinyjs::disable(selector = paste0("#structuresGroup input[value=\'",values$selectedFilter$name,"\']"))
  })
  
  #Show the download section when annotate button is clicked
  output$annotateButtonClicked <- reactive({
    input$annotateButton
    return(input$annotateButton > 0)
  })
  outputOptions(output, "annotateButtonClicked", suspendWhenHidden=FALSE)
  
  #Doing the query when annotate button is clicked --------------------------------------------------------------
  observeEvent(input$annotateButton, {
    #remove version numbers for identifier column
    y = c(as.character(values$data[,input$columnList]))
    values$ids = c(sapply(strsplit(y, ".", fixed = TRUE), "[", 1)) 
    
    #show progress bar for user feedback
    withProgress(message = 'Querying Database...Please Wait', value = 20, {
      values$annotationSymbols = NULL
      values$annotationSymbolsFinal = NULL
      attributes = unique(c(input$homologuesGroup,input$featuresGroup,input$structuresGroup))
      if (length(attributes) > 20){
        showModal(modalDialog("You have selected too many attributes to annotate. Please select 20 or fewer."))
      }
      
      tryCatch(
        {
          values$annotationSymbols = getBM(attributes = attributes, filters = input$filtersList, values = values$ids, mart = values$ensembl, uniqueRows = TRUE)
        }, error = function(e) {
          if (length(attributes) > 20){
            showModal(modalDialog("You have selected too many annotations. Select 20 or fewer."))
          } else {
            showModal(modalDialog("Your query could not be processed. Please try again."))
          }
        },warning = function(e) {
          if (length(attributes) > 20){
            showModal(modalDialog("You have selected too many annotations. Select 20 or fewer."))
          } else {
            showModal(modalDialog("Your query could not be processed. Please try again."))
            
          }        
        }
      )
    })
  })

  #agrregates columns if all matches are kept
  aggre = function() {
    if (input$resolveMatches == "Keep only first match, discard the rest"){
      values$annotationSymbolsFinal = values$annotationSymbols[!duplicated(values$annotationSymbols[[values$selectedFilter$name]]), ]
    } else if (input$resolveMatches == "Keep all matches"){
      values$annotationSymbolsFinal = aggregate(by = list(values$annotationSymbols[[values$selectedFilter$name]]), x = values$annotationSymbols, FUN = function(x) paste(unique(x),collapse=", ")) #collapse rows with same name
      values$annotationSymbolsFinal[[values$selectedFilter$name]] = NULL #remove aggregated identifiers column 
      colnames(values$annotationSymbolsFinal)[1] = values$selectedFilter$name #rename identifiers column
    } 
  }
  
  #sets download handler to give the user the annotated file --------------------------------------------------------
  output$downloadModified = downloadHandler(
    filename = function() {
      fileName = regmatches(input$rawFile$name, regexpr("([A-Z|a-z|-]*)",input$rawFile$name))
      paste0(fileName, "_annotated.", input$downloadModifiedExtension)
    },
    content = function(file) {
      values$data[[input$columnList]] = values$ids #replace original data with non-version identifiers
      #replace the identifier column with the name of the filter for merge to work
      colnames(values$data)[colnames(values$data)==input$columnList] <- values$selectedFilter$name 
      
      tryCatch({
        aggre()
      }, error = function(e) {
        showModal(modalDialog("Your query could not be processed. Please ensure your inputs are correct."))
      }, warning = function(e) {
        showModal(modalDialog("Your query could not be processed. Please ensure your inputs are correct."))
      })
      
      values$annotatedData = NULL
      values$annotatedData = merge(values$data, values$annotationSymbolsFinal, all.x = TRUE)
      print(values$annotatedData)
      if (input$downloadModifiedExtension == "csv") {
        write.csv(values$annotatedData, file, row.names = FALSE, quote = TRUE)
      } else if (input$downloadModifiedExtension == "txt") {
        write.table((values$annotatedData), file, sep = "\t", row.names = FALSE, quote = TRUE)
      } else {
        write.xlsx(values$annotatedData, file, row.names = FALSE, quote = TRUE)
      }
    }
  )
  
  #sets download handler to give the user the file with only annotations ------------------------------------------------
  output$downloadAnnotations = downloadHandler(
    filename = function() {
      paste0("Annotations.", input$downloadAnnotationsExtension)
    },
    content = function(file) {
      tryCatch({
        aggre()
      }, error = function(e) {
        showModal(modalDialog("Your query could not be processed. Please ensure your inputs are correct."))
      }, warning = function(e) {
        showModal(modalDialog("Your query could not be processed. Please ensure your inputs are correct."))
      })
      if (input$downloadAnnotationsExtension == "csv") {
        write.csv(values$annotationSymbolsFinal, file, row.names = FALSE, quote = TRUE)
      } else if (input$downloadAnnotationsExtension == "txt") {
        write.table((values$annotationSymbolsFinal), file, sep = "\t", row.names = FALSE, quote = TRUE)
      } else {
        write.xlsx(values$annotationSymbolsFinal, file, row.names = FALSE, quote = TRUE)
      }    
      }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
