library(shiny)
library(shinyjs)
library(sf)
library(tidyr)
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(icesTAF)
library(shinycssloaders)
library(tibble)
library(data.table)
library(shinythemes)

initComplete <- JS(
  "function(settings, json){",
  "  var table = this.api();",
  "  $('<button id=\"toggle-select-all\" class=\"btn\" style=\"background-color:#1b98e0; color: white; margin: 10px;\">Select All/Unselect All</button>').prependTo($(table.table().container()).find('.dataTables_filter'));",
  "  var allSelected = false;",
  "  $('#toggle-select-all').click(function() {",
  "    allSelected = !allSelected;",
  "    table.rows().every(function(rowIdx, a, b) {",
  "      if (allSelected) {",
  "        this.select();",
  "      } else {",
  "        this.deselect();",
  "      }",
  "    });",
  "    var selectedIDs = table.rows({selected: true}).indexes().toArray();",
  "    Shiny.setInputValue('Variables_rows_selected', selectedIDs);",
  "  });",
  "}"
)



# Load necessary data
ecoregions <- readRDS("Data/Ecoregions.RData")
url_github <- "https://raw.githubusercontent.com/ices-eg/WGINOR/refs/heads/main/TAF_ATAC/output/tables.Rdata"
load(url(url_github))

# Extract unique categories excluding NAs for UI creation
category_choices <- unique(info$category)
category_choices <- category_choices[!is.na(category_choices)]

ui <- fluidPage(
  tags$head(
    includeCSS("www/styles.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&display=swap"),
  ),
  useShinyjs(),
  withTags({
    div(
      class = "header",
      checked = NA,
      h1(
      ),
      h2("IEA Explorer")
    )
  }),
  navbarPage(
    "Menu",
    id = "menu",
    tabPanel("Ecoregions and Variables", sidebarLayout(
      sidebarPanel(
        img(src = "Map_Norwegian_Sea.svg", height = 400, width = "100%", class = "responsive-img"),
        br(),
        checkboxGroupInput("selected_categories", "Select Categories:", choices = category_choices, selected = category_choices),
      ),
      mainPanel(
        DTOutput("Variables"),
        br(),
        actionButton("continue", "Continue", class = "btn-success"),
        br(),
        br(),
        textOutput("lastUpdate")
      )
    )),
    tabPanel(
                  "Info",
                  fluidRow(
                    includeMarkdown("www/ATAC_description.Rmd"),
                    br(),
                    br(),
                    img(id="img11",src = "ATAC_series_interpretation2023.png", height = 600, width = "100%", class = "responsive-img"),
                    br(),
                    br()
                  )
                )
  )
)

server <- function(input, output, session) {
  # Track original indices when filtering for selected categories
  filtered_data <- reactive({
    req(input$selected_categories)
    
    # Add original indices as an attribute but do not select it
    info_filtered <- info %>%
      mutate(original_index = row_number()) %>% # This preserves original indices
      filter(category %in% input$selected_categories & !is.na(category)) 
    
    # Save original indices in a reactive value
    rv$original_indices <- info_filtered$original_index
    
    # Select columns for DT without showing original_index
    info_filtered %>% select(FullName, unit, category)  
  })
  
  rv <- reactiveValues(DataVariables = NULL, original_indices = NULL, tabsCreated = FALSE)

  output$lastUpdate <- renderText({
    paste("Last updated on:", extract_github_commit_date())
  })
  output$Variables <- renderDataTable(server = FALSE, {
    datatable(
      filtered_data(),
      escape = FALSE,
      extensions = "Select",
      selection = 'none',
      options = list(
        select = list(style = "multi"),
        initComplete = initComplete,
        pageLength = 15,
        autoWidth = TRUE,
        ordering = FALSE
      )
    )
  })
  
  observeEvent(input$continue, {
    req(input$Variables_rows_selected)
    
    selected_indices <- rv$original_indices[as.integer(input$Variables_rows_selected) + 1]
    rv$DataVariables <- table.all[selected_indices]
    
    if (!rv$tabsCreated) {
      rv$tabsCreated <- TRUE
      
      insertTab(inputId = "menu",
                tabPanel(
                  "Graphs",
                  fluidRow(
                    column(12, plotOutput("Graphs", height = "600px") %>% withSpinner(type = 8, image = "rotating_fish.gif", id = "spinner-custom"))
                  )
                ),
                target = "Info", position = "before"
      )
      insertTab(inputId = "menu",
                tabPanel(
                  "Download",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("startyear", "Select a start year:",
                                  choices = seq(from = 1907, to = 2023, by = 1),
                                  selected = 1907),
                      br(),
                      selectInput("endyear", "Select an end year:",
                                  choices = seq(from = 1980, to = 2023, by = 1),
                                  selected = 2023)
                    ),
                    mainPanel(
                      fluidRow(downloadButton("CSV", label = "Download CSV file"))
                    )
                  )
                ),
                target = "Info", position = "before"
      )
      updateTabsetPanel(session, "menu", selected = "Graphs")
    }
  })
  
  output$Graphs <- renderPlot({
    
    session$sendCustomMessage(type = "toggle-spinner", message = "hide")
    selected_data <- rv$DataVariables
    width <- session$clientData$output_Graphs_width
    title_size <- max(ceiling(width / 50), 16)
    axistitle_size <- max(ceiling(width / 60), 12)
    text_size <- max(ceiling(width / 80), 12)
    if (!is.null(selected_data) && length(selected_data) > 0) {
      plotlist <- mapply(function(data_item, j) {
        if (is_tibble(data_item) && ncol(data_item) >= 2) {
          ggATAC(result = data_item,width=width) +
            xlim(c(1980, NA)) +
            ggtitle(paste0(info$FullName[j],
                           "\nData transformation: ", info$transformation[j],
                           "\nAutoregressive process: AR(", info$AR[j], ")")) +
            ylab(paste0("Units: ", info$unit[j])) +
            theme(plot.title = element_text(size = 16),
                  axis.title = element_text(size = axistitle_size, face = "bold"),
                  axis.text = element_text(size = text_size))
        } else {
          ggplot() +
            ggtitle(paste0(info$FullName[j],
                           "\nData transformation: ", info$transformation[j],
                           "\nAutoregressive process: AR(", info$AR[j], ")")) +
            theme(plot.title = element_text(size = 15),
                  axis.title = element_text(size = axistitle_size, face = "bold"),
                  axis.text = element_text(size = text_size))
        }
      }, selected_data, rv$original_indices[as.numeric(input$Variables_rows_selected)], SIMPLIFY = FALSE)
      if (!is.null(plotlist) && length(plotlist) > 0) {
        n <- length(plotlist)
        numrow <- ceiling(n / 2)  
        do.call(gridExtra::grid.arrange, list(grobs = plotlist, nrow = numrow))
      }
    } else {
      print("No data selected for plotting.")
    }
  }, height = reactive({
    width <- session$clientData$output_Graphs_width  # Get dynamic width
    num_plots <- length(rv$DataVariables)  # Get number of selected plots
    if (is.null(width)) return(400) 
    if (num_plots == 0) return(400)
    
    plot_height <- width * 4 / 9  # Maintain 16:9 aspect ratio
    total_height <- plot_height * ceiling(num_plots / 2)  # Scale with number of plots
    return(total_height)
  }))
  
  
  output$CSV <- downloadHandler(
    filename = function() {
      paste0("Data_", format(Sys.time(), "%s"), ".zip")
    },
    content = function(file) {
      # Retrieve selected years
      
      startyear <- as.numeric(input$startyear)
      endyear <- as.numeric(input$endyear)
      # Validate year range
      if (startyear > endyear) {
        # Show error modal dialog and prevent download
        shiny::showModal(shiny::modalDialog(
          title = "Invalid Year Range",
          paste("The start year (", startyear, ") cannot be greater than the end year (", endyear, ")."),
          easyClose = TRUE,
          footer = NULL
        ))
        return()  # Exit without downloading
      }
      # Proceed with data modification and CSV creation if year range is valid
      selected_data <- rv$DataVariables
      
      # Initialize an empty list to store modified data.frames
      modified_data <- list()
      metadata <- list()
      
      for (i in seq_along(selected_data)) {
        data_item <- selected_data[[i]]
        
        # Ensure the data_item is a data.table
        if (!is.data.table(data_item)) {
          data_item <- as.data.table(data_item)
        }
        
        # Filter data based on year range
        data_item <- data_item %>%
          filter(year >= startyear & year <= endyear)
        
        # Add a column to the data.table
        data_item[, TableName := info$FullName[rv$original_indices[input$Variables_rows_selected[i]]]]
        
        
        # Append the modified data.table to the list
        modified_data[[i]] <- data_item
        metadata[[i]]<-info[rv$original_indices[input$Variables_rows_selected[i]],]
      }
      
      # Bind all data.tables row-wise and write to CSV
      Outputtable <- rbindlist(modified_data)
      Metadatatable<-rbindlist(metadata)
      write.csv(Outputtable, "www/Table.csv", row.names = FALSE)
      write.csv(Metadatatable, "www/Metadata.csv", row.names = FALSE)
      zip(file,
          files = c("www/Table.csv", "www/Metadata.csv"),
          extras = '-j')
    }
  )
  
}

shinyApp(ui = ui, server = server)
