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

initComplete <- JS(
  "function(settings, json){",
  "  var table = this.api();",
  "  $('<button id=\"toggle-select-all\" class=\"btn btn-primary\" style=\"margin: 10px;\">Select All/Unselect All</button>').prependTo($(table.table().container()).find('.dataTables_filter'));",
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
  tags$head(includeCSS("www/styles.css")),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Manrope:wght@300&family=Roboto:wght@100&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.css")
  ),
  useShinyjs(),
  withTags({
    div(
      class = "header",
      checked = NA,
      h1(
        img(
          width = 200,
          src = "ICES_logo.png",
          class = "topleft"
        )
      ),
      h2("WGINOR Shiny")
    )
  }),
  navbarPage(
    "Menu",
    id = "menu",
    tabPanel("Ecoregions and Variables", sidebarLayout(
      sidebarPanel(
        img(src = "Map_Norwegian_Sea.svg", height = 400, width = "100%", class = "responsive-img"),
        selectInput("selected_shapefile", "Choose ecoregions", choices = unique(ecoregions$Ecoregion)),
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
    ))
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
      
      appendTab(inputId = "menu",
                tabPanel(
                  "Graphs",
                  fluidRow(
                    img(id="img11",src = "ATAC_series_interpretation2023.png", height = 600, width = "100%", class = "responsive-img"),
                    br(),
                    br(),
                    column(12, plotOutput("Graphs", height = "600px") %>% withSpinner(type = 8, image = "rotating_fish.gif", id = "spinner-custom"))
                  )
                )
      )
      appendTab(inputId = "menu",
                tabPanel(
                  "Download",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("startyear", "Select a start year:",
                                  choices = seq(from = 1980, to = 2023, by = 1),
                                  selected = 2023),
                      br(),
                      selectInput("endyear", "Select an end year:",
                                  choices = seq(from = 1980, to = 2023, by = 1),
                                  selected = 2023)
                    ),
                    mainPanel(
                      fluidRow(downloadButton("CSV", label = "Download CSV file"))
                    )
                  )
                )
      )
      updateTabsetPanel(session, "menu", selected = "Graphs")
    }
  })
  
  output$Graphs <- renderPlot({
    
    session$sendCustomMessage(type = "toggle-spinner", message = "hide")
    selected_data <- rv$DataVariables
    if (!is.null(selected_data) && length(selected_data) > 0) {
      plotlist <- lapply(selected_data, function(data_item, j) {
        if (is_tibble(data_item) && ncol(data_item) >= 2) {
          ggATAC(result = data_item) +
            xlim(c(1980, NA)) +
            ggtitle(paste0(info$FullName[j],
                           "\nData transformation: ", info$transformation[j],
                           "\nAutoregressive process: AR(", info$AR[j], ")")) +
            ylab(paste0("Units: ", info$unit[j])) +
            theme(plot.title = element_text(size = 16),
                  axis.title = element_text(size = 14, face = "bold"),
                  axis.text = element_text(size = 12))
        } else {
          ggplot() +
            ggtitle(paste0(info$FullName[j],
                           "\nData transformation: ", info$transformation[j],
                           "\nAutoregressive process: AR(", info$AR[j], ")")) +
            theme(plot.title = element_text(size = 15),
                  axis.title = element_text(size = 14, face = "bold"),
                  axis.text = element_text(size = 12))
        }
      }, j = rv$original_indices[as.numeric(input$Variables_rows_selected)])
      
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
    if (is.null(width)) return(400)  # Default height if not yet available
    if (num_plots == 0) return(400)  # Default when no plots
    
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
      req(startyear <= endyear)
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
      write.csv(Outputtable, "www/Table.csv")
      write.csv(Metadatatable, "www/Metadata.csv")
      zip(file,
          files = c("www/Table.csv", "www/Metadata.csv"),
          extras = '-j')
    }
  )
  
}

shinyApp(ui = ui, server = server)
