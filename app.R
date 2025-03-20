library(shiny)
library(leaflet)
library(sf)
library(tidyr)
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(icesTAF)
library(tibble)
library(shinycssloaders)

initComplete = JS(
  "function(settings, json) {",
  "var selectedIndices = {};",
  "var toggleButton = $('<button id=\"toggle-select-all\" class=\"btn-primary\" style=\"position: absolute; left: 0;margin-left: 10px;\">Select All/Unselect All</button>')",
  "toggleButton.appendTo($(settings.nTableWrapper).find('.dataTables_filter'));",
  "toggleButton.click(function() {",
  "var checked = !(this.dataset.checked === 'true');",
  "this.dataset.checked = checked;",
  "$('.dt-checkbox').each(function() {",
  "var rowIndex = $(this).closest('tr').data('index');",
  "selectedIndices[rowIndex] = checked;",
  "$(this).prop('checked', checked);",
  "});",
  "Shiny.setInputValue('selected_ids', Object.keys(selectedIndices));",
  "});",
  
  "$('.dt-checkbox').on('change', function() {",
  "var index = $(this).closest('tr').data('index');",
  "selectedIndices[index] = $(this).is(':checked');",
  "Shiny.setInputValue('selected_ids', Object.keys(selectedIndices));",
  "});",
  
  "settings.oApi._fnCallbackReg(settings, 'aoDrawCallback', function() {",
  "$('.dt-checkbox').each(function() {",
  "$(this).prop('checked', selectedIndices[$(this).closest('tr').data('index')]);",
  "});",
  "});",
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
    tags$link(rel = "stylesheet", href = "https://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.css"),
    tags$style(type = "text/css", "
      #graphs-container {
        overflow-y: auto;
        border: 1px solid #ccc;
        transition: height 0.5s ease;
        height: auto; /* Use auto for dynamic height */
      }
      #Graphs {
        height: 100vh; /* Set initial height to view height */
        overflow-y: auto; /* Enable vertical scrolling */
      }
    ")
  ),
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
        leafletOutput("map", height = 400),
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
  
  observeEvent(input$select_all, {
    isolate({
      if (length(input$selected_categories) < length(category_choices)) {
        updateCheckboxGroupInput(session, "selected_categories", selected = category_choices)
      } else {
        updateCheckboxGroupInput(session, "selected_categories", selected = character(0))
      }
    })
  })
  
  output$lastUpdate <- renderText({
    paste("Last updated on:", extract_github_commit_date())
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = ecoregions, fillColor = ~factor(Ecoregion), color = "black", opacity = 0.7, fillOpacity = 0.3)
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
        pageLength = 20,
        autoWidth = TRUE,
        ordering = FALSE
      )
    )
  })
  
  # Modifying the observeEvent function for the 'continue' button
  observeEvent(input$continue, {
    req(input$Variables_rows_selected)
    print(input$Variables_rows_selected)
    
    # Convert selected row indices in DT table to original indices stored in rv
    selected_indices <- rv$original_indices[as.numeric(input$Variables_rows_selected) + 1]
    rv$DataVariables <- table.all[selected_indices]
    
    # Check if tabs have not been created yet
    if (!rv$tabsCreated) {
      rv$tabsCreated <- TRUE
      
      appendTab(inputId = "menu",
                tabPanel(
                  "Graphs",
                  fluidRow(
                    column(12, plotOutput("Graphs", height = "800px"))
                  )
                )
      )
      appendTab(inputId = "menu",
                tabPanel(
                  "Download",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("startyear", "Select a start year:", choices = seq(from = 1980, to = 2023, by = 1), selected = 2023),
                      br(),
                      selectInput("endyear", "Select an end year:", choices = seq(from = 1980, to = 2023, by = 1), selected = 2023)
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
    selected_data <- rv$DataVariables
    
    if (!is.null(selected_data) && length(selected_data) > 0) {
      plotlist <- list()
      for (i in seq_along(selected_data)) {
        data_item <- selected_data[[i]]
        j <- rv$original_indices[as.numeric(input$Variables_rows_selected)[i] ] # Original index mapping
        
        if (is_tibble(data_item) && ncol(data_item) >= 2) {
          plotlist[[i]] <- ggATAC(result = data_item) +
            xlim(c(1980, NA)) +
            ggtitle(paste0(info$FullName[j],
                           "\nData transformation: ", info$transformation[j],
                           "\nAutoregressive process: AR(", info$AR[j], ")")) +
            ylab(paste0("Units: ", info$unit[j])) +
            theme(plot.title = element_text(size = 16),
                  axis.title = element_text(size = 14, face = "bold"),
                  axis.text = element_text(size = 12))
          
          if (info$transformation[j] == TRUE) {
            plotlist[[i]] <- plotlist[[i]] + ylim(c(0, NA))
          }
        } else {
          plotlist[[i]] <- ggplot() +
            ggtitle(paste0(info$FullName[j],
                           "\nData transformation: ", info$transformation[j],
                           "\nAutoregressive process: AR(", info$AR[j], ")")) +
            theme(plot.title = element_text(size = 15),
                  axis.title = element_text(size = 14, face = "bold"),
                  axis.text = element_text(size = 12))
        }
      }
      if (!is.null(plotlist) && length(plotlist) > 0) {
        n <- length(plotlist)
        numrow <- ceiling(n / 2)  # Calculate the number of rows needed
        
        # Ensure plotlist is a list of ggplots or grobs
        if (all(sapply(plotlist, inherits, "ggplot"))) {
          do.call(gridExtra::grid.arrange, c(plotlist, nrow = numrow))
        } else {
          print("Plotlist does not contain valid ggplot objects.")
        }
      }
    }
  }, height = function() {
    length(rv$DataVariables) * 400  # Adjust height dynamically based on number of plots
  })
  
  output$CSV <- downloadHandler(
    filename = function() {
      paste0("Data_", format(Sys.time(), "%s"), ".csv")
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
      }
      
      # Bind all data.tables row-wise and write to CSV
      Outputtable <- rbindlist(modified_data)
      write.csv(Outputtable, file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
