# Load necessary libraries
library(shiny)
library(shinyjs)
library(tidyr)
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(shinycssloaders)
library(tibble)
library(data.table)
library(shinythemes)
library(xlsx)
library(markdown)

# Define JavaScript for row hover tooltip
rowHoverTooltip <- JS(
  "function(settings, json){",
  "  var table = this.api();",
  "  // Create a tooltip div",
  "  var tooltipDiv = $('<div></div>').attr('id', 'tooltip').css({",
  "    position: 'absolute',",
  "    padding: '8px',",
  "    background: '#eeeeee',",
  "    border: '1px solid #dddddd',",
  "    visibility: 'hidden'",
  "  }).appendTo('body');",
  
  "  table.on('mouseover', 'tr', function() {",
  "    var rowData = table.row(this).data();",
  "    if (rowData) {",
  "      $('#tooltip').html('Full name: ' + rowData[0] + '<br>Unit: ' + rowData[1] + '<br>Category: ' + rowData[2]);",
  "      tooltipDiv.css('visibility', 'visible').fadeIn(100);",
  "    }",
  "  });",
  
  "  table.on('mouseout', 'tr', function() {",
  "    tooltipDiv.css('visibility', 'hidden').fadeOut(100);",
  "  });",
  
  "  table.on('mousemove', function(e) {",
  "    $('#tooltip').css('top', e.pageY + 10).css('left', e.pageX + 10);",
  "  });",
  
  "}"
)

# Load necessary data
url_github <- "https://raw.githubusercontent.com/ices-eg/WGINOR/refs/heads/main/TAF_ATAC/output/tables.Rdata"
load(url(url_github))
info <- info[-1,]

# Extract unique categories excluding NAs for UI creation
category_choices <- unique(info$Category)
category_choices <- category_choices[!is.na(category_choices)]

ui <- fluidPage(
  tags$head(
    includeCSS("www/styles.css"),
    includeScript("www/scripts.js"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&display=swap"),
  ),
  useShinyjs(),
  withTags({
    div(
      class = "header",
      checked = NA,
      h1(),
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
        actionButton("continue", "Continue", class = "btn-success", width = 200),
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
        img(id = "img11", src = "ATAC_series_interpretation2023.png", height = 600, width = "100%", class = "responsive-img"),
        br(),
        br()
      )
    )
  )
)

server <- function(input, output, session) {
  # Track original IDs when filtering for selected categories
  filtered_data <- reactive({
    req(input$selected_categories)
    
    info_filtered <- info %>%
      filter(Category %in% input$selected_categories & !is.na(Category))
    
    # Save IDs in a reactive value
    rv$selected_ids <- info_filtered$ID
    
    # Select columns for DT
    info_filtered %>% select(FullName, Unit, Category) %>%
      rename(`Full name` = FullName)
  })
  
  rv <- reactiveValues(DataVariables = NULL, selected_ids = NULL, tabsCreated = FALSE)
  
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
        initComplete = rowHoverTooltip,
        pageLength = 15,
        autoWidth = TRUE,
        ordering = FALSE
      )
    )
  })
  
  observeEvent(input$continue, {
    req(input$Variables_rows_selected)
    
    # Use selected indices to get corresponding IDs
    selected_ids <- rv$selected_ids[as.integer(input$Variables_rows_selected)]
    
    rv$DataVariables <- table.all[selected_ids]  # Use IDs for data extraction
    
    Years <- unlist(lapply(rv$DataVariables, function(df) df$year))
    min_year <- min(Years)
    max_year <- max(Years)
    
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
                      sliderInput("yearRange", "Select Year Range:", min = min_year, max = max_year, value = c(min_year, max_year), step = 1, sep = "", width = "100%"),
                      width = "100%"
                    ),
                    mainPanel(
                      fluidRow(downloadButton("CSV", label = "Download xlsx file"))
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
      plotlist <- lapply(names(selected_data), function(id) {
        data_item <- selected_data[[id]]
        info_item <- info[info$ID == id, ]
        
        if (is_tibble(data_item) && ncol(data_item) >= 2) {
          ggATAC(result = data_item, width = width) +
            xlim(c(1980, NA)) +
            ggtitle(paste0(info_item$FullName,
                           "\nData transformation: ", info_item$Transformation,
                           "\nAutoregressive process: AR(", info_item$AR, ")")) +
            ylab(paste0("Units: ", info_item$Unit)) +
            theme(plot.title = element_text(size = title_size),
                  axis.title = element_text(size = axistitle_size, face = "bold"),
                  axis.text = element_text(size = text_size))
        } else {
          ggplot() +
            ggtitle(paste0(info_item$FullName,
                           "\nData transformation: ", info_item$Transformation,
                           "\nAutoregressive process: AR(", info_item$AR, ")")) +
            theme(plot.title = element_text(size = title_size),
                  axis.title = element_text(size = axistitle_size, face = "bold"),
                  axis.text = element_text(size = text_size))
        }
      })
      
      if (!is.null(plotlist) && length(plotlist) > 0) {
        n <- length(plotlist)
        numrow <- ceiling(n / 2)
        do.call(gridExtra::grid.arrange, list(grobs = plotlist, nrow = numrow))
      }
    } else {
      print("No data selected for plotting.")
    }
  }, height = reactive({
    width <- session$clientData$output_Graphs_width
    num_plots <- length(rv$DataVariables)
    if (is.null(width)) return(400)
    if (num_plots == 0) return(400)
    
    plot_height <- width * 1/3
    total_height <- plot_height * ceiling(num_plots / 2)
    return(total_height)
  }))
  
  output$CSV <- downloadHandler(
    filename = function() {
      paste0("Data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
    },
    content = function(file) {
      # Retrieve selected years
      years <- as.numeric(input$yearRange)
      
      selected_data <- rv$DataVariables
      
      # Initialize an empty list to store modified data.frames
      modified_data <- list()
      metadata <- list()
      
      for (id in names(selected_data)) {
        data_item <- selected_data[[id]]
        
        # Ensure the data_item is a data.table
        if (!is.data.table(data_item)) {
          data_item <- as.data.table(data_item)
        }
        
        # Filter data based on year range
        data_item <- data_item %>%
          filter(year >= years[1] & year <= years[2])
        
        # Add a column to the data.table
        data_item[, Variable := info$FullName[info$ID == id]]
        
        # Append the modified data.table to the list
        modified_data[[id]] <- data_item
        metadata[[id]] <- info[info$ID == id, ]
      }
      
      # Bind all data.tables row-wise and write to CSV
      Outputtable <- rbindlist(modified_data)
      Metadatatable <- rbindlist(metadata)
      write.xlsx(Metadatatable, file = file, sheetName = "Metadata", row.names = FALSE)
      write.xlsx(Outputtable, file = file, sheetName = "Table", append = TRUE, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
