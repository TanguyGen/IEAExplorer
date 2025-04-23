#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import markdown
#' @importFrom DT datatable renderDataTable
#' @importFrom shinycssloaders withSpinner
#' @importFrom gridExtra grid.arrange
#' @importFrom tibble is_tibble
#' @importFrom data.table as.data.table rbindlist is.data.table
#' @importFrom xlsx write.xlsx
#' @noRd
#' 
app_server <- function(input, output, session) {
  rv <- reactiveValues(DataVariables = NULL, selected_ids = NULL, tabsCreated = FALSE)
  # Load data
  url_github <- "https://raw.githubusercontent.com/ices-eg/WGINOR/refs/heads/main/TAF_ATAC/output/tables.Rdata"
  base::load(base::url(url_github))
  
  updateCheckboxGroupInput(session, "selected_categories", choices = base::unique(stats::na.omit(info$Category)), selected = base::unique(stats::na.omit(info$Category)))
  
  filtered_data <- reactive({
    req(input$selected_categories)
    info_filtered <- filter(info, Category %in% input$selected_categories & !is.na(Category))
    rv$selected_ids <- info_filtered$ID
    select(info_filtered, FullName, Unit, Category, Description, Source) %>% rename(`Full name` = FullName)
  })
  
  output$lastUpdate <- renderText({
    paste("Last updated on:", extract_github_commit_date())
  })
  
  output$Variables <- DT::renderDataTable(server = FALSE, {
    DT::datatable(
      filtered_data(),
      escape = FALSE,
      extensions = "Select",
      selection = 'none',
      options = list(
        select = list(style = "multi"),
        initComplete = DT::JS("initTooltipJS"),
        pageLength = 15,
        autoWidth = TRUE,
        ordering = FALSE,
        columnDefs = list(list(visible = FALSE, targets = c(4, 5)))
      )
    )
  })
  
  observeEvent(input$continue, {
    req(input$Variables_rows_selected)
    selected_ids <- rv$selected_ids[base::as.integer(input$Variables_rows_selected)]
    rv$DataVariables <- table.all[selected_ids]
    
    Years <- base::unlist(base::lapply(rv$DataVariables, function(df) df$year))
    min_year <- base::min(Years)
    max_year <- base::max(Years)
    
    if (!rv$tabsCreated) {
      rv$tabsCreated <- TRUE
      insertTab("menu", tabPanel("Graphs",
                                               fluidRow(
                                                 column(12, shinycssloaders::withSpinner(plotOutput("Graphs", height = "600px"), type = 8, image = "www/rotating_fish.gif", id = "spinner-custom"))
                                               )
      ), target = "Info", position = "before")
      
      insertTab("menu", tabPanel("Download",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   sliderInput("yearRange", "Select Year Range:", min = min_year, max = max_year, value = c(min_year, max_year), step = 1, sep = "", width = "100%")
                                                 ),
                                                 mainPanel(fluidRow(downloadButton("CSV", label = "Download xlsx file")))
                                               )
      ), target = "Info", position = "before")
    }
    updateTabsetPanel(session, "menu", selected = "Graphs")
  })
  
  output$Graphs <- renderPlot({
    session$sendCustomMessage(type = "toggle-spinner", message = "hide")
    selected_data <- rv$DataVariables
    width <- session$clientData$output_Graphs_width
    title_size <- base::max(base::ceiling(width / 50), 16)
    axistitle_size <- base::max(base::ceiling(width / 60), 12)
    text_size <- base::max(base::ceiling(width / 80), 12)
    
    if (!base::is.null(selected_data) && base::length(selected_data) > 0) {
      plotlist <- base::lapply(base::names(selected_data), function(id) {
        data_item <- selected_data[[id]]
        info_item <- info[info$ID == id, ]
        
        if (tibble::is_tibble(data_item) && base::ncol(data_item) >= 2) {
          ggATAC(result = data_item, width = width) +
            xlim(c(1980, NA)) +
            ggtitle(info_item$FullName) +
            ylab(paste0("Units: ", info_item$Unit)) +
            theme(
              plot.title = element_text(size = title_size),
              axis.title = element_text(size = axistitle_size, face = "bold"),
              axis.text = element_text(size = text_size)
            )
        } else {
          ggplot() +
            ggtitle(info_item$FullName) +
            theme(
              plot.title = element_text(size = title_size),
              axis.title = element_text(size = axistitle_size, face = "bold"),
              axis.text = element_text(size = text_size)
            )
        }
      })
      
      if (!base::is.null(plotlist) && base::length(plotlist) > 0) {
        n <- base::length(plotlist)
        numrow <- base::ceiling(n / 2)
        base::do.call(gridExtra::grid.arrange, list(grobs = plotlist, nrow = numrow))
      }
    }
  }, height = reactive({
    width <- session$clientData$output_Graphs_width
    num_plots <- base::length(rv$DataVariables)
    if (base::is.null(width)) return(400)
    if (num_plots == 0) return(400)
    plot_height <- width * 1 / 3
    total_height <- plot_height * base::ceiling(num_plots / 2)
    return(total_height)
  }))
  
  output$CSV <- downloadHandler(
    filename = function() {
      paste0("Data_", base::format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
    },
    content = function(file) {
      years <- base::as.numeric(input$yearRange)
      selected_data <- rv$DataVariables
      modified_data <- list()
      metadata <- list()
      
      for (id in base::names(selected_data)) {
        data_item <- selected_data[[id]]
        if (!data.table::is.data.table(data_item)) {
          data_item <- data.table::as.data.table(data_item)
        }
        data_item <- filter(data_item, year >= years[1] & year <= years[2])
        data_item[, Variable := info$FullName[info$ID == id]]
        modified_data[[id]] <- data_item
        metadata[[id]] <- info[info$ID == id, ]
      }
      
      Outputtable <- data.table::rbindlist(modified_data)
      Metadatatable <- data.table::rbindlist(metadata)
      xlsx::write.xlsx(Metadatatable, file = file, sheetName = "Metadata", row.names = FALSE)
      xlsx::write.xlsx(Outputtable, file = file, sheetName = "Table", append = TRUE, row.names = FALSE)
    }
  )
}
