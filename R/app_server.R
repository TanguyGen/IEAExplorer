#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import markdown
#' @import tibble
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
  load(url(url_github))
  
  updateCheckboxGroupInput(session, "selected_categories", choices = unique(stats::na.omit(info$Category)), selected = unique(stats::na.omit(info$Category)))
  
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
    
    data <- filtered_data()
    
    data<-data%>% 
      add_column(" " = "", .before = 1)
    
    DT::datatable(
      data,
      escape = FALSE,
      extensions = "Select",
      selection = 'none',
      options = list(
        select = list(style = "multi"),
        initComplete = DT::JS("initTooltipJS"),
        pageLength = 15,
        autoWidth = TRUE,
        ordering = FALSE,
        rownames=FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0,4, 5)),
          list(orderable = TRUE, className = 'select-checkbox', targets = 1)
                          )
      )
    )
  })
  
  observeEvent(input$continue, {
    req(input$Variables_rows_selected)
    selected_ids <- rv$selected_ids[as.integer(input$Variables_rows_selected)]
    rv$DataVariables <- table.all[selected_ids]
    
    Years <- unlist(lapply(rv$DataVariables, function(df) df$year))
    min_year <- min(Years)
    max_year <- max(Years)
    
    if (!rv$tabsCreated) {
      rv$tabsCreated <- TRUE
      insertTab("menu", tabPanel("Graphs",
                                               fluidRow(
                                                 column(12, shinycssloaders::withSpinner(plotOutput("Graphs", height = "600px"), type = 6))
                                               )
      ), target = "Info", position = "before")
      
      insertTab("menu", tabPanel(
        "Download",
        fluidRow(
          column(
            width = 2,
            br(),
            downloadButton("CSV", label = "Download", class = "btn btn-lg btn-primary", style = "width: 100%")
          ),
          column(
            width = 10,
            sliderInput(
              "yearRange", "Select Year Range:",
              min = min_year, max = max_year,
              value = c(min_year, max_year),
              step = 1, sep = "", width = "100%"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 8, offset = 2,
            DT::dataTableOutput("selectedVarsTable")
          )
        )
      ), target = "Info", position = "before")
    }
    updateTabsetPanel(session, "menu", selected = "Graphs")
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
        
        if (tibble::is_tibble(data_item) && ncol(data_item) >= 2) {
          ggATAC(result = data_item, width = width) +
            xlim(c(1980, NA)) +
            ggtitle(stringr::str_wrap(info_item$FullName, width = 30))+
            ylab(info_item$Unit) +
            xlab("Year") +
            theme(
              plot.title = element_text(size = title_size,hjust = 0.5),
              axis.title = element_text(size = axistitle_size, face = "bold"),
              axis.text = element_text(size = text_size)
            )
        } else {
          ggplot() +
            ggtitle(stringr::str_wrap(info_item$FullName, width = 30)) +
            theme(
              plot.title = element_text(size = title_size,hjust = 0.5),
              axis.title = element_text(size = axistitle_size, face = "bold"),
              axis.text = element_text(size = text_size)
            )+
            xlab("Year") 
        }
      })
      
      if (!is.null(plotlist) && length(plotlist) > 0) {
        n <- length(plotlist)
        numrow <- ceiling(n / 2)
        do.call(gridExtra::grid.arrange, list(grobs = plotlist, nrow = numrow))
      }
    }
  }, height = reactive({
    width <- session$clientData$output_Graphs_width
    num_plots <- length(rv$DataVariables)
    if (is.null(width)) return(400)
    if (num_plots == 0) return(400)
    plot_height <- width * 1 / 3
    total_height <- plot_height * ceiling(num_plots / 2)
    return(total_height)
  }))
  
  output$CSV <- downloadHandler(
    filename = function() {
      paste0("Data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
    },
    content = function(file) {
      years <- as.numeric(input$yearRange)
      selected_data <- rv$DataVariables
      modified_data <- list()
      metadata <- list()
      
      for (id in names(selected_data)) {
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
  
  output$selectedVarsTable <- DT::renderDataTable({
    req(rv$DataVariables)
    info %>%
      filter(ID %in% names(rv$DataVariables)) %>%
      select(FullName, Unit, Description, Source) %>%
      rename(
        `Full name` = FullName,
        `Units` = Unit,
        `Description` = Description,
        `Source` = Source
      )
  })
}
