#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import rintrojs
#' @noRd
app_ui <- function(request) {
  golem_add_external_resources()# Load external styles and scripts
  
  fluidPage(
    introjsUI(), #Add intro
    # HTML <head> content: CSS, JS, and fonts
    tags$head(
      includeCSS(app_sys("app/www/styles.css")), # Load custom CSS
      tags$script(src = "www/initComplete.js"), # Load custom JavaScript
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&display=swap")  # Add nice font
    ),
    
    withTags({
      div(class = "header",
          checked = NA,
          h1(), #Let some space for visual
          h2("IEA Explorer")) #Title
    }),
    
    # Navigation bar layout with multiple tabs
    navbarPage(
      "Menu", #Navigation bar title
      id = "menu",
      
      # --- First Tab: Variables ---
      tabPanel(
        "Variables",
        sidebarLayout(
          sidebarPanel(
            img(
              # Map image shown in sidebar
              src = "www/Map_Norwegian_Sea.svg",
              height = 400,
              width = "100%",
              class = "responsive-img"
            ),
            br(),
            introBox(
            checkboxGroupInput("selected_categories", "Select Categories:", choices = NULL),
            data.step = 2,
            data.intro = "You can filter the variables by categories."
            ),  # Dynamic checkboxes for selecting categories
            actionButton("help", "Tutorial", class = "btn-blue")
          ),
          mainPanel(
            introBox(
            DT::DTOutput("Variables"), # Add interactive table of variables
            data.step = 1,
            data.intro = "Select here one or more variables you want to look at. You can click on Select All to select all variables from the displayed table."
            ),
            br(),
            introBox(
            actionButton("continue", "Continue", class = "btn-success", width = 200), #Button Continue
            data.step = 3,
            data.intro = "Once you have chose your variables click on Continue to see the graphs."
            ),
            br(),
            br(),
            textOutput("lastUpdate")  # Displays the last update date
          )
        )
      ),tabPanel(
        "Graphs",
        fluidRow(
          column(12,
                 introBox(
                 shinycssloaders::withSpinner(
                   plotOutput("Graphs", height = "600px"),
                   type = 6
                 ),
                 data.step = 4,
                 data.intro = "Here you can see the graphs of the selected variables."
                 )
          )
        )
      ),
      tabPanel(
        "Download",
        introBox(
          fluidRow(
            column(2, br(),
                   downloadButton("CSV", "Download", class = "btn btn-lg btn-primary", style = "width: 100%")
            ),
            column(10,
                   introBox(
                   sliderInput("yearRange", "Select Year Range:",
                               min = 1980,
                               max = as.numeric(format(Sys.Date(), "%Y")),
                               value = c(1980, as.numeric(format(Sys.Date(), "%Y"))),
                               step = 1,
                               sep = "",
                               width = "100%"),
                   data.step = 6,
                   data.intro = "Select the year range for which you want to download the data."
                   )
            )
          ),
          br(),
          fluidRow(
            column(2,
                   introBox(
                   checkboxGroupInput("selected_parameters", "Select parameters to download:",
                                      choices = c("Observations", "Trend", "Prediction", "Prediction percentiles"),
                                      selected = c("Observations", "Trend", "Prediction", "Prediction percentiles")
                   ),
                   data.step = 7,
                   data.intro = "Select the type of data you want to download."
                   )
            ),
            column(10, offset = 2,
                   DT::dataTableOutput("selectedVarsTable")
            )
          ),
          data.step = 5,
          data.intro = "Here you can download the data used to make the graphs."
        )

      ),
      tabPanel(
        "Info",
        introBox(
          fluidRow(
            htmltools::includeMarkdown("inst/app/www/ATAC_description.Rmd"), # Include Markdown description
            br(),
            br(),
            img(
              # Series interpretation image
              id = "img11",
              src = "www/ATAC_series_interpretation2023.png",
              height = 600,
              width = "100%",
              class = "responsive-img"
            ),
            br(),
            br()
          ),
          data.step = 8,
          data.intro = "For information on how to read and interprete the graphs, please refer to the description below."
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  
  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"), app_title = "ieaexplorer"))
}
