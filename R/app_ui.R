#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  golem_add_external_resources()
  fluidPage(
    tags$head(
      includeCSS(app_sys("app/www/styles.css")),
      tags$script(src = "www/initComplete.js"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&display=swap")
    ),
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
      tabPanel("Ecoregions and Variables",
                      sidebarLayout(
                        sidebarPanel(
                          img(src = "www/Map_Norwegian_Sea.svg", height = 400, width = "100%", class = "responsive-img"),
                          br(),
                          checkboxGroupInput("selected_categories", "Select Categories:", choices = NULL)
                        ),
                        mainPanel(
                          DT::DTOutput("Variables"),
                          br(),
                          actionButton("continue", "Continue", class = "btn-success", width = 200),
                          br(),
                          br(),
                          textOutput("lastUpdate")
                        )
                      )
      ),
      tabPanel("Info",
                      fluidRow(
                        htmltools::includeMarkdown("inst/app/www/ATAC_description.Rmd"),
                        br(),
                        br(),
                        img(id = "img11", src = "www/ATAC_series_interpretation2023.png", height = 600, width = "100%", class = "responsive-img"),
                        br(),
                        br()
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
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ieaexplorer"
    )
  )
}
