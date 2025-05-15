#' Extract the Last Commit Date of a Specific File from a GitHub Repository
#'
#' This function queries the GitHub API to retrieve the most recent commit date
#' of a specified file within a repository. It optionally uses a GitHub personal
#' access token to avoid rate limiting.
#'
#' @param token Optional. A GitHub personal access token (character string) to authenticate the request.
#'              This helps avoid rate limiting by the GitHub API.
#'
#' @return A character string in the format \code{"dd/mm/yyyy"} representing the last commit date,
#'         or \code{NULL} if the request fails or the structure is unexpected.
#'
#' @examples
#' extract_github_commit_date()
#' extract_github_commit_date("your_token_here")
#'
#' @import httr
#' @import jsonlite
#' @export

extract_github_commit_date <- function(token = NULL) {
  url <- "https://api.github.com/repos/ices-eg/WGINOR/commits?path=TAF_ATAC/output/tables.Rdata"
  
  headers <- add_headers(`User-Agent` = "R script")
  
  if (!is.null(token)) {
    headers <- add_headers(`User-Agent` = "R script",
                           Authorization = paste("token", token))
  }
  
  response <- tryCatch({
    GET(url, headers)
  }, error = function(e) {
    message("Request error: ", e$message)
    return(NULL)
  })
  
  if (is.null(response) || status_code(response) != 200) {
    message("GitHub request failed: HTTP ",
            if (!is.null(response))
              status_code(response)
            else
              "unknown")
    return(NULL)
  }
  
  parsed_data <- tryCatch({
    fromJSON(content(response, "text"), flatten = TRUE)
  }, error = function(e) {
    message("JSON parsing error: ", e$message)
    return(NULL)
  })
  
  if (is.data.frame(parsed_data) && nrow(parsed_data) > 0) {
    last_commit_date <- parsed_data$commit.committer.date[1]
    last_commit_date <- as.POSIXct(last_commit_date, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    return(paste("Last updated on:", format(last_commit_date, "%d/%m/%Y")))
  } else {
    message("Unexpected API structure or no commits found.")
    return(NULL)
  }
}
