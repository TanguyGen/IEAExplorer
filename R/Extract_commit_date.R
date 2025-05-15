#' Extract the Last Commit Date of a Specific File from a GitHub Repository
#'
#' This function queries the GitHub API to retrieve the most recent commit date
#' If an error occurs, the function returns \code{NULL}.
#'
#'
#' @return A character string in the format \code{"Last updated on: dd/mm/yyyy"} representing the last commit date,
#'         or \code{NULL} if the request fails or the structure is unexpected.
#'
#' @examples
#' extract_github_commit_date()
#'
#' @import httr
#' @import jsonlite
#' @export

extract_github_commit_date <- function() {
  url= "https://api.github.com/repos/ices-eg/WGINOR/commits?path=TAF_ATAC/output/tables.Rdata"
  response <- GET(url)
  if (status_code(response) != 200) {
    message("Failed to retrieve data from GitHub: HTTP Status", status_code(response))
    return(NULL)
  }
  
  # Parse JSON assuming it becomes a data frame with flattened column names
  parsed_data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  
  if (is.data.frame(parsed_data) && nrow(parsed_data) > 0) {
    # Extract the date from the first row corresponding commit's committer's date
    last_commit_date <- parsed_data$commit.committer.date[1]
    last_commit_date <- as.POSIXct(last_commit_date, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    return(paste("Last updated on: ", extract_github_commit_date()))  # Format as "Month-Year"
  } else {
    message("Unexpected structure from GitHub API")
    return(NULL)
  }
}
