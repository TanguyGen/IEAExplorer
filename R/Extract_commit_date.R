library(httr)
library(jsonlite)

# Function to fetch and extract last update date from GitHub API
extract_github_commit_date <- function() {
  response <- GET(url)
  url= "https://api.github.com/repos/ices-eg/WGINOR/commits?path=TAF_ATAC/output/tables.Rdata"
  if (status_code(response) != 200) {
    stop("Failed to retrieve data from GitHub: HTTP Status", status_code(response))
  }
  
  # Parse JSON assuming it becomes a data frame with flattened column names
  parsed_data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  
  if (is.data.frame(parsed_data) && nrow(parsed_data) > 0) {
    # Extract the date from the first row corresponding commit's committer's date
    last_commit_date <- parsed_data$commit.committer.date[1]
    last_commit_date <- as.POSIXct(last_commit_date, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    return(format(last_commit_date, "%d/%m/%Y"))  # Format as "Month-Year"
  } else {
    stop("Unexpected structure from GitHub API")
  }
}
