e <- new.env()
load(url("https://raw.githubusercontent.com/ices-eg/WGINOR/refs/heads/main/TAF_ATAC/output/tables.Rdata"), envir = e)
if (length(ls(envir = e)) != 2) {
  showNotification("Default RData must contain two objects.", type = "error")
  return()
}
Info <- e$info
Table.all <- e$table.all

var_test <- c("NAO1","Oil","ZooBMay")

data_test <- Table.all[var_test]
