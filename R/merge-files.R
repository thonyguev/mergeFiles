# Hello, world!  This is an example function named 'hello' which prints 'Hello, world!'.  You can learn more about package authoring
# with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard shortcuts for package authoring: Install Package: 'Ctrl + Shift + B'
# Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'

# Development Code
#library(formatR)
#formatR::tidy_rstudio()

# Libraries
library(readxl)
library(dplyr)
library(stringr)

merge_files <- function(path, extension, tag = "full") {
  if (!dir.exists(path)) {
    warning(str_glue("'{path}' does not exist!"))
    return()
  }
  files <- list.files(path = path, pattern = str_glue("*.{extension}$"))
  if (length(files) == 0) {
    warning(str_glue("'.{extension}' extension files not found!"))
    return()
  }
  print(str_glue("Path: {path}"))
  print(str_glue("Files: {length(files)}"))
  merge <- lapply(files, function(file) {
    location_tag <- switch(tag, full = str_glue("{path}/{file}"), relative = str_glue("{file}"))
    current_file <- read_excel(str_glue("{path}/{file}"))
    print(str_glue("{length(current_file)} - columns found in {file}"))
    mutate(current_file, location = rep(location_tag, nrow(current_file)), .before = colnames(current_file[, 1])) %>%
      return()
  })
  convert_rows <- bind_rows(merge)
  data.frame(convert_rows) %>%
    return()
}
