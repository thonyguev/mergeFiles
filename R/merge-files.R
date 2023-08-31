# Hello, world!  This is an example function named 'hello' which prints 'Hello, world!'.  You can learn more about package authoring
# with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard shortcuts for package authoring: Install Package: 'Ctrl + Shift + B'
# Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'

# Development Code
#library(formatR)
#formatR::tidy_rstudio()

check_library <- function(lib_name) {
  lib <- as.character(substitute(lib_name))
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
  }
  if (require(lib, character.only = TRUE)) {
    library(lib, character.only = TRUE)
  }
}

merge_files <- function(path, extension, tag = "full") {
  check_library(readxl)
  check_library(dplyr)
  check_library(stringr)
  check_library(magrittr)

  if (!dir.exists(path)) {
    warning(str_glue("'{path}' does not exist!"))
    return()
  }
  files <- list.files(path = path, pattern = str_glue("*.{extension}$"))
  if (length(files) == 0) {
    warning(str_glue("'.{extension}' extension files not found!"))
    return()
  }
  message(str_glue("Path: {path}"))
  message(str_glue("Files: {length(files)}"))

  merge <- lapply(files, function(file) {
    message(str_glue("{file}:"))

    location_tag <- switch(tag, full = str_glue("{path}/{file}"), relative = str_glue("{file}"))

    sheets <- excel_sheets(str_glue("{path}/{file}"))
    message(paste0(" - sheet: ", length(sheets)))

    current_file <- lapply(sheets,function (sheet) read_excel(str_glue("{path}/{file}"),sheet = sheet)) %>%
      bind_rows() %>%
      data.frame()

    mutate(current_file, location = rep(location_tag, nrow(current_file)), .before = colnames(current_file[, 1])) %>%
      return()
  })
  convert_rows <- bind_rows(merge)
  data.frame(convert_rows) %>%
    return()
}

#path_1 <- "C:/Users/dev/Documents/pdf excel/ofertas"
#path_2 <- "C:/Users/dev/Desktop"
#merge_files(path = path_2, extension = "xlsx") %>% View()
