library(plumber)
library(rmarkdown)
library(jsonlite)

#* @apiTitle Shiny API
#* @apiDescription API for executing R code on Shiny server, generating plots, and handling R objects

#* Redirect to API documentation
#* @get /
function(res) {
  res$status <- 302
  res$setHeader("Location", "/__swagger__/")
  list(message = "Redirecting to API documentation...")
}

#* Execute R code and return output
#* @param code The R code to execute
#* @param openaiFileIdRefs List of files (optional)
#* @post /execute_code
function(req, res) {
  code <- req$body$code
  
  # If files are included, download them
  if (!is.null(req$body$openaiFileIdRefs)) {
    files <- req$body$openaiFileIdRefs
    for (file in files) {
      download.file(file$download_link, destfile = file$name)
    }
  }
  
  tryCatch({
    result <- capture.output(eval(parse(text = code)))
    res$status <- 200
    list(status = "success", output = paste(result, collapse = "\n"))
  }, error = function(e) {
    res$status <- 400
    list(status = "error", message = e$message)
  })
}

#* Generate and return a plot
#* @param code R code to generate plot
#* @param openaiFileIdRefs List of files (optional)
#* @post /plot
#* @serializer contentType list(type="image/png")
function(req, res) {
  code <- req$body$code
  
  # If files are included, download them
  if (!is.null(req$body$openaiFileIdRefs)) {
    files <- req$body$openaiFileIdRefs
    for (file in files) {
      download.file(file$download_link, destfile = file$name)
    }
  }
  
  tryCatch({
    png(filename = "plot.png")
    eval(parse(text = code))
    dev.off()
    readBin("plot.png", "raw", n = file.info("plot.png")$size)
  }, error = function(e) {
    res$status <- 400
    list(status = "error", message = e$message)
  })
}

#* Save and return an R object
#* @param code R code to create object
#* @param object_name Name of the R object to save
#* @param openaiFileIdRefs List of files (optional)
#* @post /save_object
#* @serializer contentType list(type="application/octet-stream")
function(req, res) {
  code <- req$body$code
  object_name <- req$body$object_name
  
  # If files are included, download them
  if (!is.null(req$body$openaiFileIdRefs)) {
    files <- req$body$openaiFileIdRefs
    for (file in files) {
      download.file(file$download_link, destfile = file$name)
    }
  }
  
  tryCatch({
    eval(parse(text = code))
    save(list = object_name, file = paste0(object_name, ".RData"))
    readBin(paste0(object_name, ".RData"), "raw", n = file.info(paste0(object_name, ".RData"))$size)
  }, error = function(e) {
    res$status <- 400
    list(status = "error", message = e$message)
  })
}

#* Install packages from CRAN
#* @param packages List of packages to install
#* @post /install_cran
function(req, res) {
  packages <- req$body$packages
  tryCatch({
    install.packages(packages)
    res$status <- 200
    list(status = "success", message = paste("Packages installed:", paste(packages, collapse = ", ")))
  }, error = function(e) {
    res$status <- 400
    list(status = "error", message = e$message)
  })
}

#* Install packages from GitHub
#* @param repos List of GitHub repositories to install (in format 'username/repo')
#* @post /install_github
function(req, res) {
  repos <- req$body$repos
  tryCatch({
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    devtools::install_github(repos)
    res$status <- 200
    list(status = "success", message = paste("GitHub repositories installed:", paste(repos, collapse = ", ")))
  }, error = function(e) {
    res$status <- 400
    list(status = "error", message = e$message)
  })
}

# Helper function to clean the R code from user query
clean_r_code <- function(user_query) {
  cleaned_code <- gsub("[^a-zA-Z0-9(){}<>+=*/.,;:_\\s]", "", user_query)
  return(cleaned_code)
}
