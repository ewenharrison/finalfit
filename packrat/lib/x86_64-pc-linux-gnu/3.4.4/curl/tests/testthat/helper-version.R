cat("This is libcurl version", curl_version()$version, "with", curl_version()$ssl_version, "\n")

# Try to load test server
find_test_server <- function(){
  h <- curl::new_handle(timeout = 10, failonerror = TRUE)

  # Try to download latest test-server list
  servers <- tryCatch({
    con <- curl::curl("https://jeroen.github.io/curl/servers.R", handle = h)
    on.exit(close(con))
    out <- source(con, local = TRUE)
    out$value
  }, error = function(e){
    message("Failed to download server list:", e$message)
    c("https://eu.httpbin.org", "https://httpbin.org", "http://httpbin.org")
  })


  # Try each test-server in the list
  for(host in servers){
    tryCatch({
      url <- paste0(host, "/get")
      req <- curl_fetch_memory(url, handle = h)
      return(host)
    }, error = function(e){
      message(paste0("Not using ", host, ": ", e$message))
    })
  }

  stop("All testing servers seem unavailable. No internet connection?")
}

testserver <- find_test_server()
cat("Using test server:", testserver, "\n")

httpbin <- function(path){
  paste0(testserver, "/", sub("^/", "", path))
}
