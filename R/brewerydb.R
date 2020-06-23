BreweryDB <- R6::R6Class(
  "BreweryDB",
  public = list(
    initialize = function(format = private$.formats) {
      self$format <- match.arg(format)
    },

    get = function(query) {
      query <- checkmate::assert_list(query)
      query <- purrr::list_modify(list(format = self$format), !!!query)

      url <- httr::modify_url(private$.base_uri, query = query)

      response <- httr::GET(url, params = list(key = private$.key))

      content <- httr::content(response, "text")

      jsonlite::fromJSON(content)
    }
  ),

  private = list(
    .formats = c("json", "php", "xml"),
    .format = "php",
    .base_uri = "https://sandbox-api.brewerydb.com/v2/",
    .key = Sys.getenv("sandbox_key")
  ),

  active = list(
    format = function(f) {
      if (missing(f)) {
        return(private$.format)
      }

      private$.format <- f
    },

    breweries = function() {
      self$get(list())
    }
  )
)

bdb <- BreweryDB$new()
bdb$breweries
