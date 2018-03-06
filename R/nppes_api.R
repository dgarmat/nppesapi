#' NPPES API
#'
#' Query the NPPES NPI registry.
#'
#' @param query Character string representing a valid query.
#'
#' @return An object of class \code{"nppes_api"}; essentially, an R list with
#' various components depending on the submitted query.
#'
#' @rdname nppes_api
#'
#' @export
#'
#' @examples
#' nppes_api("city=dayton")
#' nppes_api("number=1124021324")

nppes_api <- function(query) {

  # Query the NPPES API
  url <- httr::modify_url("https://npiregistry.cms.hhs.gov/api/", query = query)
  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # Parse the returned JSON file
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  # Turn API errors into R errors
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "NPPES API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  # Return a nppes_api object (a list)
  structure(
    list(
      "content" = parsed,
      "query" = query,
      "response" = resp
    ),
    class = "nppes_api"
  )

}


#' @rdname nppes_api
#'
#' @export
print.nppes_api <- function(x, ...) {
  cat("<NPPES ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}


#' NPPES Fields
#'
#' Print available NPPES field values that can be queried.
#'
#' @references A vector containing the names of the available fields for querying.
#' See https://npiregistry.cms.hhs.gov/registry/help-api for field format requirements.
#' Fields updated March 5th, 2018
#'
#' @export
nppes_fields <- function() {
  c("number", "enumeration_type", "taxonomy_description", "first_name", "last_name", "organization_name",
    "address_purpose", "city", "state", "postal_code", "country_code", "limit", "skip")
}


#' NPI to Specialty
#'
#' Query the NPPES NPI registry with an NPI and return provider's specialty.
#'
#' @param npi Valid 10 digit NPI (National Provider Identifier). Can be a numeric or string.
#'
#' @return An object of class \code{"data.frame"}; containing the NPI queried, specialty
#' and date last updated in NPPES.
#'
#' @rdname npi_specialty
#'
#' @export
#'
#' @examples
#' npi_specialty(1649246182)
#' npi_specialty(1831117050)

npi_specialty <- function(npi) {
  if(nchar(npi) != 10){
    stop(paste0("NPI to query for specialty must be of length 10. NPI ", npi, " is not."))
  }
  nppes_result <- nppes_api(paste0("number=", npi))
  specialty <- nppes_result$content$results[[1]]$taxonomies[[1]]$desc
  last_updated <-  nppes_result$content$results[[1]]$basic$last_updated
  data.frame("number" = npi, "specialty" = specialty, "last_updated" = last_updated)
}


#' NPI Lookup
#'
#' Query the NPPES NPI registry with multiple NPIs and return provider information.
#'
#' @param npis Vector of valid 10 digit NPIs (National Provider Identifier). Can be a numeric, character or mixed vector.
#'
#' @return An object of class \code{"data.frame"}; containing the NPIs queried, and the information requested.
#' One per row. Currently only supports field = speciality.
#'
#' @rdname npi_lookup
#'
#' @export
#'
#' @examples
#' npi_lookup(c(1649246182, 1831117050))

npi_lookup <- function(npis, field = "specialty") {
  # check if field being mapped is one there is a function for
  acceptable_fields <- c("specialty") #, "address")  # will add address later!
  if(!(field %in% acceptable_fields)){
    stop(paste0(field, " field is not one of the acceptable fields. It should be one of these: ",
                paste(acceptable_fields, collapse = ", ")))
  }

  # map according to field specified
  if(field == "specialty"){
    npilist <- lapply(npis, npi_specialty)
  }

  # current plan for this function is a list of ifs, but what if want more than one field?
  # would overwrite it

  # combine all results into a data frame
  do.call(rbind, npilist)
  # note this will error out it any npi returned an error
}


#' NPPES API All
#'
#' Query the NPPES NPI registry with automatic handling of the 200 return limit. Combine results as if one query.
#' The query limit limit is 200. This is overcome with skip = 200, etc. This does all that
#'
#' @param query Character string representing a valid query.
#' @param limit If want to limit rows, can, rather than loop for a long time. Default is 1000.
#' If want all, set limit = 100000000. But it is about 10KB per entry. So the absolute max
#' would be 100,000 providers per GB of ram.
#'
#' @return An object of class \code{"nppes_api"}; essentially, an R list with
#' various components depending on the submitted query.
#'
#' @rdname nppes_api_all
#'
#' @export
#'
#' @examples
#' dayton_providers <- nppes_api_all("city=dayton")
#' length(dayton_providers$content)
#'
#' # note the query can stop early
#' # Are there over 1000 plastic surgeons in Beverley Hills, 90210?
#' plastic_90210 <- nppes_api_all("postal_code=90210&taxonomy_description=PLASTIC+SURGERY", limit = 1000, verbose = TRUE)
#' length(plastic_90210$content)
#' # Nope, fewer than 100 in that zip code. May be some untapped market.
#'

nppes_api_all <- function(query, limit = 10000, verbose = FALSE) {
  # limit is supposed to be an integer, so should have an L after it, but that's not known by everyone
  # and don't want to confuse new users

  # if limit is not an number, return an error. Does this handle negative ints? Or 0?
  if(!is.numeric(limit)){
    stop(paste0("limit must be a number. Entered ", limit))
  }

  if(limit <= 0){
    stop(paste0("limit must be a positive numbner. Entered ", limit))
  }

  if(!is.integer(limit)){
    if(limit - floor(limit) > 0){ # this seems kind of a messy test for an integer
      warning(paste0("removing decimals from limit given, ", limit))
    }
    limit <- as.integer(limit)
  }

  # how many cycles to run max?
  cycles <- ceiling(limit / 200)
  remaining_providers <- limit
  full_query <- list()

  # could do parralell probably
  # start with a for loop, but lapply would make more sense
  for(cycle in seq(1, cycles)){
    if(remaining_providers > 0){ # this seems kind of a messy test for continuing
      result <- tryCatch(nppes_api(paste0(query, "&skip=", ((cycle - 1) * 200), "&limit=200")),
               error = function(err){
                 # handle Server returned nothing errors
                 print(paste0("Error from nppes_api: ", err, ". Skipping cycle ", cycle))
               }
      )
      if(length(result$content$results) == 200){
        # have not run out
        remaining_providers <- remaining_providers - length(result$content$results)
      } else{
        # have run out
        remaining_providers <- 0
      }
      full_query <- c(full_query, result$content$results)
      if(verbose){
        print(paste0(round((remaining_providers / limit), 2) * 100, " percent remaining, at maximum"))
        print(paste0(cycle, " / ", cycles, " API queries ran"))
      }
    }
  }

  # Return an nppes_api object (a list)
  structure(
    list(
      "content" = full_query,     # this is all results appended
      "query" = query,            # this is not replicatable
      "response" = result$resp    # this is the last query ran
    ),
    class = "nppes_api"
  )
}
