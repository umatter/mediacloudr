##' Return Media Sources Health
##' 
##' Fetch data from the api/v2/mediahealth/list method of the MediaCloud API.
##' @usage media_health_list(media_id)
##' @param media_id integer, media id of sources to fetch data on
##' @return A list of two data frames, one with the general health variables, and one on the coverage gaps
##' @details  The Media Health API call provides information about the health of a media source, 
##' meaning to what degree we are capturing all of the stories published by that media source. 
##' Media Cloud collects its data via automatically detected RSS feeds on the open web. 
##' This means first that the system generally has data for a given media source from the time we 
##' first enter that source into our database. Second, Media Cloud data for a given media source is 
##' only as good as the set of feeds we have for that source. Our feed scraper is not perfect and so 
##' sometimes misses feeds it should be collecting. Third, feeds change over time. We periodically 
##' rescrape every media source for new feeds, but this takes time and is not perfect.
##' The only way we have of judging the health is judging the relative number of stories over time. 
##' This media call provides a set of metrics that compare the current number of stories being 
##' collected by the media source with the number of stories collected over the past 90 days, 
##' and also compares coverage over time with the expected volume. More details are in the field 
##' descriptions in the reference link to the github documentation of the API.
##' @references https://github.com/mediacloud/backend/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2mediahealthlist
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' 
##' \dontrun{media_health_list(media_id =1)}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


media_health_list <-
     function (media_id) {
          
          # parse dummy url
          REQUEST_URL <-  parse_url("https://api.mediacloud.org/api/v2/mediahealth/list?media_id=0&key=NA")
          # define and build url for API call
          REQUEST_URL$query$media_id <- media_id
          REQUEST_URL$query$key <- mc.key # must be stored in global environment
          
          
          # fetch the data
          url <- build_url(REQUEST_URL)
          message(paste0("Query API: ", url, "\n"))
          data <- try(fromJSON(url))
          if (class(data)[1] == "try-error") {
               media_list <- data.frame(media_id=media_id)
          } else {
               
               # check response format output
               if (length(data) > 0) {
                    # extract data
                    media_list <- data
                    coverage_gap_list <- data$coverage_gaps_list
                    media_list$coverage_gab_list <- NULL
                    
               } else {
                    # no results found, return dummy df
                    message("API returned empty response...\n")
                    media_list <- data.frame(media_id=media_id)
                    coverage_gap_list <- data.frame(media_id=media_id)
                    
               }
               
          }
          
          return(list(media_list=media_list, coverage_gap_list=coverage_gap_list))
     }


