##' Stories Public Tag Count
##' 
##' Fetch data from the api/v2/stories_public/tab_count method of the MediaCloud API.
##' @usage tag_sets_list(last_tag_sets_id = 0, rows = 20)
##' @param last_tag_sets_id integer, return tag sets with a tag_sets_id greater than this value (default = 0)
##' @param rows integer, number of tag sets to return. Cannot be larger than 100 (default:20)
##' @return ...
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2stories_publiccount\cr 
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # https://api.mediacloud.org/api/v2/tag_sets/list
##' \dontrun{tagslist <- tag_sets_list()}
##' \dontrun{tagslist}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


tag_sets_list <-
     function (last_tag_sets_id = 0, rows = 20) {

       
       # parse dummy url
       REQUEST_URL <-  parse_url("https://api.mediacloud.org/api/v2/tag_sets/list?rows=NA&last_tag_sets_id=NA&key=NA")
       # define and build url for API call
       REQUEST_URL$query$last_tag_sets_id <- last_tag_sets_id
       REQUEST_URL$query$rows <- rows
       REQUEST_URL$query$key <- mc.key # must be stored in global environment
       
       
       # fetch the data
       url <- build_url(REQUEST_URL)
       data <- try(fromJSON(url))
       if (class(data)[1] == "try-error") {
         tagsetslist <- data.frame(description="NA",
                             label="NA",
                             name = "NA",
                             show_on_media = NA,
                             show_on_stories = NA,
                             tag_sets_id = as.integer(NA),
                             last_tag_sets_id = last_tag_sets_id,
                             rows = rows)
       } else {
         
         # check response format output
         if (length(data) > 0) {
           # extract data
           tagsetslist <- data
           tagsetslist$last_tag_sets_id <- last_tag_sets_id
           tagsetslist$rows <- rows

         } else {
           # no results found, return dummy df
           message("API returned empty response...\n")
           tagsetslist <- data.frame(description="NA",
                                     label="NA",
                                     name = "NA",
                                     show_on_media = NA,
                                     show_on_stories = NA,
                                     tag_sets_id = as.integer(NA),
                                     last_tag_sets_id = last_tag_sets_id,
                                     rows = rows)
         }

       }
       
       return(tagsetslist)
     }


