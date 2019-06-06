##' Stories Public Tag Count
##' 
##' Fetch data from the api/v2/stories_public/tab_count method of the MediaCloud API.
##' @usage stories_public_tagcount(q, fq = NULL, limit=1000, tag_sets_id=NULL)
##' @param q ("query") parameter which the API method directly passes to Solr
##' @param fq ("filter query") parameter which the API method directly passes to Solr (default = NULL)
##' @param limit numeric, number of tags to fetch from Solr (default = 1000)
##' @param tag_sets_id character, return only tags belonging to this tag set (default = NULL)
##' @return ...
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2stories_publiccount\cr 
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # fetch Count pf tags in stories containing the word 'obama' in The New York Times. 
##' # (https://api.mediacloud.org/api/v2/stories_public/tag_count?q=obama&fq=media_id:1&limit=3)
##' \dontrun{obama_tags <- stories_public_tagcount(q="obama", fq="media_id=1", limit=3)}
##' \dontrun{obama_tags}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


stories_public_tagcount <-
     function (q, fq = NULL, limit=1000, tag_sets_id=NULL) {
       # check if arguments are valid for API method
       stopifnot(is.character(q))
       stopifnot(is.character(fq))
       
       
       # parse dummy url
       REQUEST_URL <-  parse_url("https://api.mediacloud.org/api/v2/stories_public/tag_count?limit=1000&tag_sets_id=NA&q=NA&fq=NULL&key=NA")
       # define and build url for API call
       REQUEST_URL$query$limit <- limit
       REQUEST_URL$query$tag_sets_id <- tag_sets_id
       REQUEST_URL$query$q <- q
       REQUEST_URL$query$fq <- fq
       REQUEST_URL$query$key <- mc.key # must be stored in global environment
       
       
       # fetch the data
       url <- build_url(REQUEST_URL)
       data <- try(fromJSON(url))
       if (class(data)[1] == "try-error") {
         tagcount <- data.frame(count=as.integer(NA),
                             description="NA",
                             is_static = NA,
                             label = "NA",
                             show_on_media = NA,
                             show_on_stories = NA,
                             tag = "NA",
                             tag_set_label = "NA",
                             tag_set_name = "NA",
                             tag_sets_id = tag_sets_id,
                             q = q,
                             fq = fq,
                             limit = limit)
       } else {
         
         # check response format output
         if (length(data) > 0) {
           # extract data
           count <- data
           count$q <- q
           count$fq <- fq
           count$limit <- limit

         } else {
           # no results found, return dummy df
           message("API returned empty response...\n")
           tagcount <- data.frame(count=as.integer(NA),
                                  description="NA",
                                  is_static = NA,
                                  label = "NA",
                                  show_on_media = NA,
                                  show_on_stories = NA,
                                  tag = "NA",
                                  tag_set_label = "NA",
                                  tag_set_name = "NA",
                                  tag_sets_id = tag_sets_id,
                                  q = q,
                                  fq = fq,
                                  limit = limit)
         }

       }
       
       return(count)
     }


