##' Return Multiple Processed Stories
##' 
##' Fetch data from the api/v2/stories_public/list method of the MediaCloud API.
##' @usage stories_public_list(last_processed_stories_id = 0, rows = 20, feeds_id = NULL, q=NULL, fq = NULL, sort="processed_stories_id", wc=0, show_feeds=0)
##' @param last_processed_stories_id integer, Return stories in which the processed_stories_id is greater than this value. (default = 0)
##' @param rows integer, number of tag sets to return. Cannot be larger than 100 (default:20)
##' @param feeds_id character, Return only stories that match the given feeds_id, sorted my descending publish date (default = NULL)
##' @param q ("query") parameter which the API method directly passes to Solr (default = NULL)
##' @param fq ("filter query") parameter which the API method directly passes to Solr (default = NULL)
##' @param sort character, Returned results sort order. Supported values: processed_stories_id, random (default = processed_stories_id)
##' @param wc integer, if set to 1, include a 'word_count' field with each story that includes a count of the most common words in the story (default = 0)
##' @param show_feeds integer, if set to 1, include a 'feeds' field with a list of the feeds associated with this story (default=0)
##' @return ...
##' @details  The last_processed_stories_id parameter can be used to page through these results. The API will return stories with aprocessed_stories_id greater than this value.
##' To get a continuous stream of stories as they are processed by Media Cloud, the user must make a series of calls to api/v2/stories_public/list in which last_processed_stories_id for
##' each call is set to the processed_stories_id of the last story in the previous call to the API. A single call can only return up to 10,000 results, but you can get the full list of results by paging through the full list using last_processed_stories_id. 
##' Note: stories_id and processed_stories_id are separate values. The order in which stories are processed is different than the stories_id order. The processing pipeline involves downloading, extracting, and vectoring stories. Requesting by the processed_stories_id field guarantees that the user will receive every story (matching the query criteria if present) in the order it is processed by the system.
##' The q and fq parameters specify queries to be sent to a Solr server that indexes all Media Cloud stories. The Solr server provides full text search indexing of each sentence collected by Media Cloud. All content is stored as individual sentences. The api/v2/stories_public/list call searches for sentences matching the q and / or fq parameters if specified and the stories that include at least one sentence returned by the specified query.
##' The q and fq parameters are passed directly through to Solr.
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2stories_publiclist\cr 
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # Return a stream of all stories from The New York Times mentioning 'obama' greater than the given last_processed_stories_id.
##' # (https://api.mediacloud.org/api/v2/stories_public/list?last_processed_stories_id=2523432&q=text:obama+AND+media_id:1)
     ##' \dontrun{obama_tags <- stories_public_list(last_processed_stories_id ="2523432", q="obama AND media_id:1")}
##' \dontrun{obama_tags}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


stories_public_list <-
     function (last_processed_stories_id = 0, rows = 20, feeds_id = NULL, q=NULL,
               fq = NULL, sort="processed_stories_id", wc=0, show_feeds=0) {

       # parse dummy url
       REQUEST_URL <-  parse_url("https://api.mediacloud.org/api/v2/stories_public/list?last_processed_stories_id=0&rows=20&feeds_id=NULL&q=NULL&fq=NULL&sort=processed_stories_id&wc=0&show_feeds=0&key=NA")
       # define and build url for API call
       REQUEST_URL$query$last_processed_stories_id <- last_processed_stories_id
       REQUEST_URL$query$rows <- rows
       REQUEST_URL$query$feeds_id <- feeds_id
       REQUEST_URL$query$fq <- fq
       REQUEST_URL$query$q <- q
       REQUEST_URL$query$sort <- sort
       REQUEST_URL$query$wc <- wc
       REQUEST_URL$query$show_feeds <- show_feeds
       REQUEST_URL$query$key <- mc.key # must be stored in global environment
       
       
       # fetch the data
       url <- build_url(REQUEST_URL)
       data <- try(fromJSON(url))
       if (class(data)[1] == "try-error") {
         stories_list <- data.frame(ap_syndicated = NA,
                             collect_date="NA",
                             feeds = NA,
                             guid = "NA",
                             language = NA,
                             media_id = "NA",
                             media_name = "NA",
                             media_url = "NA",
                             processed_stories_id = as.integer(NA),
                             publish_date = "NA",
                             stories_id = as.integer(NA),
                             story_tags = list(),
                             title = "NA",
                             url = "NA",
                             word_count = NA)
       } else {
         
         # check response format output
         if (length(data) > 0) {
           # extract data
           stories_list <- data
           stories_list$q <- q
           stories_list$fq <- fq
           stories_list$limit <- limit

         } else {
           # no results found, return dummy df
           message("API returned empty response...\n")
              stories_list <- data.frame(ap_syndicated = NA,
                                         collect_date="NA",
                                         feeds = NA,
                                         guid = "NA",
                                         language = NA,
                                         media_id = "NA",
                                         media_name = "NA",
                                         media_url = "NA",
                                         processed_stories_id = as.integer(NA),
                                         publish_date = "NA",
                                         stories_id = as.integer(NA),
                                         story_tags = list(),
                                         title = "NA",
                                         url = "NA",
                                         word_count = NA)
         }

       }
       
       return(stories_list)
     }


