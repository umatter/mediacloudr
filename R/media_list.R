##' Return Multiple Media Sources
##' 
##' Fetch data from the api/v2/media/list method of the MediaCloud API.
##' @usage media_list(last_media_id = 0, rows = 20, name = NULL, tag_name = NULL, tags_id=NULL, q=NULL, timespan_id = NULL, topic_mode = NULL, include_dubs = 0, unhealthy = 0, similar_media_id = NULL, sort="id")
##' @param last_media_id integer, Return media sources with a media_id greater than this value (default = 0)
##' @param rows integer, number of tag sets to return. Cannot be larger than 100 (default:20)
##' @param name character, name of media source for which to search (default = NULL)
##' @param tag_name character, Name of tag for which to return belonging media (default = NULL)
##' @param tags_id character, Return media associate with the given tag (default = NULL)
##' @param q ("query") parameter which the API method directly passes to Solr (default = NULL)
##' @param timespans_id Return media within the given timespan (default = NULL)
##' @param topic_mode character, If set to 'live', return media from live topics (default = NULL)
##' @param include_dubs integer, if set to 1, include duplicate media among the results (default = 0 )
##' @param unhealthy integer, if set to 1, only return media that are currently marked as unhealthy (see mediahealth/list) (default = NULL)
##' @param similar_media_id integer, if set to 1, return media with the most tags in common (default = NULL)
##' @param sort character, sort order of media: 'id', or 'num_stories' (default = 'id')
##' @return ...
##' @details  If the name parameter is specified, the call returns only media sources that match a case insensitive search specified value. If the specified value is less than 3 characters long, the call returns an empty list.
##' By default, media are sorted by media_id. If the sort parameter is set to 'num_stories', the media will be sorted by decreasing number of stories in the past 90 days.
##' By default, calls that specify a name parameter will only return media that are not duplicates of some other media source. Media Cloud has many media sources that are either subsets of other media sources or are just holders for spidered media from a given media source, both of which are marked as duplicate media and are not included in the default results. If the 'include_dups' parameter is set to 1, those duplicate sources will be included in the results.
##' the timespans_id parameter is specified, return media within the given time slice, sorted by descending inlink_count within the timespan. If topic_mode is set to 'live', return media from the live topic stories rather than from the frozen snapshot.
##' If the q parameter is specified, return only media that include at least on sentence that matches the given Solr query. For a description of the Solr query format, see the stories_public/list call.
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2medialist
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # https://api.mediacloud.org/api/v2/media/list?last_media_id=1&rows=2
##' 
##' \dontrun{media_list(last_media_id ="1", rows=2)}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


media_list <-
     function (last_media_id = 0, rows = 20, name = NULL,
               tag_name = NULL, tags_id=NULL, q=NULL,
               timespan_id = NULL, topic_mode = NULL,
               include_dubs = 0, unhealthy = NULL,
               similar_media_id = NULL, sort="id") {

       # parse dummy url
       REQUEST_URL <-  parse_url("https://api.mediacloud.org/api/v2/media/list?last_media_id=0&rows=20&name=NULL&tag_name=NULL&tags_id=NULL&q=NULL&timespan_id=NULL&topic_mode=NULL&include_dubs=0&unhealthy=NULL&similar_media_id=NULL&sort=id&key=NA")
       # define and build url for API call
       REQUEST_URL$query$last_media_id <- last_media_id
       REQUEST_URL$query$rows <- rows
       REQUEST_URL$query$name <- name
       REQUEST_URL$query$tag_name <- tag_name
       REQUEST_URL$query$tags_id <- tags_id
       REQUEST_URL$query$q <- q
       REQUEST_URL$query$timespan_id <- timespan_id
       REQUEST_URL$query$topic_mode <- topic_mode
       REQUEST_URL$query$include_dubs <- include_dubs
       REQUEST_URL$query$unhealthy <- unhealthy
       REQUEST_URL$query$similar_media_id <- similar_media_id
       REQUEST_URL$query$sort <- sort
       REQUEST_URL$query$key <- mc.key # must be stored in global environment
       
       
       # fetch the data
       url <- build_url(REQUEST_URL)
       message(paste0("Query API: ", url, "\n"))
       data <- try(fromJSON(url))
       if (class(data)[1] == "try-error") {
         media_list <- data.frame(is_healthy=NA,
                                  is_monitored=NA,
                                  media_id=as.integer(NA),
                                  media_source_tags = "NA",
                                  name = "NA",
                                  num_sentences_90 = as.numeric(NA),
                                  num_stories_90 = as.numeric(NA),
                                  public_notes = "NA",
                                  start_date = "NA",
                                  url = "NA")
       } else {
         
         # check response format output
         if (length(data) > 0) {
           # extract data
           media_list <- data

         } else {
           # no results found, return dummy df
           message("API returned empty response...\n")
              media_list <- data.frame(is_healthy=NA,
                                       is_monitored=NA,
                                       media_id=as.integer(NA),
                                       media_source_tags = "NA",
                                       name = "NA",
                                       num_sentences_90 = as.numeric(NA),
                                       num_stories_90 = as.numeric(NA),
                                       public_notes = "NA",
                                       start_date = "NA",
                                       url = "NA")
         }

       }
       
       return(media_list)
     }


