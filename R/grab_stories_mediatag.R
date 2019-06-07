##' Return All Stories of a Media Tag
##' 
##' Fetch data on all stories corresponding to a given media tag via the api/v2/stories_public/list method of the MediaCloud API.
##' @usage grab_stories_mediatag(tags_id_media, last_processed_stories_id = 0, rows = 100, max_calls =NULL, feeds_id = NULL, q=NULL, fq = NULL, sort="processed_stories_id", wc=0, show_feeds=0)
##' @param tags_id_media character, the media tag id of the collection of outlets for which all stories shall be fetched.
##' @param last_processed_stories_id integer, Return stories in which the processed_stories_id is greater than this value. (default = 0). This sets the starting point for grabbing all that follows.
##' @param rows integer, number of tag sets to return. Cannot be larger than 100 (default:100)
##' @param max_calls integer, number of total calls to the API. If NULL, no restrictions are applied, all stories will be collected. (default=NULL)
##' @param feeds_id character, Return only stories that match the given feeds_id, sorted my descending publish date (default = NULL)
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
     ##' \dontrun{obama_tags <- grab_stories_mediatag(last_processed_stories_id ="2523432", q="obama AND media_id:1")}
##' \dontrun{obama_tags}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite

grab_stories_mediatag <-
     function (tags_id_media, last_processed_stories_id = 0, rows = 100,
               max_calls= NULL, feeds_id = NULL, q=NULL, fq = NULL,
               sort="processed_stories_id", wc=0, show_feeds=0) {
          
          # prepare iteration
          start <- last_processed_stories_id
          rows <- rows
          n_calls <- 0
          q <- paste0("tags_id_media:", tags_id_media)
          all_stories <- data.table(ap_syndicated = NA,
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
          
          # start stream of api calls and responses
          while (TRUE) {
               
               # query one batch at a time
               stories <- 
                    as.data.table(stories_public_list(last_processed_stories_id = start,
                                        rows = rows,
                                        feeds_id = feeds_id,
                                        q = q,
                                        fq = fq,
                                        sort = sort,
                                        wc = wc,
                                        show_feeds = show_feeds))
    
               # all stories grabbed? (or run into error)
               if (length(stories)==0) break
               
               # add stories to list of stories
               if (n_calls==0){
                    all_stories <- rbindlist(list(all_stories[-1],
                                                  stories),
                                             use.names = TRUE,
                                             fill = TRUE)
               } else {
                    all_stories <- rbindlist(list(all_stories,
                                                  stories),
                                             use.names = TRUE,
                                             fill = TRUE)
               }

               # stop early if there is a restriction on total number of calls
               if (!is.null(max_calls)){
                    n_calls <- n_calls + 1
                    if (n_calls==max_calls) break
               }
               
               # update for next batch
               start <- stories[nrow(stories)]$processed_stories_id
               
          }
          
       return(all_stories)
     }


