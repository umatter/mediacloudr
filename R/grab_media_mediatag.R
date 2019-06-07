##' Return All Media of a Media Tag
##' 
##' Fetch data on all media outlets corresponding to a given media tag via the api/v2/media/list method of the MediaCloud API.
##' @usage grab_media_mediatag(tags_id_media, last_media_id = 0, rows = 100, max_calls=NULL, name = NULL, tag_name = NULL, tags_id=NULL, q=NULL, timespan_id = NULL, topic_mode = NULL, include_dubs = 0, unhealthy = 0, similar_media_id = NULL, sort="id")
##' @param tags_id_media character, the media tag id of the collection of outlets for which all media shall be fetched.
##' @param last_media_id integer, Return media sources with a media_id greater than this value (default = 0)
##' @param rows integer, number of tag sets to return. Cannot be larger than 100 (default:100)
##' @param max_calls integer, number of total calls to the API. If NULL, no restrictions are applied, all media will be collected. (default=NULL)
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


grab_media_mediatag <-
     function (tags_id_media, last_media_id = 0, rows = 100, max_calls=NULL,
               name = NULL, tag_name = NULL, tags_id=NULL, q=NULL,
               timespan_id = NULL, topic_mode = NULL, include_dubs = 0,
               unhealthy = 0, similar_media_id = NULL, sort="id") {
          
          # prepare iteration
          start <- last_media_id
          rows <- rows
          n_calls <- 0
          q <- paste0("tags_id_media:", tags_id_media)
          all_media <- data.frame(is_healthy=NA,
                                   is_monitored=NA,
                                   media_id=as.integer(NA),
                                   media_source_tags = "NA",
                                   name = "NA",
                                   num_sentences_90 = as.numeric(NA),
                                   num_stories_90 = as.numeric(NA),
                                   public_notes = "NA",
                                   start_date = "NA",
                                   url = "NA")
          
          # start stream of api calls and responses
          while (TRUE) {
               
               # query one batch at a time
               media <- 
                    as.data.table(media_list(last_media_id = start,
                                             rows = rows,
                                             name = name,
                                             tag_name = tag_name,
                                             tags_id = tags_id,
                                             q = q,
                                             timespan_id = timespan_id,
                                             topic_mode = topic_mode,
                                             include_dubs = include_dubs,
                                             unhealthy = unhealthy,
                                             similar_media_id = similar_media_id,
                                             sort = sort))
    
               # all media grabbed? (or run into error)
               if (nrow(media)==1 & is.na(media[1]$media_id)) break
               
               # add media to list of media
               if (n_calls==0){
                    all_media <- rbindlist(list(all_media[-1],
                                                  media),
                                             use.names = TRUE,
                                             fill = TRUE)
               } else {
                    all_media <- rbindlist(list(all_media,
                                                  media),
                                             use.names = TRUE,
                                             fill = TRUE)
               }

               # stop early if there is a restriction on total number of calls
               n_calls <- n_calls + 1
               if (!is.null(max_calls)){
                    if (n_calls==max_calls) break
               }
               
               # update for next batch
               start <- media[nrow(media)]$media_id
               
          }
          
       return(all_media)
     }


