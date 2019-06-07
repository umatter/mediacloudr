##' Return All Counts of counts Associated with a Media Tag
##' 
##' Fetch data on all story counts corresponding to a given media tag via the api/v2/counts_public/count method of the MediaCloud API.
##' @usage grab_storiescount_mediatag(tags_id_media, fq = NULL, split = NULL, split_period = "day")
##' @param tags_id_media character, the media tag id of the collection of outlets for which all counts shall be fetched.
##' @param fq ("filter query") parameter which the API method directly passes to Solr (default = NULL)
##' @param split if set to 1 or true, split the counts into date ranges (default = NULL)
##' @param split_period character, indicating for which date periods counts should be returned: day, week, month, year (default = "day")
##' @param max_calls integer, number of total calls to the API. If NULL, no restrictions are applied, all counts will be collected. (default=NULL)
##' @return ...
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2counts_publiccount\cr 
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # fetch count data on stories about US politics in the us mainstream media for the time frame 2014-01-01T00:00:00Z TO 2014-03-01T00:00:00Z
##' \dontrun{storiescount <- grab_storiescount_mediatag(tags_id_media="8875027", fq="tags_id_stories:9360846 AND publish_day:[2014-01-01T00:00:00Z TO 2014-03-01T00:00:00Z]")}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite
grab_storiescount_mediatag <-
     function (tags_id_media, fq = NULL, split = 0, split_period = "day") {
          
          # first, collect all media outlets corresponding with the collection
          # it is necessary to iterate over media ids because the counts_public/count
          # method does not return information about the media (only counts per timeframe)
          message("Get media ids...\n")
          media_outlets <- media_list(rows = 100,
                                      tags_id = tags_id_media) # improve this to ensure getting all media associated with this tag!
          
          # prepare iteration
          n_calls <- 0
          media_ids <- unique(media_outlets$media_id)
          n_outlets <- length(media_ids)
          outlets_queried <- 0
          q_basic <- paste0("tags_id_media:", tags_id_media)
          fq_char <- ifelse(is.null(fq), "NA", fq)
          all_counts <- data.table(count=as.integer(NA),
                              date="NA",
                              split_period = "NA",
                              media_id = "NA",
                              tags_id_media="NA",
                              fq=fq_char)
          
          message("Get counts for each media id...\n")
          # start stream of api calls and responses
          while (TRUE) {
               
               # prepare query
               media_id <- media_ids[1]
               q <- paste0(q_basic, " AND media_id:", media_id)
               
               # query one batch at a time
              counts <- stories_public_count(q = q,
                                             fq = fq,
                                             split = split,
                                             split_period = split_period)
              counts$media_id <- media_id
              counts$tags_id_media <- tags_id_media
              counts$fq <- fq_char
    
               # all counts grabbed? (or run into error)
               if (length(counts)==0) break
              
               
               # add counts to list of counts
              counts$media_id <- media_id
               if (outlets_queried==0){
                    all_counts <- rbindlist(list(all_counts[-1],
                                                  counts),
                                             use.names = TRUE,
                                             fill = TRUE)
               } else {
                    all_counts <- rbindlist(list(all_counts,
                                                  counts),
                                             use.names = TRUE,
                                             fill = TRUE)
               }

               # stop early if there is a restriction on total number of calls
               # if (!is.null(max_calls)){
               #      n_calls <- n_calls + 1
               #      if (n_calls==max_calls) break
               # }
               
               # update for next batch or exit loop if done
               outlets_queried <- outlets_queried + 1
               if (n_outlets==outlets_queried) break
               media_ids <- media_ids[-1]
          }
          
          # merge media meta information with counts
          all_counts$media_id <- as.character(all_counts$media_id)
          media_outlets$media_id <- as.character(media_outlets$media_id)
          all_counts2 <- merge(all_counts, media_outlets,
                              by="media_id",
                              all = TRUE)

       return(all_counts2)
     }


