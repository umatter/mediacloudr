##' Return Multiple Tags
##' 
##' Fetch data from the api/v2/tags/list method of the MediaCloud API.
##' @usage tags_list(last_tag_sets_id = 0, tag_sets_id=NULL, rows = 20, public = NULL, search = NULL, similar_tags_id = NULL)
##' @param last_tags_id integer, Return tags with a tags_id is greater than this value (default = 0)
##' @param tag_sets_id character, Return tags belonging to the given tag sets. The most useful tag set is tag set 5.
##'  Can be passed multiple times to return any tag belonging to any of the tag sets.(default = NULL)
##' @param rows integer, number of tag sets to return. Cannot be larger than 100 (default = 20)
##' @param public integer, If public=1, return only public tags (default = NULL)
##' @param search integer, see details (default = NULL)
##' @param similar_tags_id character, return list of tags with a similar id (default = NULL)
##' @return ...
##' @details If the search parameter is set, the call will return only tags that match a case insensitive search for the given text. 
##' The search includes the tag and label fields of the tags plus the names and label fields of the associated tag sets. So a search for
##'  'politics' will match tags whose tag or label field includes 'politics' and also tags belonging to a tag set whose name or label field includes 'politics'. If the search parameter has less than three characters, an empty result set will be returned.
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2stories_publiccount\cr 
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # https://api.mediacloud.org/api/v2/tags/list?rows=2&tag_sets_id=5&last_tags_id=8875026
##' \dontrun{tagslist <- tags_list(rows = 2, tag_sets_id=5, last_tags_id=8875026)}
##' \dontrun{tagslist}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


tags_list <-
     function(last_tags_id = 0, tag_sets_id=NULL, rows = 20,
              public = NULL, search = NULL, similar_tags_id = NULL) {

       # parse dummy url
       REQUEST_URL <-  parse_url("https://api.mediacloud.org/api/v2/tags/list?rows=20&tag_sets_id=NULL&public=NULL&search=NULL&similar_tags_id=NULL&last_tags_id=0&key=NA")
       # define and build url for API call
       REQUEST_URL$query$last_tags_id <- last_tags_id
       REQUEST_URL$query$rows <- rows
       REQUEST_URL$query$tag_sets_id <- tag_sets_id
       REQUEST_URL$query$public <- public
       REQUEST_URL$query$search <- search
       REQUEST_URL$similar_tags_id <- similar_tags_id
       REQUEST_URL$query$key <- mc.key # must be stored in global environment
       
       
       # fetch the data
       url <- build_url(REQUEST_URL)
       data <- try(fromJSON(url))
       if (class(data)[1] == "try-error") {
         tagslist <- data.frame(description="NA",
                                is_static = NA,
                             label="NA",
                             show_on_media = NA,
                             show_on_stories = NA,
                             tag = "NA",
                             tag_set_description = "NA",
                             tag_set_label = "NA",
                             tag_set_name = "NA",
                             tag_sets_id = tag_sets_id,
                             tags_id = as.integer(NA),
                             last_tags_id = last_tags_id,
                             rows = rows)
       } else {
         
         # check response format output
         if (length(data) > 0) {
           # extract data
           tagslist <- data
           tagslist$last_tags_id <- last_tags_id
           tagslist$rows <- rows

         } else {
           # no results found, return dummy df
           message("API returned empty response...\n")
           tagslist <- data.frame(description="NA",
                                  is_static = NA,
                                  label="NA",
                                  show_on_media = NA,
                                  show_on_stories = NA,
                                  tag = "NA",
                                  tag_set_description = "NA",
                                  tag_set_label = "NA",
                                  tag_set_name = "NA",
                                  tag_sets_id = tag_sets_id,
                                  tags_id = as.integer(NA),
                                  last_tags_id = last_tags_id,
                                  rows = rows)
         }

       }
       
       return(tagslist)
     }


