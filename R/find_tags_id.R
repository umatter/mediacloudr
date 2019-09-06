##' Find Tags ID Given A Tag Sets ID
##' 
##' The function finds the tags_id given the name of a tag and a tag_sets_id.
##' @usage find_tags_id(tag_name, tag_sets_id, last_tags_id = 0, rows = 20)
##' @param tag_name character, the name of the tag for which the id should be found.
##' @param tag_sets_id character, the tag sets id of the tag sets to search in.
##' @param last_tags_id integer, Return tags with a tags_id is greater than this value (default = 0)
##' @param rows integer, number of tag sets to return. Cannot be larger than 100 (default = 20)
##' @return ...
##' @details See the example "Find the tags_id for 'odd' given the tag_sets_id" in the official API documentation. 
##' This is an R implementation of essentially the same function.
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md\cr 
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # 
##' \dontrun{tagsids <- find_tags_id(tag_name = "newspapers", tag_sets_id = 5, rows = 2)}
##' \dontrun{tagsids}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


find_tags_id <-
     function(tag_name, tag_sets_id, last_tags_id = 0, rows = 20) {
       
       # get all tags
       tags <- tags_list(last_tags_id = last_tags_id,
                         tag_sets_id = tag_sets_id,
                         rows = rows)
       
       if (length(tags)==0){
         stop("No tags found\n")
       }
       
       # find the tag id
       tagsids <- tags$tags_id[tags$tag==tag_name]
       return(tagsids)
     }


