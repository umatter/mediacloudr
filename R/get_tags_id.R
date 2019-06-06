##' Get Tag IDs 
##' 
##' The function returns specific tag ids and tag names from the nyt-theme tag list provided by MediaCloud
##' @usage get_tags_id(tag_name, exact=FALSE, source="local")
##' @param tag_name character, the name of the tag for which the id should be found.
##' @param fuzzy logical, if TRUE, only exact matches are returned. If FALSE tab_name is considered a regular expression. (default = FALSE)
##' @param source character, either "local" or "online". if "online" the tags list is downloaded from mediacloud when this function is called.
##' if "local" a local copy of the list (might not be up to date) shipped with mediacloudr is used. (default = "local")
##' @return a data frame containing the found tag names and tag ids
##' @details ...
##' @references https://mediacloud.org/support/theme-list
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # 
##' \dontrun{govtag_id <- get_tags_id(tag_name = "united states politics")}
##' \dontrun{govtag_id}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


get_tags_id <-
     function(tag_name, exact=FALSE, source="local") {
          
       # load the tags ids
          if (source=="local"){
               tags <- read.csv(system.file("exdata", "nyt-theme-tags.csv", package = "mediacloudr"))
          } else {
              tags <- read.csv("https://mediacloud.org/s/nyt-theme-tags.csv") 
          }
       
       # find the tag ids
          if (exact){
               tagsids <- tags[tags$tag==tag_name,]
               
          } else {
               tagsids <- tags[grepl(tag_name, tags$tag),]
          }
          
       return(tagsids)
     }


