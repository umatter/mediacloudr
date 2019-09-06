##' Return List of Themes
##' 
##' Fetch theme list from https://mediacloud.org/s/nyt-theme-tags.csv as data table.
##' @usage get_themes_list()
##' @return a data.table object
##' @details  ...
##' @references https://mediacloud.org/support/theme-list
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # https://api.mediacloud.org/api/v2/media/list?last_media_id=1&rows=2
##' 
##' \dontrun{get_themes_list()}
##' @export
##' @import data.table


get_themes_list <-
     function() {
       return(fread("https://mediacloud.org/s/nyt-theme-tags.csv"))
     }


