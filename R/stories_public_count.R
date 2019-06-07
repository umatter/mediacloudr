##' Stories Public Count
##' 
##' Fetch data from the api/v2/stories_public/count method of the MediaCloud API.
##' @usage stories_public_count(q, fq = NULL, split = NULL, split_period = "day")
##' @param q ("query") parameter which the API method directly passes to Solr
##' @param fq ("filter query") parameter which the API method directly passes to Solr (default = NULL)
##' @param split if set to 1 or true, split the counts into date ranges (default = NULL)
##' @param split_period character, indicating for which date periods counts should be returned: day, week, month, year (default = "day")
##' @return ...
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2stories_publiccount\cr 
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # fetch count of stories containing the word 'obama' in the New York Times 
##' # (https://api.mediacloud.org/api/v2/stories_public/count?q=obama&fq=media_id:1)
##' \dontrun{obama <- stories_public_count(q="obama", fq="media_id=1")}
##' \dontrun{obama}
##' # fetch count of stories containing 'africa' in the New York Times for each week from 2014-01-01 to 2014-03-01:
##' \dontrun{africa <- stories_public_count(q="africa AND media_id:1", fq="publish_day:[2014-01-01T00:00:00Z TO 2014-03-01T00:00:00Z]", split=1, split_period = "week")}
##' \dontrun{africa}
##' @export
##' @import httr
##' @import data.table
##' @import jsonlite


stories_public_count <-
     function (q, fq = NULL, split = 0, split_period = "day") {
 
          
       
       
       # parse dummy url
       REQUEST_URL <-  parse_url("https://api.mediacloud.org/api/v2/stories_public/count?split=NULL&split_period=day&q=NA&fq=NULL&key=NA")
       # define and build url for API call
       REQUEST_URL$query$split <- split
       REQUEST_URL$query$split_period <- split_period
       REQUEST_URL$query$q <- q
       REQUEST_URL$query$fq <- fq
       REQUEST_URL$query$key <- mc.key # must be stored in global environment
       
       
       # fetch the data
       url <- build_url(REQUEST_URL)
       message(paste0("Query API: ", url))
       data <- try(fromJSON(url))
       if (class(data)[1] == "try-error") {
         count <- data.frame(count=as.integer(NA),
                             date="NA",
                             q = q,
                             split_period = split_period)
       } else {
         
         # check response format output
         if (length(data) > 0) {
           # extract data
           if (is.null(split) | split == 0){
             # result is count over entire period
             count <- data.frame(count = data[["count"]],
                                 date = "NA",
                                 q = q,
                                 split_period = "NA")
           } else {
             # result is counts per date period
             count <- data[["counts"]]
             count$q <- q
             count$split_period <- split_period
           }
           
         } else {
           # no results found, return dummy df
           message("API returned empty response...\n")
           count <- data.frame(count=as.integer(NA),
                               date="NA",
                               q = q,
                               fq = fq,
                               split_period = "NA" )
         }
         
         
       }
       
      
       return(count)
     }


