##' Sentences count
##' 
##' Fetch data from the api/v2/sentences/count method of the MediaCloud API.
##' @usage sentences_count(q, fq = NULL, split = NULL, split_start_date = NULL, split_end_date = NULL)
##' @param q ("query") parameter which the API method directly passes to Solr
##' @param fq ("filter query") parameter which the API method directly passes to Solr (default = NULL)
##' @param split if set to 1 or true, split the counts into date ranges (default = NULL)
##' @param split_start_date date on which to start date splits, in YYYY-MM-DD format
##' @param split_end_date date on which to end date splits, in YYYY-MM-DD format
##' @return A data frame with a row for each candidate and columns with the following variables describing the candidate:\cr addlBio.candidate.shortTitle,\cr addlBio.candidate.firstName,\cr addlBio.candidate.nickName,\cr addlBio.candidate.middleName,\cr addlBio.candidate.lastName,\cr addlBio.candidate.suffix,\cr addlBio.additional.item*.name,\cr addlBio.additional.item*.data
##' @references https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#sentences\cr 
##' @author Ulrich Matter <umatter-at-protonmail.com>
##' @examples
##' # First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
##' \dontrun{mc.key <- "yourkey"}
##' # fetch count of sentences containing the word 'obama' in the New York Times 
##' \dontrun{obama <- sentences_count(q="sentence:obama", fq="media_id=1")}
##' \dontrun{obama}
##' # fetch count of sentences containing 'africa' in the U.S. Mainstream Media from 2014-01-01 to 2014-03-01:
##' \dontrun{africa <- sentences_count(q="sentence:africa+AND+tags_id_media:8875027", split=1, split_start_date="2014-01-01", split_end_date="2014-03-01")}
##' \dontrun{africa}
##' @export


sentences_count <-
     function (q, fq = NULL, split = 0, split_start_date = NULL, split_end_date = NULL) {
          
          # check if arguments are valid for API method
          stopifnot(is.character(q))
          if (!is.null(split_start_date)) {
               if ((split == 1 | split == "true") & (!is.character(split_start_date) | !is.character(split_end_date)) ) {
                    stop("No valid start and end date provided!")
               }
               if ((split == 0 | split == "false") & (is.character(split_start_date) | is.character(split_end_date)) ) {
                    warning("Ignoring start and end date (split==0)!")
               }
               if ((split == 1 | split == "true") &
                   (is.character(split_start_date) & is.character(split_end_date)) &
                   (split_start_date == split_end_date)) {
                    warning("Same start_date and end_date as input, extending end_date by one day.")
                    split_end_date <- as.character(as.Date(split_end_date) + 1)
               }
          }

          
          # define fix parameters for this wrapper
          BASE_URL <- "https://api.mediacloud.org/api/v2/"
          METHOD <- "sentences/count?"
          PARAMS <- c("q=", "&fq=", "&split=", "&split_start_date=", "&split_end_date=", "&key=")
          META <- c("start", "end", "gap")
          
          # get url for this request
          input <- list(q=q, 
                        fq=fq, 
                        split=split, 
                        split_start_date=split_start_date,
                        split_end_date=split_end_date,
                        key=mc.key)
          input_complete <- unlist(input[!sapply(input, is.null)])
          params_complete <- PARAMS[!sapply(input, is.null)] 
          query <- paste0(params_complete, input_complete, collapse = "")
          URL <- paste0(BASE_URL, METHOD, query)
          
          # fetch the data
          data <- try(fromJSON(URL))
          if (class(data)[1] == "try-error") {
               data <- data.table(count = NA)
          }
          
          # check response format output
          if (length(data) > 1) {
               # metadata
               count <- data["count"]
               meta <- as.data.table(c(count, data$split[META]))
               setnames(meta, old="count", new = "total_count")
               # detailed data
               split <- data$split[!(names(data$split) %in% META)]
               # in case of only one day, everything is in the meta-part, thus return empty dt
               if (length(split) == 0) {
                     counts <- data.table()
               } else {
                     counts <- melt(as.data.table(split), measure.vars = names(split))
                     setnames(counts, old = c("variable", "value"), new = c("date", "count"))
               }
               # put it all together
               data <- list(metadata = meta, counts = counts)
          } else {
               data <- as.data.table(data)
               setnames(data, old = "count", new = "total_count")
          }
          
          return(data)
     }

sentences_count_all <- 
     function (q, fq = NULL, split = 0, split_start_date = NULL, split_end_date = NULL) {
          
          # check if arguments are valid for API method
          stopifnot(is.character(q))
          if (!is.null(split)) {
               if ((split == 1 | split == "true") & (!is.character(split_start_date) | !is.character(split_end_date)) ) {
                    stop("No valid start and end date provided!")
               }
               if ((split == 0 | split == "false") & (is.character(split_start_date) | is.character(split_end_date)) ) {
                    warning("Ignoring start and end date (split==0)!")
               }
          }

          # vectorize over respective argument
          # if no date-splitting specified
          if (split == 0 | split == "false") {
               # vectorize over q
               vsent_count <- Vectorize(sentences_count, vectorize.args = "q", SIMPLIFY = FALSE)
               vres <- vsent_count(q, fq, split, split_start_date, split_end_date)
               output <- rbindlist(vres, fill = TRUE)
               output$q <- q
               
          } else {
               # vectorize over q and loop over dates
               all_dates <- cbind(split_start_date, split_end_date)
               n_dates <- nrow(all_dates)
               vsent_count <- Vectorize(sentences_count, vectorize.args = "q", SIMPLIFY = FALSE)
               all_counts <- list()
               length(all_counts) <- n_dates
               all_meta <- list()
               length(all_meta) <- n_dates
               for (i in 1:n_dates) {
                    start.i <- all_dates[i,1]
                    end.i <- all_dates[i,2]
                    cat(paste0("Fetching counts from ", start.i, " to ", end.i, " ...\n"))
                    
                    # fetch data (vectorized over q)
                    vres <- vsent_count(q, fq, split, start.i, end.i)
                    # separate results
                    # metadata
                    meta_list <- lapply(vres, function(x) x[["metadata"]])
                    meta <- rbindlist(meta_list, fill = TRUE)
                    meta$q <- q
                    all_meta[[i]] <- meta
                    # counts per date
                    counts_list <- lapply(vres, function(x) x[["counts"]])
                    counts <- rbindlist(counts_list, fill = TRUE)
                    counts$q <- q
                    all_counts[[i]] <- counts
               }
               
               # combine all results in one data.table
               counts <- rbindlist(all_counts, fill = TRUE)
               meta <- rbindlist(all_meta, fill = TRUE)
               output <- list(meta, counts)
          }
          return(output)
     }



          
          
          
