% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tags_list.R
\name{tags_list}
\alias{tags_list}
\title{Return Multiple Tags}
\usage{
tags_list(last_tag_sets_id = 0, tag_sets_id=NULL, rows = 20, public = NULL, search = NULL, similar_tags_id = NULL)
}
\arguments{
\item{last_tags_id}{integer, Return tags with a tags_id is greater than this value (default = 0)}

\item{tag_sets_id}{character, Return tags belonging to the given tag sets. The most useful tag set is tag set 5.
Can be passed multiple times to return any tag belonging to any of the tag sets.(default = NULL)}

\item{rows}{integer, number of tag sets to return. Cannot be larger than 100 (default = 20)}

\item{public}{integer, If public=1, return only public tags (default = NULL)}

\item{search}{integer, see details (default = NULL)}

\item{similar_tags_id}{character, return list of tags with a similar id (default = NULL)}
}
\value{
...
}
\description{
Fetch data from the api/v2/tags/list method of the MediaCloud API.
}
\details{
If the search parameter is set, the call will return only tags that match a case insensitive search for the given text. 
The search includes the tag and label fields of the tags plus the names and label fields of the associated tag sets. So a search for
 'politics' will match tags whose tag or label field includes 'politics' and also tags belonging to a tag set whose name or label field includes 'politics'. If the search parameter has less than three characters, an empty result set will be returned.
}
\examples{
# First, make sure your personal Mediacloud API key is saved as character string in the mc.key variable:
\dontrun{mc.key <- "yourkey"}
# https://api.mediacloud.org/api/v2/tags/list?rows=2&tag_sets_id=5&last_tags_id=8875026
\dontrun{tagslist <- tags_list(rows = 2, tag_sets_id=5, last_tags_id=8875026)}
\dontrun{tagslist}
}
\references{
https://github.com/berkmancenter/mediacloud/blob/master/doc/api_2_0_spec/api_2_0_spec.md#apiv2stories_publiccount\cr
}
\author{
Ulrich Matter <umatter-at-protonmail.com>
}
