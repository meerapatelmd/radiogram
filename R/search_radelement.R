#' GET response to a code. This function will determine the vocabulary system automatically.
#' @param code one of RADLEX, LOINC, DICOM, SNOMEDCT
#' @param elements TRUE if elements are desired.
#' @import httr
#' @import jsonlite
#' @import purrr
#' @import rubix
#' @import centipede
#' @import dplyr
#' @export

search_radelement <-
        function(term, page) {

                resp <- httr::GET("https://phpapi.rsna.org/radelement/public/v1/elements?search",
                                  list(search = term,
                                       page = page))

                parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

                if (http_error(resp)) {
                        stop(
                                sprintf(
                                        "RSNA API request failed [%s]\n%s\n<%s>",
                                        status_code(resp),
                                        parsed$message,
                                        parsed$documentation_url
                                ),
                                call. = FALSE
                        )
                }


                structure(
                        list(
                                content = parsed$data,
                                meta = parsed$meta,
                                links = parsed$links,
                                response = resp
                        ),
                        class = "rsna_api"
                )

        }



