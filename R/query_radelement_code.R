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

query_radelement_code <-
        function(code, values = FALSE, elements = TRUE, page = 1) {

                if (centipede::nchar_letter(code) == 0) {
                        system <- "SNOMEDCT"
                } else if (grepl("RID", code) == TRUE) {
                        system <- "RADLEX"
                } else {
                        system <- "LOINC"
                }

                if (elements == TRUE) {
                        url <- paste0("https://phpapi.rsna.org/radelement/public/v1/codes/", system, "/", code)
                        resp <- httr::GET(url)
                } else if (values == TRUE) {
                        url <- paste0("https://phpapi.rsna.org/radelement/public/v1/codes/", system, "/", code, "/values")
                        resp <- httr::GET(url,
                                          query = list(page = page))
                } else {
                        url <- paste0("https://phpapi.rsna.org/radelement/public/v1/codes/", system, "/", code)
                        resp <- httr::GET(url,
                                          query = list(page = page))
                }

                parsed <<- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

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

                payload <-
                parsed$data %>%
                        purrr::map(function(x) t(as.data.frame(x))) %>%
                        dplyr::bind_rows() %>%
                        rubix::cleanup_colnames()

                structure(
                        list(
                                content = payload,
                                meta = parsed$meta,
                                links = parsed$links,
                                response = resp
                        ),
                        class = "rsna_api"
                )

        }



