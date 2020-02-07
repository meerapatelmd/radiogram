#' GET all RadElement Codes
#' @param system one of RADLEX, LOINC, DICOM, SNOMEDCT
#' @import httr
#' @import jsonlite
#' @import purrr
#' @import rubix
#' @import dplyr
#' @export

query_radelement_codes <-
        function(system = c("RADLEX", "LOINC", "DICOM", "SNOMEDCT"),
                 page = 1) {

                url <- paste0("https://phpapi.rsna.org/radelement/public/v1/codes/", path = system)

                resp <- httr::GET(url,
                                  query = list(page = page))

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

                payload <-
                parsed$data %>%
                        purrr::map(function(x) as.data.frame(x)) %>%
                        dplyr::bind_rows() %>%
                        rubix::cleanup_colnames()

                structure(
                        list(
                                content = payload,
                                response = resp
                        ),
                        class = "rsna_api"
                )

        }



