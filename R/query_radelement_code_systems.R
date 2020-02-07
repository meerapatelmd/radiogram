#' GET all RadElement Codes
#' @import httr
#' @import jsonlite
#' @import purrr
#' @import rubix
#' @import dplyr
#' @export

query_radelement_codes <-
        function() {

                resp <- httr::GET("https://phpapi.rsna.org/radelement/public/v1/codes")

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
                purrr::transpose(parsed) %>%
                        purrr::map(function(x) as.data.frame(x)) %>%
                        dplyr::bind_rows() %>%
                        dplyr::rename_all(stringr::str_remove_all, "^data[.]{1}") %>%
                        rubix::cleanup_colnames()

                structure(
                        list(
                                content = payload,
                                response = resp
                        ),
                        class = "rsna_api"
                )

        }



