#' GET response to a code. This function will determine the vocabulary system automatically.
#' @description search
#'
#' @param search Query string used to search the names of data elements
#' @param page Page number to get elements for. 15 elements per page. Default 1.
#' @param sort Data to sort element on. Default name.
#' @param order Direction to sort elements on, desc or asc. Default asc
#' @export

search_cdes <-
        function(search = NULL,
                 page = 1,
                 sort = "name",
                 order = c("asc", "desc")) {

                # search = NULL,
                # page = 1,
                # sort = "name",
                # order = c("asc", "desc")


                        resp <- httr::GET("https://phpapi.rsna.org/radelement/public/v1/elements",
                                          query = list(search = search,
                                                       page = page,
                                                       sort = sort,
                                                       order = order))

                        parsed <- httr::content(x = resp,
                                                as = "text",
                                                encoding = "UTF-8")


                        if (httr::http_error(resp)) {
                                stop(
                                        sprintf(
                                                "RSNA API request failed [%s]\n%s\n<%s>",
                                                httr::status_code(resp),
                                                parsed$message,
                                                parsed$documentation_url
                                        ),
                                        call. = FALSE
                                )
                        }

                        jsonlite::fromJSON(parsed)

        }



#' @title
#' Get All Common Data Elements
#' @export

get_cdes <-
        function() {

                resp <- httr::GET("https://phpapi.rsna.org/radelement/public/v1/elements")

                parsed <- httr::content(resp, as = "text", encoding = "UTF-8")

                if (httr::http_error(resp)) {
                        stop(
                                sprintf(
                                        "RSNA API request failed [%s]\n%s\n<%s>",
                                        httr::status_code(resp),
                                        parsed$message,
                                        parsed$documentation_url
                                ),
                                call. = FALSE
                        )
                }

                jsonlite::fromJSON(parsed)

        }

