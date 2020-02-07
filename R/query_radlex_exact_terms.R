#' Query based on search terms that does not write to catalogue
#' @param terms vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @importFrom rubix call_mr_clean
#' @import dplyr
#' @export

query_radlex_exact_terms <-
        function(terms, limit = NULL, concept_name_table = "public.v4_concept_name") {
                Args <- as.list(terms)
                if (is.null(limit)) {
                        for (i in 1:length(Args)) {
                                if (i == 1) {
                                        output <- list()
                                        sql_statement <- paste0("SELECT * FROM public.v4_concept_name WHERE concept_name = '", Args[[1]], "'")
                                } else {
                                        sql_statement <- paste0("SELECT * FROM public.v4_concept_name WHERE concept_name = '", Args[[i]], "'")
                                }
                                sql_statement <- paste0(sql_statement, ";")
                                conn <- mySeagull::connect_to_local_postgres(dbname = "radlex")
                                output[[i]] <- DBI::dbGetQuery(conn = conn,
                                                               statement = sql_statement)
                                DBI::dbDisconnect(conn)

                        }
                } else {
                        for (i in 1:length(Args)) {
                                if (i == 1) {
                                        sql_statement <- paste0("SELECT * FROM public.v4_concept_name WHERE concept_name = '", Args[[1]], "'")
                                } else {
                                        sql_statement <- paste0("SELECT * FROM public.v4_concept_name WHERE concept_name = '", Args[[i]], "'")
                                }

                                sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                                conn <- mySeagull::connect_to_local_postgres(dbname = "radlex")
                                output[[i]] <- DBI::dbGetQuery(conn = conn,
                                                               statement = sql_statement)
                                DBI::dbDisconnect(conn)
                        }

                }

                names(output) <- unlist(Args)
                return(output)

        }
