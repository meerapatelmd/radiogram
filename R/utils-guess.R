





guess_vocabulary <-
        function(code) {
                if (centipede::nchar_letter(code) == 0) {
                        system <- "SNOMEDCT"
                } else if (grepl("RID", code) == TRUE) {
                        system <- "RADLEX"
                } else {
                        system <- "LOINC"
                }
        }
