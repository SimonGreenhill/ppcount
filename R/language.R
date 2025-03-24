#' Functions to operate on language data

#' Returns a dataframe of taxa to remove
#'
#' Dialect handling:
#'   - go over dialects,
#'   - find the ID of the parent language of each dialect (=Language_ID)
#'   - if we do not have any other entries in the taxa table for that Language_ID
#'     (i.e. no other samples of that language), we 'promote' the dialect to a
#'     language.
#'   - otherwise we remove dialects
#' @param taxa A dataframe with columns ID, Name, Glottocode, Level
#' @returns A dataframe of (ID, Name, Glottocode, Reason)
#' @export
#' @examples
#' taxa <- data.frame(
#'     ID=c("abaga", "aeka"),
#'     Name=c("abaga", "aeka"),
#'     Glottocode=c("abag1245", "aeka1244"),
#'     Level=c("language", "dialect"),
#'     Language_ID=c("", "musa1266"))
#' get_non_languages(taxa)
get_non_languages <- function(taxa) {
    # check required colnames
    for (req in c('ID', 'Name', 'Glottocode', 'Level')) {
        if (req %in% colnames(taxa) == FALSE) { stop(paste("Missing expected column: ", req)) }
    }

    # Remove taxa if we don't know what "official" language they belong to
    to_remove <- taxa |>
        dplyr::filter(Glottocode %in% c(NA, "")) |>
        dplyr::mutate(Reason="Unknown language") |>
        dplyr::select("ID", "Name", "Glottocode", "Reason")

    # go over dialects,
    # find the ID of the parent language of each dialect (=Language_ID)
    # if we do not have any other entries in the taxa table for that Language_ID
    # (i.e. no other samples of that language), we 'promote' the dialect to a
    # language.
    language_glottocodes <- taxa |>
        dplyr::filter(Level == 'language') |>
        dplyr::filter(Glottocode %in% c(NA, "") == FALSE) |>
        dplyr::pull("Glottocode") |>
        unique()

    promoted <- taxa |>
        dplyr::filter(Level == 'dialect') |> # get dialects
        dplyr::filter(Language_ID %in% language_glottocodes == FALSE) |>  # remove ones that already have language level counterparts
        dplyr::mutate(Glottocode=Language_ID, Level="language")  # update the rest

    # update the promoted dialects -> languages and log
    if (nrow(promoted) > 0) {
        apply(promoted, 1, function(row) {
            logger::log_info("Promoting {row['ID']} to language {row['Glottocode']}")
        })
        taxa <- dplyr::rows_update(taxa, promoted, by=c("ID"))
    }

    # Remove taxa that are tagged as "dialects" in glottolog but have not been promoted
    to_remove <- taxa |>
        dplyr::filter(Level != 'language' | is.na(Level)) |>
        dplyr::mutate(Reason=paste("Invalid Level:", Level)) |>
        dplyr::select("ID", "Name", "Glottocode", "Reason") |>
        rbind(to_remove)

    # handle duplicates
    uniques <- taxa |>
        dplyr::select("ID", "Glottocode") |>
        dplyr::group_by(Glottocode) |>
        dplyr::slice_head(n=1)
    to_remove <- taxa |>
        dplyr::select("ID", "Glottocode") |>
        dplyr::filter(ID %in% uniques$ID == FALSE) |>
        dplyr::mutate(Reason="Duplicate Glottocode") |>
        dplyr::left_join(taxa |> dplyr::select("ID", "Name"), dplyr::join_by(ID==ID)) |> # merge Name back
        rbind(to_remove)

    # log this
    if (nrow(to_remove) > 0) {
        apply(to_remove, 1, function(row) {
            logger::log_info("Removing {row['ID']} [{row['Glottocode']}] because: {row['Reason']}")
        })
    }
    to_remove |> dplyr::select("ID", "Name", "Glottocode", "Reason")
}

