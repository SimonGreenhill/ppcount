
logger::log_threshold('ERROR')  # turn off log output


taxa <- read.csv(text="
ID,Name,Glottocode,Level,Language_ID,TestComment
1,abaga,abaga,abag1245,language,,Should be kept
2,aeka,aeka,aeka1244,dialect,musa1266,should be removed as aisi is language level example
3,aghu,aghu,,language,,should be removed as unknown glottocode
4,aisi,aisi,musa1266,language,,Should be kept
5,alekano,alekano,alek1238,language,,Should be kept (not deleted with alekano2)
6,alekano2,alekano2,alek1238,language,,Should be removed in favor of alekano2
7,enga-kaina,kaina,kain1269,dialect,enga1252,Should be promoted as representative of Enga
")

test_that("test get_non_languages", {
    # 1 alekano2 alekano2   alek1238   Duplicate Glottocode
    # 2     aeka     aeka   aeka1244 Invalid Level: dialect
    # 3     aghu     aghu       <NA>       Unknown language
    to_remove <- get_non_languages(taxa)
    expect_equal(nrow(to_remove), 3)

    expect_true('alekano2' %in% to_remove$ID)
    expect_true('aghu' %in% to_remove$ID)
    expect_true('aeka' %in% to_remove$ID)

    expect_equal(
        to_remove[to_remove$ID == 'alekano2', ]$Reason,
        'Duplicate Glottocode'
    )

    expect_equal(
        to_remove[to_remove$ID == 'aeka', ]$Reason,
        'Invalid Level: dialect'
    )

    expect_equal(
        to_remove[to_remove$ID == 'aghu', ]$Reason,
        'Unknown language'
    )
})


test_that("test get_non_languages deals with empty Glottocode correctly", {
    to_remove <- get_non_languages(data.frame(
        ID=1, Name='a', Glottocode=NA, Level='Family', Language_ID=1
    ))
    expect_equal(nrow(to_remove), 2)
    expect_equal(
        sort(to_remove$Reason),
        c('Invalid Level: Family', 'Unknown language'))
})


test_that("test get_non_languages deals with NA level correctly", {
    to_remove <- get_non_languages(data.frame(
        ID=1, Name='a', Glottocode='aaaa1234', Level=NA, Language_ID=1
    ))
    expect_equal(nrow(to_remove), 1)
    expect_equal(to_remove$Reason, c('Invalid Level: NA'))
})


honkola <- structure(
    list(ID = c("Selkup", "Nenets", "Hungarian", "Mansi", "Khanty", "Udmurt", "Komi", "Mari", "Mordvin", "Northern_Sami",  "Ume_Sami", "Skolt_Sami", "Estonian", "Livonian", "Karelian", "Veps", "Finnish"),
         Name = c("Selkup", "Nenets", "Hungarian", "Mansi", "Khanty", "Udmurt", "Komi", "Mari", "Mordvin", "Northern_Sami", "Ume_Sami", "Skolt_Sami", "Estonian", "Livonian", "Karelian", "Veps", "Finnish"),
         Glottocode = c("selk1253", "nene1249", "hung1274", "mans1258", "fare1244", "udmu1245", "komi1269", "east2328", "erzy1239", "nort2671", "umes1235", "skol1241", "esto1258", "livv1244", "kare1335", "veps1250", "finn1318"),
         Level = c("language", "language", "language", "language", "language", "language", "language", "language", "language", "language", "language", "language", "language", "language", "language", "language", "language"),
         Language_ID = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_
    )), row.names = c(NA, -17L), class = c("tbl_df", "tbl", "data.frame"))


# this dataset gives these log notices -- because apply works on empty tibbles. Cool.
# INFO [2025-03-19 14:00:02] Promoting NA to language NA
# INFO [2025-03-19 14:00:02] Removing NA [NA] because: NA
test_that("test get_non_languages - honkola et al ", {
    to_remove <- get_non_languages(honkola)
    expect_equal(nrow(to_remove), 0)
})
