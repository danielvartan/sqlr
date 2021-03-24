# Source the file before running the functions

# library(checkmate)
# library(stringr)
# library(usethis)

tags <- function(write = FALSE) {
    pubmed <- list(
        "all fields" = "[ALL]",
        "author" = "[Author]",
        "author - corporate" = "[Author - Corporate]",
        "author - first" = "[Author - First]",
        "author - identifier" = "[Author - Identifier]",
        "author - last" = "[Author - Last]",
        "book" = "[Book]",
        "conflict of interest statements" = "[Conflict of Interest Statements]",
        "ec/rn number" = "[EC/RN Number]",
        "editor" = "[Editor]",
        "filter" = "[Filter]",
        "grant number" = "[Grant Number]",
        "isbn" = "[ISBN]",
        "investigator" = "[Investigator]",
        "issue" = "[Issue]",
        "journal" = "[Journal]",
        "language" = "[Language]",
        "location id" = "[Location ID]",
        "mesh major topic" = "[MeSH Major Topic]",
        "mesh subheading" = "[MeSH Subheading]",
        "mesh terms" = "[MeSH Terms]",
        "other term" = "[Other Term]",
        "pagination" = "[Pagination]",
        "pharmacological action" = "[Pharmacological Action]",
        "publication type" = "[Publication Type]",
        "publisher" = "[Publisher]",
        "secondary source id" = "[Secondary Source ID]",
        "subject - personal name" = "[Subject - Personal Name]",
        "supplementary concept" = "[Supplementary Concept]",
        "text word" = "[Text Word]",
        "title" = "[Title]",
        "title/abstract" = "[Title/Abstract]",
        "abstract" = "[Title/Abstract]",
        "keyword" = "[Title/Abstract]",
        "keywords" = "[Title/Abstract]",
        "transliterated title" = "[Transliterated Title]",
        "volume" = "[Volume]"
    )

    embase <- list(
        "abbreviated journal title" = "ta",
        "abstract or citation" = "ac",
        "abstract" = "ab",
        "accession number" = "an",
        "affiliation" = "ff",
        "article title" = "ti",
        "author address" = "ad",
        "author email" = "em",
        "author keywords" = "kw",
        "author name" = "au",
        "author's first name" = "af",
        "cas registry number" = "rn",
        "clinical trial number" = "cn",
        "coden" = "cd",
        "conference date" = "dc",
        "conference editor" = "ed",
        "conference location" = "lc",
        "conference name" = "nc",
        "country of author" = "ca",
        "country of journal" = "cy",
        "device manufacturer" = "df",
        "device trade name" = "dn",
        "digital object identifier (doi)" = "do",
        "drug manufacturer" = "mn",
        "drug trade name" = "tn",
        "embase classification" = "cl",
        "index term" = "de",
        "issn" = "is",
        "issue" = "ip",
        "keyword" = "kw",
        "keywords" = "kw",
        "language of article" = "la",
        "language of summary" = "ls",
        "medline pmid" = "ui",
        "molecular sequence number" = "ms",
        "orcid" = "oc",
        "original non-english abstract" = "oa",
        "original non-english author keywords" = "ok",
        "original non-english title" = "tt",
        "page range" = "pg",
        "publication date" = "pd",
        "publication type" = "it",
        "publication year" = "py",
        "source title" = "jt",
        "source type" = "pt",
        "start page" = "sp",
        "subheading" = "lnk",
        "title" = "ti",
        "volume" = "vi"
    )

    ebsco <- list(
        "abstract" = "AB",
        "acession number" = "AN",
        "age group" = "AG",
        "all text" = "TX",
        "author affiliation" = "AF",
        "author" = "AU",
        "chochrane AN" = "CH",
        "commentary" = "CR",
        "corporate author" = "CA",
        "dissertation number" = "DN",
        "entry date" = "EM",
        "exact major subject heading" = "MM",
        "exact minor subject heading" = "DH",
        "exact subject heading" = "MH",
        "gender" = "CT",
        "grant information" = "GI",
        "instrumentation" = "IN",
        "isbn" = "IB",
        "issn" = "IS",
        "issue" = "IP",
        "journal subset" = "SB",
        "journal title abbreviation" = "JT",
        "language" = "LA",
        "legal" = "LE",
        "medline pmid" = "PM",
        "name" = "NM",
        "named person" = "NP",
        "notes" = "NT",
        "number of pages" = "PG",
        "original study" = "OS",
        "pagination" = "PP",
        "publication [exact]" = "JN",
        "publication date" = "DT",
        "publication name" = "SO",
        "publication type" = "PT",
        "publisher" = "PB",
        "report number" = "RP",
        "series title" = "SE",
        "special interest" = "SC",
        "start page" = "SP",
        "subject" = "SU",
        "supplement title" = "IR",
        "table of contents" = "TC",
        "title" = "TI",
        "volume" = "VI",
        "word in major subject heading" = "MJ",
        "word in subject heading" = "MW",
        "year of publication" = "PY"
    )

    lilacs <- list(
        "abstract" = "ab",
        "author" = "au",
        "subject descriptor" = "mh",
        "title" = "ti"
    )

    scielo <- list(
        "abstract" = "ab",
        "author" = "au",
        "journal" = "ta",
        "publication year" = "year_cluster",
        "sponsor" = "sponsor",
        "title" = "ti"
    )

    export <- stringr::str_subset(ls(), "write", negate = TRUE)

    if (isTRUE(write)) {
        if(!(dir.exists("./data/"))) dir.create("./data/")

        tags <- mget(export)

        usethis::use_data(tags, overwrite = TRUE)

        # for (i in export) {
        #     file <- paste0("./data/", i, ".rda")
        #     save(list = i, file = file, envir = environment(),
        #          compress = "bzip2", version = 2)
        # }
    }

    invisible(mget(export))
}

# tags <- tags()
