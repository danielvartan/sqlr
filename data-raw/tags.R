# Source the file before running the functions

# library(checkmate)
# library(stringr)
# library(usethis)

#' Create a list with search field tags from several databases
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tags()` creates and returns a `list` object containing lists with the
#' search field tags of several databases. This values are used by the
#' `query()` function.
#'
#' See the `query()` function documentation to get a list of the database
#' providers documentation.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `tags.rda` file to `"./data/"` (default: `FALSE`).
#'
#' @return An invisible `list` object containing lists with the search field
#'   tags of several databases.
#'
#' @family data functions
#' @export
#'
#' @examples
#' \dontrun{
#' tags()}
tags <- function(write = FALSE) {

    # See: <http://help.psycnet.org/>
    apa <- list(
        "Any Field" = "Any Field",
        "Abstract" = "Abstract",
        "Abstracts" = "Abstract", # Alias
        "Administration Time" = "Administration Time",
        "Affiliation" = "Affiliation",
        "Author" = "Author",
        "Author of Reviewed Item" = "Author of Reviewed Item",
        "Book Title" = "Book Title",
        "Conference" = "Conference",
        "Content owner" = "Content owner",
        "Correction date" = "Correction date",
        "Correspondence" = "Correspondence",
        "DOI number" = "DOI Number",
        "First Page" = "First Page",
        "Geographic Location" = "Geographic Location",
        "Grant/Sponsorship" = "Grant/Sponsorship",
        "Index Terms" = "Index Terms",
        "ISBN" = "ISBN",
        "ISSN" = "ISSN",
        "Journal Title" = "Journal Title",
        "Keywords" = "Keywords",
        "Keyword" = "Keywords", # Alias
        "Language" = "Language",
        "MeSH" = "MeSH",
        "Population" = "Population",
        "Publication Date" = "Publication Date",
        "Publisher" = "Publisher",
        "PubMed ID (PMID)" = "PubMed ID (PMID)",
        "Purpose" = "Purpose",
        "Release Date" = "Release Date",
        "Setting" = "Setting",
        "Source Document Citation" = "Source Document Citation",
        "Test Name" = "Test Name",
        "Test Construct" = "Test Construct",
        "Tests & Measures" = "Tests & Measures",
        "Therapist Name" = "Therapist Name",
        "Title" = "Title",
        "Titles" = "Title", # Alias
        "Title of Reviewed Item" = "Title of Reviewed Item",
        "Unique Identifier" = "Unique Identifier",
        "Year of Reviewed Item" = "Year of Reviewed Item",
        "Series Title" = "SE",
        "Special Interest" = "SC",
        "Start Page" = "SP",
        "Subject" = "SU",
        "Supplement Title" = "IR",
        "Table of Contents" = "TC",
        "Title" = "TI",
        "Volume" = "VI",
        "Word in Major Subject Heading" = "MJ",
        "Word in Subject Heading" = "MW",
        "Year of Publication" = "PY"
    )

    # See: <http://support.ebsco.com/help/>
    ebsco <- list(
        "Abstract" = "AB",
        "Abstracts" = "AB", # Alias
        "Acession Number" = "AN",
        "Age Group" = "AG",
        "All Text" = "TX",
        "Author Affiliation" = "AF",
        "Author" = "AU",
        "Chochrane AN" = "CH",
        "Commentary" = "CR",
        "Corporate Author" = "CA",
        "Dissertation Number" = "DN",
        "Entry Date" = "EM",
        "Exact Major Subject Heading" = "MM",
        "Exact minor subject heading" = "DH",
        "Exact Subject Heading" = "MH",
        "Gender" = "CT",
        "Grant Information" = "GI",
        "Instrumentation" = "IN",
        "ISBN" = "IB",
        "ISSN" = "IS",
        "Issue" = "IP",
        "Journal Subset" = "SB",
        "Journal Title Abbreviation" = "JT",
        "Language" = "LA",
        "Legal" = "LE",
        "Medline PMID" = "PM",
        "Name" = "NM",
        "Named Person" = "NP",
        "Notes" = "NT",
        "Number of Pages" = "PG",
        "Original Study" = "OS",
        "Pagination" = "PP",
        "Publication [Exact]" = "JN",
        "Publication Date" = "DT",
        "Publication Name" = "SO",
        "Publication Type" = "PT",
        "Publisher" = "PB",
        "Report Number" = "RP",
        "Series Title" = "SE",
        "Special Interest" = "SC",
        "Start Page" = "SP",
        "Subject" = "SU",
        "Supplement Title" = "IR",
        "Table of Contents" = "TC",
        "Title" = "TI",
        "Titles" = "TI", # Alias
        "Volume" = "VI",
        "Word in Major Subject Heading" = "MJ",
        "Word in Subject Heading" = "MW",
        "Year of Publication" = "PY"
    )

    # See: <https://bit.ly/399d14T>
    embase <- list(
        "Abbreviated journal title" = "ta",
        "Abstract or citation" = "ac",
        "Abstract" = "ab",
        "Abstracts" = "ab", # Alias
        "Accession number" = "an",
        "Affiliation" = "ff",
        "Article title" = "ti",
        "Associated PUI" = "aid",
        "Author address" = "ad",
        "Author email" = "em",
        "Author keywords" = "kw",
        "Author name" = "au",
        "Author" = "au", # Alias
        "Author's First Name" = "af",
        "Book publisher" = "bp",
        "CAS registry number" = "rn",
        "Citation" = "ct",
        "Clinical trial number" = "cn",
        "CODEN code" = "cd",
        "Conference date" = "dc",
        "Conference editor" = "ed",
        "Conference location" = "lc",
        "Conference name" = "nc",
        "Country of author" = "ca",
        "Country of journal" = "cy",
        "DB Collection" = "dtype",
        "Device manufacturer" = "df",
        "Device trade name" = "dn",
        "DOI" = "do",
        "Drug manufacturer" = "mn",
        "Drug trade name" = "tn",
        "Editor" = "ed",
        "Embase classification" = "cl",
        "Exploded terms" = "exp",
        "Index term" = "de",
        "ISBN" = "ib",
        "ISSN" = "is",
        "Issue" = "ip",
        "Keyword" = "kw", # Alias
        "Keywords" = "kw", # Alias
        "Language of article" = "la",
        "Language of summary" = "ls",
        "Link" = "lnk",
        "Luwak unique id" = "id",
        "Medline PMID" = "ui",
        "Molecular sequence number" = "ms",
        "ORCID" = "oc",
        "Original non-English abstract" = "oa",
        "Original non-English author keywords" = "ok",
        "Original non-English title" = "tt",
        "Page range" = "pg",
        "Publication date" = "pd",
        "Publication type" = "it",
        "Publication year" = "py",
        "Publisher item identifier" = "ii",
        "source title" = "jt",
        "Source type" = "pt",
        "Start page" = "sp",
        "Subheading" = "lnk",
        "Title" = "ti", # Alias
        "Titles" = "ti", # Alias
        "Volume" = "vi"
    )

    # See: <https://bvsalud.org/en/8246-2/>
    lilacs <- list(
        "Abstract" = "ab",
        "Abstracts" = "ab", # Alias
        "Author" = "au",
        "Subject descriptor" = "mh",
        "Title" = "ti",
        "Titles" = "ti" # Alias
    )

    # See: <https://pubmed.ncbi.nlm.nih.gov/help/>
    pubmed <- list(
        "All Fields" = "[ALL]",
        "Author" = "[Author]",
        "Author - Corporate" = "[Author - Corporate]",
        "Author - First" = "[Author - First]",
        "Author - Identifier" = "[Author - Identifier]",
        "Author - Last" = "[Author - Last]",
        "Book" = "[Book]",
        "Conflict of Interest Statements" = "[Conflict of Interest Statements]",
        "EC/RN Number" = "[EC/RN Number]",
        "Editor" = "[Editor]",
        "Filter" = "[Filter]",
        "Grant Number" = "[Grant Number]",
        "ISBN" = "[ISBN]",
        "Investigator" = "[Investigator]",
        "Issue" = "[Issue]",
        "Journal" = "[Journal]",
        "Language" = "[Language]",
        "Location ID" = "[Location ID]",
        "MeSH Major Topic" = "[MeSH Major Topic]",
        "MeSH Subheading" = "[MeSH Subheading]",
        "MeSH Terms" = "[MeSH Terms]",
        "Other Term" = "[Other Term]",
        "Pagination" = "[Pagination]",
        "Pharmacological Action" = "[Pharmacological Action]",
        "Publication Type" = "[Publication Type]",
        "Publisher" = "[Publisher]",
        "Secondary Source ID" = "[Secondary Source ID]",
        "Subject - Personal Name" = "[Subject - Personal Name]",
        "Supplementary Concept" = "[Supplementary Concept]",
        "Text Word" = "[Text Word]",
        "Title" = "[Title]",
        "Titles" = "[Title]", # Alias
        "Title/Abstract" = "[Title/Abstract]",
        "transliterated title" = "[Transliterated Title]",
        "volume" = "[Volume]"
    )

    # See: <https://bit.ly/3lJvVnQ>
    scielo <- list(
        "Abstract" = "ab",
        "Abstracts" = "ab", # Alias
        "Author" = "au",
        "Journal" = "ta",
        "Publication year" = "year_cluster",
        "Sponsor" = "sponsor",
        "Title" = "ti",
        "Titles" = "ti" # Alias
    )

    # See: <https://bit.ly/2QAylcS>
    scopus <- list(
        "Abstract" = "ABS",
        "Abstracts" = "ABS", # Alias
        "All Fields" = "ALL",
        "Doc Title" = "TITLE",
        "Title" = "TITLE", # Alias
        "Titles" = "TITLE", # Alias
        "Doc Title, Abstract" = "TTILE-ABS",
        "Doc Title, Abstract, Keyword" = "TITLE-ABS-KEY",
        "Doc Title, Abstract, Keyword, Author" = "TITLE-ABS-KEY-AUTH",
        "Affiliation" = "AFFIL",
        "Affiliation City" = "AFFILCITY",
        "Affiliation Country" = "AFFILCOUNTRY",
        "Affiliation ID" = "AF-ID",
        "Affiliation Organization" = "AFFILORG",
        "Author" = "AUTH",
        "Author Collaboration" = "AUTHCOLLAB",
        "Author First Initial" = "AUTHFIRST",
        "Author ID" = "AU-ID",
        "Author Last Name" = "AUTHLASTNAME",
        "Author Name" = "AUTHOR-NAME",
        "ORCID" = "ORCID",
        "Sequence Bank" = "SEQBANK",
        "Sequence Number" = "SEQNUMBER",
        "CAS Registry Number" = "CASREGNUMBER",
        "Chemical" = "CHEM",
        "Chemical Name" = "CHEMNAME",
        "Conference Information" = "CONF",
        "Conference Location" = "CONFLOC",
        "Conference Name" = "CONFNAME",
        "Conference Sponsors" = "CONFSPONSORS",
        "Open Access" = "OA",
        "Database" = "INDEX",
        "Doc Type" = "DOCTYPE",
        "Digital Object Identifier" = "DOI",
        "Document Identifier" = "EID",
        "First Author Name" = "FIRSTAUTH",
        "First page" = "PAGEFIRST",
        "Language" = "LANGUAGE",
        "Last page" = "PAGELAST",
        "Load Date" = "LOAD-DATE",
        "Pages" = "PAGES",
        "Editor" = "EDITOR",
        "Editor First Name" = "EDFIRST",
        "Editor Last Name" = "EDLASTNAME",
        "Funding Information" = "FUND-ALL",
        "Funding Sponsor" = "FUND-SPONSOR",
        "Grant Number" = "FUND-NO",
        "Sponsor Acronym" = "FUND-ACR",
        "Author Keywords" = "AUTHKEY",
        "Index Terms" = "INDEXTERMS",
        "Keywords" = "KEY",
        "Keyword" = "KEY", # Alias
        "Manufacturer" = "MANUFACTURER",
        "Trade Name" = "TRADENAME",
        "Article Numberr" = "ARTNUM",
        "Book Publisher" = "BOOKPUB",
        "CODEN" = "CODEN",
        "Date of Publication" = "PUBDATETXT",
        "EISSN" = "EISSN",
        "Exact Source Title" = "EXACTSRCTITLE",
        "ISBN" = "ISBN",
        "ISSN" = "ISSN",
        "ISSNP" = "ISSNP",
        "PubMed Identifier" = "PMID",
        "Serial Issue ID" = "ISSUE",
        "Serial Volume" = "VOLUME",
        "Source Identifier" = "SRCID",
        "Source Title" = "SRCTITLE",
        "Source Type" = "SRCTYPE",
        "Year of Publication" = "PUBYEAR",
        "Reference" = "REF",
        "Reference Article Number" = "REFARTNUM",
        "Reference Author" = "REFAUTH",
        "Reference First Page" = "REFPAGEFIRST",
        "Reference Page Numbers" = "REFPAGE",
        "Reference Publication Year" = "REFPUBYEAR",
        "Reference Source Title" = "REFSRCTITLE",
        "Reference Title" = "REFTITLE",
        "Reference Website" = "WEBSITE",
        "Subject Areas" = "SUBJAREA"
    )

    # See: <https://bit.ly/3sj8nsz>
    wos <- list(
        "All Fields" = "ALL",
        "Topic" = "TS",
        "Title" = "TI",
        "Titles" = "TI", # Alias
        "Abstract" = "AB", # Alias
        "Abstracts" = "AB", # Alias
        "Author" = "AU",
        "Authors" = "AU", # Alias
        "Author Identifiers" = "AI",
        "Author Keywords" = "AK",
        "Keyword" = "AK", # Alias
        "Keywords" = "AK", # Alias
        "Group Author" = "GP",
        "Editor" = "ED",
        "Keyword Plus" = "KP",
        "Publication Titles" = "SO",
        "DOI" = "DO",
        "Year Published" = "PY",
        "Conference" = "CF",
        "Address" = "AD",
        "Affiliation" = "OG",
        "Organization" = "OO",
        "Suborganization" = "SG",
        "Street Address" = "SA",
        "City" = "CI",
        "Province/State" = "PS",
        "Country/Region" = "CU",
        "Zip/Postal Code" = "ZP",
        "Funding Agency" = "FO",
        "Grant Number" = "FG",
        "Funding Text" = "FT",
        "Research Area" = "SU",
        "Web of Science Categories" = "WC",
        "ISSN/ISBN" = "IS",
        "Accession Number" = "UT",
        "PubMed ID" = "PMID",
        "Index Date" = "LD",
        "Publication Date" = "DOP"
    )

    export <- stringr::str_subset(ls(), "write", negate = TRUE)

    for (i in export) {
        assign(i, change_name(get(i), tolower(names(get(i)))))
    }

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
