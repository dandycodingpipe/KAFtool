#' A fully integrated Pubmed abstract retrieval function
#'
#' This function allows you to query and access key Pubmed entry data from one of two databases: PubMed and EuropePMC.
#' @name info_retrieval
#' @param Query Your typical PubMed query. Optimizing your query using the proper pubmed syntax improves results!
#' @param how_many_articles Define how many entries you'd like to access for a query. Keep it between 500-1000 for fastest results.
#' @param database Define the database you'd like to use: "pubmed" or "pmc".
#' @keywords Pubmed, PMC, Query
#' @export
#' @examples
#' info_retireval(Query = "Vape smoking AND toxicity", how_many_articles = 750, database = "pubmed")

            library(tidyverse)
            library(easyPubMed)
            library(europepmc)


      #examples:

            #query <- "Your PubMed or PMC query here"
            #how_many_articles <- 500
            #database <- "pmc" or "pubmed"

      #test
            #giveArticles <- info_retrieval(query,how_many_articles,database)

info_retrieval <- function(query, how_many_articles, database) {

      how_many_articles <- how_many_articles

      if(database == "pubmed") {

            # PubMed
            # benefits: global standard but limited to (n = 500-1500) extractions

            # code:
                  library(easyPubMed)
                  retrieved_info <- pubmed_retrieval(query = query, retmax = how_many_articles)


      } else if(database == "pmc") {
            # Europe PMC
            # benefits: more data can be extracted (n = 20,000)
            # code:
                  library(europepmc)
                  retrieved_info <- europepmc_retrieval(query = query, retmax = how_many_articles)

      } else {
            print("Error: database must equal pubmed or pmc")
      }#end if-'else if'-else loop

      return(retrieved_info)
}

# pubmed search and retrieve function

#' The NIH Pubmed search and retrieve function
#'
#' This function allows you to query and access key Pubmed entry data from one of two databases: PubMed and EuropePMC.
#' @name pubmed_retrieval
#' @param Query Your typical PubMed query. Optimizing your query using the proper pubmed syntax improves results!
#' @param retmax Define how many entries you'd like to access for a query. Keep it between 500-1000 for fastest results.
#' @keywords Pubmed, PMC, Query, Retrieval
#' @export
#' @examples
#' pubmed_retrieval(Query = "Vape smoking AND toxicity", retmax = 750)

pubmed_retrieval <- function(query, retmax) {
      #1 PMID Retrieval

      PMIDs <- get_pubmed_ids(query)
      print(paste(PMIDs$Count, "PMIDs retrieved. Fetching article information..."))

      #2 XML Format Record Download w/ PMID list
      articleInfo <- fetch_pubmed_data(PMIDs, retmax = retmax)

      #4 Convert XML to String
      #(this is to convert the content of each PubMed record to a character-class object)
      xmlToString <- articles_to_list(articleInfo)
      print(paste(length(xmlToString), "abstracts were retrieved. Creating output dataframe... (this may take a while)"))

      #5 Dataframe Retrieval
      stringToDF <- do.call(rbind,lapply(xmlToString, article_to_df, max_chars = -1, getAuthors = FALSE))
      return(stringToDF)
}

# pmc search and retrieve function

# pubmed search and retrieve function

#' The Europe PMC search and retrieve function
#'
#' This function allows you to query and access key Pubmed entry data from one of two databases: PubMed and EuropePMC.
#' @name europepmc_retrieval
#' @param Query Your typical PubMed query. Optimizing your query using the proper pubmed syntax improves results!
#' @param retmax Define how many entries you'd like to access for a query. Keep it between 500-1000 for fastest results.
#' @keywords Pubmed, EuropePMC, PMC, Query, Retrieval
#' @export
#' @examples
#' europepmc_retrieval(Query = "Vape smoking AND toxicity", retmax = 750)
europepmc_retrieval <- function(query, retmax) {

      #strict PMC condition
      #print("Would you like to conduct a strictly Europe PMC search?")
      #strictPMC <- toupper(toString(readline("Y/N:")))

      # start if statement
      #if(strictPMC == 'Y') {

      #      print("Strict PMC searches take a while. Initializing...")
            #much a much larger sample size is required for strict PMC searches
            #because not all of the entries in Europe PMC are PMC entries
      #      how_many_articles = how_many_articles*25

            #1 PMID/PMC Retrieval
      #      europe_search <- epmc_search(query, output = 'raw', limit = retmax)

      #      #2 PMC or MEDLINE Retrieval
      #      retrieved_info <- keep(europe_search, function(x) (x[['source']] == 'PMC') && !is.null(x[['abstractText']])) %>%
      #            map_dfr(~ modify_at(.x, "journalInfo", flatten_dfr))
      #} else if(strictPMC == 'N') {

            #1 PMID/PMC Retrieval
            europe_search <- epmc_search(query, output = 'raw', limit = retmax)

            #2 PMC or MEDLINE Retrieval
            retrieved_info <- keep(europe_search, function(x) (x[['source']] == 'MED') && !is.null(x[['abstractText']])) %>%
                  map_dfr(~ modify_at(.x, "journalInfo", flatten_dfr))
      #}
      # end if statement
      names(retrieved_info)[names(retrieved_info) == "id"] <- "doc_id"
      names(retrieved_info)[names(retrieved_info) == "abstractText"] <- "abstract"

      return(retrieved_info)
}
