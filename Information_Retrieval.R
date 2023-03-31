#     Information Retrieval   #

# dependencies: 
      # databases - easyPubMed, europepmc
      
      # other dependencies - 
            library(tidyverse)


      #attributes:
            
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
