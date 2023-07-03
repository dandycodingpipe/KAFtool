# KAF Rule De-duplication function
#' Removes redundant rules prior to matching rules to AOP-wiki
#'
#' This function returns a smaller ruleset containing less near-duplicates using Locality Sensitive Hashing (LSH).
#' @name KAF_Deduplication
#' @param rules Rules generated through easyKAF or the manual KAF pipeline.
#' @keywords Hashing, LSH, Locality sensitive hashing, deduplication
#' @export
#' @examples
#' KAF_Deduplication(rules)

KAF_Deduplication <- function(rules) {

  # Simplify data frame to basic elements we need (8 columns to 2 columns)
  broken_down <- Rule_Concatenator(rules)

  # Locality Sensitive Hashing Package
  library(textreuse)

  #Defining hash table parameters
  minhash <- minhash_generator(n = 2000, seed = 3552)

  #Structures string documents for LSH (change the 1 to any number to compare different "pre-bucket" sets of data)
  corpus <- TextReuseCorpus(text = broken_down$toFuzzy, tokenizer = tokenize_ngrams, n = 1,
                            minhash_func = minhash, keep_tokens = TRUE,
                            progress = TRUE)


  #Create buckets for storing potentially similar keys
  buckets <- lsh(corpus, bands = 200, progress = TRUE)

  #Calculate and extract scores according to my threshold
  final <- lsh_compare(lsh_candidates(buckets), corpus, jaccard_similarity, progress = TRUE)
  print(length(final$score))
  print(paste(length(final[which(final$score >= 0.75),]), "observations are above the 75% threshold."))
  final = final[which(final$score >= 0.9),]
  print(paste(length(final[which(final$score >= 0.90),]), "observations are above the 90% threshold."))

  #The workflow of removing duplicates
  toKeep <- c()
  toRemove <- c()
  final$a = substr(final$a, 5, nchar(final$a))
  final$b = substr(final$b, 5, nchar(final$b))
  LSH_doc_i <- unique(final$a)


  for(k in 1:length(LSH_doc_i)){

    if(LSH_doc_i[k]%in%toRemove){
      LSH_doc_i_coord <- final$b[which(final$a == LSH_doc_i[k])]
      toRemove <- c(toRemove, LSH_doc_i_coord)

    } else {

      toKeep <- c(toKeep,LSH_doc_i[k])

      LSH_doc_i_coord <- which(final$a == LSH_doc_i[k])

      for(i in 1:length(LSH_doc_i_coord)){
        if(!(final$b[LSH_doc_i_coord[i]] %in% toKeep)){
          toRemove <- c(toRemove,final$b[LSH_doc_i_coord[i]])
        }
      }

    }

  }

  toKeep <- unique(toKeep)
  toRemove <- unique(toRemove)
  print(paste(length(toRemove)))
  broken_down <- broken_down[-c(as.numeric(toRemove)),]

  return(broken_down)
}
