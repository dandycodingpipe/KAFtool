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

  library(textreuse)

  #Concatenate and filter rules according to their RHS
  broken_down_rules <- Rule_Concatenator(rules)
  rules_to_remove <- c()

  RHS_len <-  length(unique(broken_down_rules$rules.RHS))


   for(i in 1:RHS_len){

     #Defining hash table parameters
     minhash <- minhash_generator(n = 100, seed = 3552)
     #Convert the current RHS rules into keys
     corpus <- TextReuseCorpus(text = broken_down_rules$toFuzzy[which(broken_down$rules.RHS == broken_down$rules.RHS[i])], tokenizer = tokenize_ngrams, n = 1,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = TRUE)
     #Generate buckets signifying potential matches
     buckets <- lsh(corpus, bands = 25, progress = TRUE)
     #Display results
     final <- lsh_compare(lsh_candidates(buckets), corpus, jaccard_similarity, progress = TRUE)
     final = final[which(final$score > 0.75),]

     #NOTE TO SELF: THESE PARAMETERS MUST BE COMPARED FOR PERFORMANCE

     #Recursive process

     #1. Create an empty array to hold coordinates to remove, and one for positive coordinates to keep
     #2. For every unique doc-n that is not already in the removal array, add it to the positive coordinate array, and return the documents stored in it's column B
     #3. While this item in column B is not in the positive array and not in the remove array, add it to the removal array

    }

  return(clean_rules)
}
