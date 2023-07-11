# KAF Rule AOP-wiki Fuzzy Matching
#' Compare KAF results to the AOP-wiki database to find notable key events, stressors, and biological processes present in your PubMed query.
#'
#' (In development) Uses locality-sensitive hashing to efficiently cluster and calculate the Jaccard similarity between mined rules and AOP-wiki values/classes. Values below 60% similarity are not considered matches.
#' @name KAFxAOP
#' @param rules Association rule dataframe that was mined using Abstract_ARM or easyKAF. Limit this parameter to 5000-20000 rules as deduplication is very computationally demanding.
#' @keywords Hashing, LSH, Locality sensitive hashing, deduplication, Jaccard Similarity, fuzzy matching, AOP
#' @export
#' @examples
#' KAFxAOP(rules)

KAFxAOP <- function(sample) {

  results <- cbind(sample, AOPclass = NA, AOPvalue = NA)
  #and not FREE
#if(length(sample$RHS) > 5000){
#  lift_filtering <- results[order(-results$lift[1:5000]),]
#} else {
  lift_filtering <- results[order(-results$lift),]
#}

broken_down <- Rule_Concatenator(lift_filtering)

#Pre-downloaded aop-wiki quarterly backup data
doc <- "aop-wiki-xml-2023-04-01(1).xml"

#XML Processing for AOP class/value extraction
doc <- AOP_XML_children_organizer(doc)

og_doc <- doc

#doc$value <- tolower(doc$value)

#Combine AOP and association-rule corpora
concat_df <- data.frame(corpora = c(doc$value, broken_down$toFuzzy))

# Locality Sensitive Hashing Package
library(textreuse)

#Defining hash table parameters
minhash <- minhash_generator(n = 6750, seed = 2553)

#Create hash codes for every object
Hashed <- TextReuseCorpus(text = concat_df$corpora, tokenizer = tokenize_ngrams, n = 1,
                          minhash_func = minhash, keep_tokens = TRUE,
                          progress = TRUE)

#Create buckets for storing potentially similar keys (hash codes)
Bucketing <- lsh(Hashed, bands = 450, progress = TRUE)

#Calculate and extract scores according to my threshold
final <- lsh_compare(lsh_candidates(Bucketing), Hashed, jaccard_similarity, progress = TRUE)
final = final[which(final$score >= 0.6),]


#The workflow of processing LSH results

#Removing extraneous characters and presenting results as coordinates
final$a = substr(final$a, 5, nchar(final$a))
final$b = substr(final$b, 5, nchar(final$b))

final$a <- as.numeric(final$a)
final$b <- as.numeric(final$b)

aoppies <- final[which(final$a <= 3956),]
aoppies$b = as.numeric(aoppies$b)

AOPcoordinate_list <- c()
Rulescoordinate_list <- c()

for(k in 1:length(aoppies$a)){
  if(aoppies$b[k] > 3956){
    if(aoppies$score[k] >= 0.6){
print(paste("PAIR", k, ":", concat_df[aoppies$a[k],], "+", concat_df[aoppies$b[k],], "SCORE:", aoppies$score[k]))
      print(which(doc$value == concat_df[aoppies$a[k],]))
      AOPcoordinate_list <- c(AOPcoordinate_list, which(doc$value == concat_df[aoppies$a[k],]))
      Rulescoordinate_list <- c(Rulescoordinate_list,which(broken_down$toFuzzy ==concat_df[aoppies$b[k],]))
      }
    }
}

doc[AOPcoordinate_list,]
g <- lift_filtering[as.numeric(rownames(broken_down[Rulescoordinate_list,])),]
g$X
sample$AOPclass[g$X,] = doc$AOP_Class[AOPcoordinate_list,]
sample$AOPvalue[g$X,] = doc$value[AOPcoordinate_list,]

return(sample)
print("#####Done####")

}



