

#' Perform association-rules mining on lemmatized abstracts.
#'
#' Using the results from the Text_Parser() function, generate a dataframe of association-rules from the corpus. P-values for results are calculated using Fischer's exact test, and FDR are used to remove false rejections of the null hypothesis.
#' Be aware of your ARM mining parameters because these can seriously impact your computational load.
#' @name Abstract_ARM
#' @param data Your text_parser() results go here.
#' @param min_supp Define the minimum support threshold for the rules (e.g 0.01 or 0.10)
#' @param min_conf Define the minimum confidence threshold for the rules (e.g 0.50 or 0.90)
#' @param min_p Define the minimum p-value threshold for the rules (e.g 0.05 or 0.0.005)
#' @keywords Processing algorithm, association-rules mining, statistics, arules,
#' @export
#' @examples
#' Abstract_ARM(data <- data_from_Text_Parser, min_supp = 0.01, min_conf = 0.75, min_p = 0.005)

library(arules)
Abstract_ARM <- function(data, min_supp, min_conf, min_p ){
  #rule generation
  print("Converting pasrsed abstracts into item/transaction format...")
      data$doc_id <- as.factor(data$doc_id)
      data$lemma <- as.factor(data$lemma)
       txns <- as(split(data$lemma,data$doc_id), "transactions")

       #trouble shooting script
       #parsed_abstracts$doc_id <- as.factor(parsed_abstracts$doc_id)
       #parsed_abstracts$lemma <- as.factor(parsed_abstracts$lemma)
      #txns <- as(split(parsed_abstracts$lemma,parsed_abstracts$doc_id),"transactions")
      #min_supp <- 0.1
      #min_conf <- 0.5
      #min_p <- 0.005

      print("done")
  print("Initiating apriori algorithm...")
  rules <- apriori(txns, parameter = list(supp = min_supp, conf = min_conf, target = 'rules'), control = list(memopt = TRUE))

  #statistical filtering
  print("Removing rules that do not meet p-value thresholds and type-1 errors...")
  table <- is.significant(rules, txns, method = "Fisher", alpha = min_p, adjust = "fdr")
  falses <- which(table == FALSE)
  rules <- rules[-falses,]

  print("Formatting rules into R-data frame...")
  df_rules <- DATAFRAME(rules)
  #if(length(which(df_rules$RHS=="{-}"))!= 0) {
  #df_rules <- df_rules[-which(df_rules$RHS == "{-}"),]
  #}
  #if(length(which(df_rules$RHS=="{%}"))!= 0) {
  #df_rules <- df_rules[-which(df_rules$RHS== "{%}"),]
  #}
  print("Done!")
  return(df_rules)
}





