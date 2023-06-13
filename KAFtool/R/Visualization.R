#Visualization
#' The KAF visualization suite
#'
#' This function allows you to display rules as a bargraph or as a dataframe according to the classification of choice.
#' @name ruleViewer
#' @param rules The data frame of rules you want to classify
#' @param viz The visualization option ("df" or "bar")
#' @param class The classification display option ("all","bme","anatomy", "organism","diseases","chemicals","techniques","other","unlab")

#' @keywords Post processing, Association-rule mining, ARM, classification, MeSH, visualization
#' @export
#' @examples
#' ruleViewer(viz <- ruleViewer(classified_rules, "bar", "bme"))
ruleViewer <- function(rules, viz, class){


      #7 choices for data selection
      choix <- switch(class,

                      "all" = rules,
                      "bme" = rules[which(rules$MeSH_Ancestry == "A"| rules$MeSH_Ancestry == "C" | rules$MeSH_Ancestry == "D"| rules$MeSH_Ancestry == "E"),],
                      "anatomy" = rules[which(rules$MeSH_Ancestry == "A"),],
                      "organism" = rules[which(rules$MeSH_Ancestry == "B"),],
                      "diseases" = rules[which(rules$MeSH_Ancestry == "C"),],
                      "chemicals" = rules[which(rules$MeSH_Ancestry == "D"),],
                      "techniques" = rules[which(rules$MeSH_Ancestry == "E"),],
                      "other" = rules[which(rules$MeSH_Ancestry == "F"| rules$MeSH_Ancestry == "G" | rules$MeSH_Ancestry == "H" |rules$MeSH_Ancestry == "I" | rules$MeSH_Ancestry == "J" | rules$MeSH_Ancestry == "K" | rules$MeSH_Ancestry == "L" | rules$MeSH_Ancestry == "M" | rules$MeSH_Ancestry == "N" | rules$MeSH_Ancestry == "V" | rules$MeSH_Ancestry == "Z"),],
                      "unlab" = rules[which(rules$MeSH_Ancestry == "NA" | is.na(rules$MeSH_Ancestry) == TRUE),]
      )
      #test script                )
      #rules <- class_smol
      #choix <- rules
      #choix <- rules[which(rules$MeSH_Ancestry == "A"| rules$MeSH_Ancestry == "C" | rules$MeSH_Ancestry == "D"| rules$MeSH_Ancestry == "E"),]

      #type of visualization = df, bar, treemap
      freq = choix %>% count(RHS) %>% arrange(desc(n))
      alt <- choix %>% select(c(RHS, (MeSH_Ancestry)))

      alt = unique(alt)
      freq <- left_join(freq,alt)

      freq <- freq %>%
            arrange(
                  RHS,
                  desc(n),
                  MeSH_Ancestry
            )

      result = switch( viz,

            "df"= choix,
            "bar"=       ggplot(freq[1:20,], aes(x= reorder(RHS, -n), y = log10(n) )) + geom_col(data = freq, aes(x = reorder(RHS, -n), y = log10(n), fill = MeSH_Ancestry)) +geom_point(pch = 21, aes(x = reorder(RHS, -n), y = log10(n), fill = MeSH_Ancestry)) + ggtitle(paste("Rule frequencies based on:", toupper(class))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                  ylab("log10(number of Keyword Relationships)") + xlab("RHS of Keyword Relationship")+ theme(axis.text.y = element_text(size = 15)) + theme(axis.title = element_text(size = 16)) + theme(title = element_text(size = 20)) + theme(legend.position = c(0.85,0.85)) +
                   coord_flip() #+ scale_x_discrete(guide = guide_axis(n.dodge=3))
            #"tree"= cat("Division = ", val1 / val2)
      )

      return(result)
}
