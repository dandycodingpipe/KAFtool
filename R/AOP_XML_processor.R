# AOP-Wiki quarterly backup processing script
#' Converts AOP-wiki XML files into R data frames for further processing
#'
#' This function takes XML and converts
#' @name AOP_XML_children_organizer
#' @param doc XML file downloaded from AOP-wiki
#' @keywords Classification, AOP-wiki, AOP
#' @export
#' @examples
#' Word_Cleaner(data = association_rules_df)

AOP_XML_children_organizer <- function(doc) {

  library(XML)

  doc <- xmlRoot(xmlTreeParse(doc))

  class_frequencies <- table(names(doc))

  class_coordinates <- names(names(doc))

  unique_classes <- unique(class_coordinates)

  # Create a list containing the separated coordinates for every instance of every class

  my_list <- vector("list", length(unique_classes))
  names(my_list) <- unique_classes

    for(i in 1:length(unique_classes)){

      class_iteration_array <- which(class_coordinates == unique_classes[i])
      my_list[[i]] <- class_iteration_array

    }

tmp = xmlSApply(doc, function(x) xmlSApply(x, xmlValue))


tester <- data.frame(AOP_Class = 0, value = 0)

for(h in 1:length(unique_classes)){

  label <- unique_classes[h]

  for(i in 1:length(my_list[[label]])){

    # Use switch case based on the value of the number variable
    result <- switch(label,
                     "chemical" = "preferred-name",
                     "biological-object" = "name",
                     "biological-process" = "name",
                     "biological-action" = "name",
                     "stressor" = "name",
                     "taxonomy" = "name",
                     "key-event" = "short-name",
                     "aop" = "short-name",
                     default = NULL)


    coordinate <- my_list[[label]][i]


    if (!is.null(result)) {
      text <- tmp[[coordinate]][result]

      new_row <- c(label, toString(text))
      tester <- rbind(tester, new_row)
    }

  }
}
return(tester)
}








