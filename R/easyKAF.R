# easyKAF function #

# this script summarizes the entire KAF workflow of information retrieval, NLP, and ARM



#' easyKAFtool
#'
#' With the path to your virtual environment and the defined english language model of choice, this function will prompt you to entry a query to search in either PubMed or Europe PMC! If this function does not work, you probably defined the venv wrong. You will need to uninstall the package, and retry with the correct path.
#' @name easyKAF
#' @param venv The path to the initialized python virtual environment on your computer.
#' @param lang_model The name of your english language model.
#' @keywords KAF, Association-rule mining, ARM, classification, MeSH, visualization, KAFtool, Systox
#' @export
#' @examples
#' rules <- easyKAF(venv = "C:/Users/JohnDoe/venv/mar6", lang_model = "en_core_web_sm")
easyKAF <- function(venv, lang_model){

  library(tidyverse)
  library(tidytext)
  library(arules)
  library(arulesViz)

# what do you want to search?

print("((complement system pathway) OR (complement system proteins)) AND (pathology OR pathologies) AND 1980:2023[dp]")
print("dog hip dysplasia")
print("human DNA polymerase AND (damage or repair)")
print("(Crohns disease OR Crohn's disease) AND (therapy OR treatment OR prognosis)")
print(myQuery <- "(muscle synergy OR muscle coordination)")
print("the complement system pathway and diseases")
print("(gunpowder OR propellant) AND toxicity")
print("microfabrication chip AND membrane encapsulation")


#commence KAF worfklow

queryString <- toupper(toString(readline("these are some examples above. write whatever you want and see what biologists know about the subject!")))

databaseString <- toupper(toString(readline(paste("U.S PubMed or Europe PMC as a database? (EU runs better!)", '\n', "EU (1) or US (2):"))))

 if(databaseString == "1"){
            databaseString = "pmc"} else {databaseString = "pubmed"}
      # 1. retrieving




                  retrieved <- info_retrieval(query = queryString, how_many_articles = 1000,  database = databaseString)

      # 2. parsing


                  preProcessed <- Text_Parser(retrieved, venv = venv, lang_model =  lang_model, reduced_search = 0.2)

      # 3. mining

                  rules <- Abstract_ARM(preProcessed, 0.01, 0.75, 0.005)
                  #filt_rules <- which(small_rules$lift <= 2)
                  #rules <- rules[-filt_rules,]

                  return(rules)
}


