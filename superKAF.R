# superKAF A.K.A Validation Testing Environment

# OBJECTIVE: Create a function that asseses and returns the similarity between PubMed and Europe PMC retrievals

############ MWE #######################

#txns1 <- c(1,4,6,3,9,2,0,8,5)
#txns2 <- c(8,7,9,4,2,6,3,6,7)

#combine <- intersect(txns1,txns2)

#quant <- length(combine)/length(txns2)

########################################

# Compare BOTH the abstract corpus similarity and the LHS/RHS similarity of rules

source("Information_Retrieval.R")
source("NLP.R")
source("Apriori_ARM_tool.R")

#QUERIES
myQuery <- "(Crohns disease OR Crohn's disease) AND (therapy OR treatment OR prognosis)"
myQuery <- "(vape smoking OR vape smoke OR vaping) toxicity"
myQuery <- "amino acid deficiencies AND (disease OR syndrome)"
myQuery <- "induced pluripotent stem cell transplantation challenges"
myQuery <- "noise pollution AND (mental health OR anxiety OR depression)"
myQuery <- "(multigenerational OR acute) toxicity AND (BPA OR BPF OR bisphenol A OR bisphenol F)"
myQuery <- "global warming current status"

superKAF <- function(venv, lang_model){
      
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
      print("these are some examples above. Be mindful that superKAF queries can take a significant amount of time. Please be patient.")
      queryString <- toupper(toString(readline("Write whatever you want to perform a superKAF query:")))
      
      raw_data_1 <- info_retrieval(queryString, 1000, 'pubmed') 
      raw_data_2 <- info_retrieval(queryString, 1000, 'pmc')

#PMID IDENTITY %

      which(raw_data_1$pmid == intersect(raw_data_1$pmid,raw_data_2$pmid))

      length(intersect(raw_data_1$pmid,raw_data_2$pmid))/length(raw_data_2$pmid)


# Finishing ARM procedure

      NLP1 <- Text_Parser(raw_data_1, venv_path = venv, lang_model = lang_model, reduced_search = 0.2)
      NLP2 <- Text_Parser(raw_data_2, venv_path = venv, lang_model = lang_model, reduced_search = 0.2)

      rule_data_1 <- ARM(NLP1, min_supp = 0.01, min_conf = 0.75, min_p = 0.001)
      rule_data_2 <- ARM(NLP2, min_supp = 0.01, min_conf = 0.75, min_p = 0.001)



concat_data_1 <- rule_data_1 %>% mutate(rule_data_1, concat = paste("[",rule_data_1$LHS,rule_data_1$RHS,"]"))

concat_data_2 <- rule_data_2 %>% mutate(rule_data_2, concat = paste("[",rule_data_2$LHS,rule_data_2$RHS,"]"))


# Rule Identity 
print(paste("Rule Identity:",length(intersect(concat_data_1$concat,concat_data_2$concat))/length(concat_data_2$concat) ))

checked <- intersect(concat_data_1$concat,concat_data_2$concat)

#selecting for identical rules

checked <- data.frame(concat = checked)
names(checked)[1] <- "concat" 

woah <- left_join(checked, concat_data_1)
woah2 <- left_join(checked, concat_data_2)

#view(woah2[which(woah2$LHS == "{cause,mutation,patient,sequence,whole}"),])
#view(woah[which(woah$LHS== "{cause,mutation,patient,sequence,whole}"),])

#to validate by calculating the % difference in ARM metrics: support, confidence, coverage, and lift


# Average differences in ARM metrics between databases

      #(mean(woah2$support)-mean(woah$support))/mean(woah2$support)

      #(mean(woah2$confidence)-mean(woah$confidence))/mean(woah2$confidence)

      #(mean(woah2$coverage)-mean(woah$coverage))/mean(woah2$coverage)

      #(mean(woah2$lift)-mean(woah$lift))/mean(woah2$lift)

return(woah)
}










