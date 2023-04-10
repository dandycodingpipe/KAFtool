# Main
      
      source("easyKAF.R")
      source("superKAF.R")
      source("MeSH_Classification.R")
      source("Visualization.R")
      
# learn from several scientific abstracts in 3 steps:
      
      #     mining:
            rules <- easyKAF(venv = "your\\venv\\path", lang_model = "your language model")
                        #Mac venv: "/Users/Notebook/mar7"

      #     classification:

            clean_rules <- MeSH_finalizer(rules, removal = c("study","lead","±"))


      #     visualization:

            viz <- ruleViewer(clean_rules, "df", "bme")
            viz[which(viz$RHS == "{insert word of interest}"),]
     
     

      #     exporting to CSV for excel if you'd like
      
            write.csv(viz, file = "sig_rules.csv")

