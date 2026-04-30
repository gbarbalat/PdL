rm(list=ls())
library(readxl)
results_survey215846Bis <- read_excel("C:/Users/Guillaume/Desktop/PdL/results-survey215846Bis.xlsx")

library(dplyr)
library(table1)

# table1 functions ----

render.NEW <- function(x, name, data2, ...) {
  MIN <- min(x, na.rm = T)
  MAX <- max(x, na.rm = T)
  median <- median(x, na.rm = T)
  Q1 <- quantile(x, 0.25, na.rm = T)
  Q3 <- quantile(x, 0.75, na.rm = T)
  N = length(x) - sum(is.na(x))
  
  # out <- c("",
  #          "[min, max]" = paste0("[", sprintf("%.2f", MIN), ", ", sprintf("%.2f", MAX), "]"),
  #          "Median [Q1, Q3]" = paste0(sprintf("%.2f", median), " [", sprintf("%.2f", Q1), ", ", sprintf("%.2f", Q3), "]"),
  #          "N" = N)
  
  out <- c("",
           "Median [Q1, Q3]" = paste0(sprintf("%.2f", median), " [", sprintf("%.2f", Q1), ", ", sprintf("%.2f", Q3), "]")
           )
  
  out
}

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=2, eps=0.0001)))
}


path="C:/Users/Guillaume/Desktop/PdL/"
#PdL=read.csv(file = paste0(path,"results-survey215846.csv"))
#import à la main!
PdL=results_survey215846Bis

#pie chart 4 Virginie ----
Virginie=FALSE
if (Virginie) {
pie.confirm=c(36,100-36);names(pie.confirm)=c("Non-confirm.","Confirm.");pie(pie.confirm,cex=3)
x=19;who_pie=c(x,100-x);names(who_pie)=c("Pas de Traces","Traces");pie(who_pie,cex=3)
x=47;who_pie=c(x,100-x);names(who_pie)=c("< 2 pièces","> 2 pièces");pie(who_pie,cex=3)
x=28;who_pie=c(x,100-x);names(who_pie)=c("> 6 mois","< 6 mois");pie(who_pie,cex=3)
x=48;who_pie=c(x,100-x);names(who_pie)=c("Pbs de santé pris\n en compte","Pas pris en compte");pie(who_pie,cex=3)
x=60;who_pie=c(x,100-x);names(who_pie)=c("Pas aide financière","Aide");pie(who_pie,cex=3)
x=67;who_pie=c(x,100-x);names(who_pie)=c("Aide financière","Non");pie(who_pie,cex=3)
x=36;who_pie=c(x,100-x);names(who_pie)=c("Intervention diff.\n à accepter","Pas de pbs");pie(who_pie,cex=3)
x=30;who_pie=c(x,100-x);names(who_pie)=c("Mobilier détruit","Pas de destruction");pie(who_pie,cex=3)
x=44;who_pie=c(x,100-x);names(who_pie)=c("Mesures\n pas efficaces","Mesures efficaces");pie(who_pie,cex=3)
x=81;who_pie=c(x,100-x);names(who_pie)=c("PTSD Punaises","Pas PTSD");pie(who_pie,cex=3)

# Overall survey ----.
PdL %>% nrow()
PdL %>% filter(PdL_infect=="Non") %>% nrow()
PdL %>% filter(PdL_infect=="Oui") %>% nrow()

# PdL infestation > 1 year ----
var=c("age","sex","pro","fam","kids","educ","IA","mdc","psych")

anal1=PdL %>% filter(PdL_infect=="Non") %>%
  select(c(all_of(var),c("WEMWBS_deb":"WEMWBS_fin"))) %>%
  mutate_at(vars(matches("WEMWBS")),~recode(., 
                     'Jamais' = 1, 
                     'Rarement' = 2, 
                     'Parfois' = 3,
                     'Souvent' = 4,
                     'Tout le temps' = 5,
                     .default = NA_real_)) %>%
  mutate(sex=na_if(sex,"Autre")) %>%
  
  rowwise() %>% 
  mutate(sum_WEMWBS = sum(c_across(starts_with("WEMWBS")), na.rm = FALSE)) %>%
  mutate(L_WEMWBS=length(c_across(starts_with("WEMWBS"))[!is.na(c_across(starts_with("WEMWBS")))])) %>%
  ungroup() %>%
  
  select(-starts_with("WEMWBS")) %>% #na.omit()
  
  mutate(educ=recode(educ,
              "Pré-Bac"="Pre_Bac",
              "Bac, Bac+1, Bac +2 (ou équivalent)"="Bac",
              "Bac +3 ou plus"="Post_Bac"
  ),
  pro=recode(pro,
           "Etudiant"="Nope",
           "Ne travaille pas"="Nope",
           .default="yes"
           
  ),
  fam=recode(fam,
           "Vit en concubinage, PACSé, Marié"="Rel",
           "En relation mais vit séparément"="Rel",
           
           .default="Nope"
           
  ),
  IA=recode(IA,
          "Moins d'une fois par semaine"="Nope",
          "Une fois par semaine"="Nope",
          .default="yes"
          
  ),
  kids=recode(kids,
            "0"="0",
            "1"="1",
            .default="2_"
            
  )) %>%

  mutate(age=as.numeric(age)) %>%
  mutate(age=as.character(cut(age,
                              #breaks=c(quantile(age, probs = seq(0, 1, by = 0.25), na.rm=TRUE)),
                              breaks=c(16,   25,  35 ,  45 ,  55 ,  96 ),
                              
                              labels=FALSE))) %>%
  mutate(across(where(is.character),~na_if(.,""))) %>%
  mutate(pct_NA = rowSums(is.na(select(., -L_WEMWBS)))/ncol(select(., -L_WEMWBS))
         )  %>% # better than mutate(pct_NA=rowSums(is.na(.))/ncol(.)) 
  #filter(L_WEMWBS>12 & pct_NA<=0.3) 
  filter(pct_NA<=0.3) 

## Flowchart of missing data ----
#comment last 1 filter command
nrow(anal1)

hist(anal1$pct_NA)#pct NA per row
hist(anal1$L_WEMWBS, 50)


## Table 1 ----
# check that no variable has more than 30% of missing values

table(anal1$age)
colSums(is.na(anal1))
anal1 %>% select(where(is.numeric)) %>% purrr::map(summary)
anal1 %>% select(-where(is.numeric)) %>% purrr::map(table)

table1(~ .,
       #| PdL_infect,
       data=anal1,  
       rowlabelhead = "Variables",
       overall=T#, extra.col=list(`P-value`=pvalue)
       #,render.continuous=my.render.cont
) -> Table1_final
Table1_final

lm(sum_WEMWBS ~ .,anal1 %>% na.omit()) %>% summary()


# Stress & WEMWBS due to PdL ----

Stress_PdL=PdL %>% filter(PdL_infect=="Oui") %>% select(c(stress1:stress22)) %>%
  mutate_all(~recode(., 
                   'Pas du tout' = 0, 
                   'Un peu' = 1, 
                   'Moyennement' = 2,
                   'Passablement' = 3,
                   'Extrêmement' = 4,
                   .default = NA_real_))
SumStress=rowSums(Stress_PdL,na.rm=FALSE)
Stress_PdL=cbind(Stress_PdL,SumStress)

Stress_Sup=sum(!is.na(SumStress[SumStress > 33]))
Stress_Sup_idx=!is.na(SumStress[SumStress > 33])
Stress_Sup_idx=!is.na(SumStress[SumStress > 33])

Stress_inf=sum(!is.na(SumStress[SumStress <= 33]))
Stress_inf_idx=!is.na(SumStress[SumStress <= 33])

Stress_PdL %>% filter(SumStress>33) %>% nrow() /  Stress_PdL[!is.na(SumStress),] %>% nrow()


summary(lm(sum_WEMWBS ~ .,anal1 %>% filter(PdL_infect=="Oui") %>% select(-PdL_infect)))
}


# PdL infestation < 1 year ----
var_to_confirm="recognize"

var_PDL=c(
  "age","sex","pro","fam","kids","educ","IA","mdc","psych",#"log",
  
  #"recognize", #6=Yes, other=No #only to confirm that they know what a bedbug is
  "dxPdL", #dxPdL_au,
  "confirm",#confirmPdL,
  "where",# 	whereother1	whereother2	whereother3	whereother4	whereother5	
  "start",
  "duration",
  # "trtwho", 	
  "prepar",
  "Pesticides",
  "temperature",
  "Destruction",
  "moving",
  "etuvage",
  #"chemnum",
  "chemsafety",#discuss safety rules to go back home
  "chemsafety2",#compatibilite avec etat de sante du traitement par pesticides
  "effective",
  # "explain_prepar", too many missing data
  # "explain_trt",	
  # "explain_safety",
  # "explain_wishes",	
  # "explain_health",
  
  "fin",
     
  "difficother_prepar",
  "difficother_accept",
  "difficother_accept_end",
  
  # "talk_fam",
  # "talk_voisins",
  # "talk_bailleur",
  # "talk_health_pro",
  # "talk_tuteur",
  # "talk_desinsect",
  # "talk_no",
  
  "support_fam",
  # "support_voisins",
  "support_bailleur",
  # "support_health_pro",
  # "support_tuteur",
  "support_desinsect",
  
  # "contamfear_travail",
  # "contamfear_fam",
  # "contamfear_med_facil",
  # "contamfear_idel",
  # "contamfear_aux_vie",
  "contamfear_no",
  
  # "socialrestrict_travail",
  # "socialrestrict_fam",
  # "socialrestrict_cabinet_med",
  # "socialrestrict_idel",
  # "socialrestrict_aux_vie",
  "socialrestrict_no",

  # "socialrestrictfromot_travail",
  # "socialrestrictfromot_fam",
  # "socialrestrictfromot_cab_med",
  # "socialrestrictfromot_idel",
  # "socialrestrictfromot_aux_vie",
  "socialrestrictfromot_no"
)

anal3=PdL %>% filter(PdL_infect=="Oui") %>%
  
  mutate(age=as.numeric(age)) %>%
  mutate(age=as.character(cut(age, breaks=c(16,   25,  35 ,  45 ,  55 ,  96 )
                              , labels=FALSE))) %>%
  # mutate(age=as.character(cut(age,breaks=c(quantile(age, probs = seq(0, 1, by = 0.25), na.rm=TRUE))
  #                             , labels=FALSE))) %>%
  
  select(c(all_of(var_PDL),all_of(var_to_confirm)),stress1:stress22,c("WEMWBS_deb":"WEMWBS_fin")) %>%
  
  mutate_at(vars(matches("stress")),~recode(., 
                                            'Pas du tout' = 0, 
                                            'Un peu' = 1, 
                                            'Moyennement' = 2,
                                            'Passablement' = 3,
                                            'Extrêmement' = 4,
                                            .default = NA_real_)) %>%
  mutate_at(vars(matches("WEMWBS")),~recode(., 
                                            'Jamais' = 1, 
                                            'Rarement' = 2, 
                                            'Parfois' = 3,
                                            'Souvent' = 4,
                                            'Tout le temps' = 5,
                                            .default = NA_real_)) %>%
  
  rowwise() %>% 
  mutate(sum_WEMWBS = sum(c_across(starts_with("WEMWBS")), na.rm = FALSE)) %>%
  #mutate(average_WEMWBS = mean(c_across(starts_with("WEMWBS")), na.rm = TRUE)) %>%
  mutate(L_WEMWBS=length(c_across(starts_with("WEMWBS"))[!is.na(c_across(starts_with("WEMWBS")))])) %>%
  
  #mutate(sum_stress = sum(c_across(starts_with("stress")), na.rm = FALSE)) %>%
  mutate(average_stress = mean(c_across(starts_with("stress")), na.rm = TRUE)) %>%
  mutate(L_stress=length(c_across(starts_with("stress"))[!is.na(c_across(starts_with("stress")))])) %>%
  
  select(-c(stress1:stress22,starts_with(c("WEMWBS")))) %>%
  
  mutate(across(where(is.character),~na_if(.,""))) %>% ungroup() %>%
  mutate(sex=na_if(sex,"Autre")) %>%
  
  mutate(fin=recode(fin,
                    "J’ai pris en charge financièrement la préparation de mon logement et mon bailleur s'est chargé du reste" = "bon",
                    "J’ai pris en charge financièrement le coût de la préparation de mon logement et le coût des traitements" = "non",
                    "J’ai tout fait seul (auto-traitement) et/ou avec des proches / des amis / mon tuteur ou curateur"="non",
                    "Mon bailleur a pris en charge financièrement la préparation de mon logement et le traitement"="bon"),
         effective=recode(effective,
                          "Aucune efficacité jusqu’à maintenant"="nope",
                          "Oui, après moins d’une semaine"="ok",
                          "Oui, après plusieurs mois"="nope",
                          "Oui, après plusieurs semaines"="ok"),
         
         dxPdL=recode(dxPdL,
                      "A cause de problèmes de sommeil"="oth",
                      "A cause des piqûres/lésions de grattage"="oth",
                      "Autre"="oth",
                      "J’ai vu les insectes ou leurs déjections"="insect"
                      ),
         
         #Only to see if they really had a pb with bedbugs (Information bias)
         recognize=recode(recognize,
                      "6"="PdL",
                      .default="Mistake"

         ),
         
        confirm=recode(confirm, #quick start of treatment
                       "Non, il n'y a pas eu de confirmation et/ou le traitement a débuté rapidement"="no",
                       "Oui, un proche m’a aidé"="yes",#
                       .default="yes"),
         
         where=recode(where,
                      "Dans tout l’appartement"="lots", 
                      "Dans une chambre au moins et dans le salon"="lots", 
                      "Uniquement dans 1 chambre: sur mon lit"="little", 
                      "Uniquement dans 1 chambre: sur mon lit et autour de lui (plinthes, table de nuit etc..)" ="little", 
                      "Uniquement dans le salon: sur le canapé"="little", 
                      "Uniquement dans le salon: sur le canapé et autour de lui (plinthes, etc..)"="little"  ),
         start=recode(start,
                      "Il y a 1 à 3 mois"="less3M",
                      "Il y a 3 à 6 mois"="more3M",
                      "Il y a moins de 1 mois"="less3M",
                      "Il y a plus de 6 mois (combien?)"="more3M"
                      ),
         duration=recode(duration,
                      "De 1 à 3 mois"="less3M",
                      "De 3 à 6 mois"="more3M",
                      "Moins de 1 mois"="less3M",
                      "Plus de 6 mois (combien?)"="more3M"
         ),
         prepar=recode(prepar,
                     "Je n’ai pas préparé mon logement pour le traitement, je n’avais pas d’aide pour le faire"="Nil",
                     "J’ai préparé mon logement seul"="Self",
                     .default="Other"
                     
         ),   
         
         educ=recode(educ,
                    "Pré-Bac"="Pre_Bac",
                    "Bac, Bac+1, Bac +2 (ou équivalent)"="Bac",
                    "Bac +3 ou plus"="Post_Bac"
         ),
         pro=recode(pro,
                         "Etudiant"="Nope",
                         "Ne travaille pas"="Nope",
                    .default="yes"
                    
         ),
         fam=recode(fam,
                    "Vit en concubinage, PACSé, Marié"="Rel",
                    "En relation mais vit séparément"="Rel",
                    
                    .default="Nope"

         ),
         IA=recode(IA,
                    "Moins d'une fois par semaine"="Nope",
                    "Une fois par semaine"="Nope",
                    .default="yes"
                    
         ),
         kids=recode(kids,
                   "0"="0",
                   "1"="1",
                   .default="2_"
                   
         ),
        chemsafety2=recode(chemsafety2,
            "Oui"="Oui",
            "Non (pourquoi?)"="Non",
            "Non"="Non",
            ".default"="NoPesticide"

        ),
        chemsafety=recode(chemsafety,
                           "Oui"="Oui",
                           "Non"="Non",
                           ".default"="NoPesticide"
                           
        ),
        psych=recode(psych,
            "Non, jamais"="Nope",
            "Oui, actuellement"="Now",
            "Oui, anciennement"="Before"
            ),
        difficother_prepar=recode(difficother_prepar,
            "Aucune difficulté"="Nope",
              "Beaucoup de difficulté mais réalisé"="Yes",
              "Non réalisé"="Yes",
              "Réalisé avec un peu de difficulté"="Nope",
            .default=NA_character_),
        difficother_accept=recode(difficother_accept,
                                  "Aucune difficulté"="Nope",
                                  "Beaucoup de difficulté mais réalisé"="Yes",
                                  "Non réalisé"="Yes",
                                  "Réalisé avec un peu de difficulté"="Nope",
                                  .default=NA_character_),
        
        difficother_accept_end=recode(difficother_accept_end,
                                  "Aucune difficulté"="Nope",
                                  "Beaucoup de difficulté mais réalisé"="Yes",
                                  "Non réalisé"="Yes",
                                  "Réalisé avec un peu de difficulté"="Nope",
                                  .default=NA_character_),
        support_fam=recode(support_fam,
                                  "Beaucoup"="Yes",
                                  "Moyennement"="Yes",
                                  "Un peu"="No",
                                  "Pas du tout/Absent"="No",
                                  .default=NA_character_),
        support_bailleur=recode(support_bailleur,
                           "Beaucoup"="Yes",
                           "Moyennement"="Yes",
                           "Un peu"="No",
                           "Pas du tout/Absent"="No",
                           .default=NA_character_),
        support_desinsect=recode(support_desinsect,
                           "Beaucoup"="Yes",
                           "Moyennement"="Yes",
                           "Un peu"="No",
                           "Pas du tout/Absent"="No",
                           .default=NA_character_),
        Pesticide=case_when(Pesticides == "Non" | chemsafety == "NoPesticide" | chemsafety2 == "NoPesticide" ~ "No",
                            chemsafety == "Oui" & chemsafety2 == "Non" ~ "OnlyRules",
                            chemsafety == "Oui" & chemsafety2 == "Oui" ~ "Rules&Health",
                            chemsafety == "Non" ~ "NoRules",
                            TRUE ~ NA_character_
                            )
        ) %>%
  mutate(pct_NA = rowSums(is.na(select(., -L_WEMWBS)))/ncol(select(., -L_WEMWBS))
         )  %>% # better than mutate(pct_NA=rowSums(is.na(.))/ncol(.)) 

  filter(L_stress>16 & pct_NA<=0.3)# filter(L_stress>16 & pct_NA<=0.3)# filter(L_WEMWBS==14 & pct_NA<=0.3)#

# check new Pesticide variable
tmp = anal3 %>% select(c(Pesticide, Pesticides, chemsafety, chemsafety2))
table(tmp$Pesticide)
anal3=anal3 %>% select(-chemsafety,-chemsafety2,-Pesticides)

## Flowchart of missing data ----
#comment one of the last 2 filter commands, or both
nrow(anal3)

hist(anal3$pct_NA)#pct NA per row
hist(anal3$L_WEMWBS, 50)


## Table 1 ----
# check that no variable has more than 30% of missing values

table(anal3$age)
anal3 %>% select(where(is.numeric)) %>% purrr::map(summary)
anal3 %>% select(-where(is.numeric)) %>% purrr::map(table)

table1(~ .,
       #| PdL_infect,
       data=anal3,  
       rowlabelhead = "Variables",
       overall=T,# extra.col=list(`P-value`=pvalue)
       render.continuous=render.NEW
) -> Table1_final
Table1_final

lm(sum_WEMWBS ~ .,anal3 %>% select(-c(average_stress,pct_NA,starts_with("L_"))) %>% 
     na.omit()) %>% summary()
lm(average_stress ~ .,anal3 %>% select(-c(sum_WEMWBS,pct_NA,starts_with("L_"))) %>% 
     na.omit()) %>% summary()

## Confirm the reality of a bedbug infestation ----
var_confirm=c("recognize", "dxPdL", "confirm")
all_pop=anal3 %>% # filter(PdL_infect=="Oui") %>% 
  select(all_of(var_confirm))
all_confirm=all_pop %>%
  filter(recognize=="PdL" | 
           dxPdL=="insect" |
           confirm=="yes"
  )
anti_join(all_pop, all_confirm)
#confirm_PdL = oui
#Are there cases where they have seen bed bugs or feces but 1 and 3 are = 0?
all_confirm=all_pop %>%
  filter(#recognize=="PdL" &
           (dxPdL=="insect" & recognize=="PdL") |
           confirm=="yes"
  )
anti_join(all_pop, all_confirm)
who_sure <- anal3 %>%
  filter(#recognize=="PdL" &
    (dxPdL=="insect" & recognize=="PdL") |
      confirm=="yes"
  )

#pct participants with a cutoff for PTSD > 1.5
anal3 %>% filter(average_stress>1.5)
anal3 %>% filter(average_stress>1.5) %>% nrow() /  anal3 %>% filter(!is.na(average_stress)) %>% nrow()


#look up data
anal3 %>% select(-where(is.numeric)) %>% purrr::map(table)
anal3 %>% select(where(is.numeric)) %>% purrr::map(summary)

anal3 %>% is.na() %>% colSums()/nrow(anal3)

idx_NA_WEMWBS=is.na(anal3$sum_WEMWBS)

#anal3 <- who_sure
save(anal3,file=paste0(Sys.Date(),"_raw_df.RData"))

# Impute ----
do_impute <- function(anal3) {
anal3 = anal3 %>% select(-c(starts_with("L_"),pct_NA)) %>%
  mutate_if(is.character,as.factor)
N_imp=5
# options(na.action='na.pass')
# tmp=model.matrix( ~ .,anal3) %>% as.data.frame() %>% select(-"(Intercept)");colSums(is.na(tmp))

colSums(is.na(anal3))
set.seed(12345)
anal3_AllImp = mice::mice(anal3, 
                        m=N_imp, method = "pmm") %>%
  mice::complete("long")
}
anal3_AllImp <- do_impute(anal3)
#anal3_AllImp <- do_impute(who_sure)

colSums(is.na(anal3_AllImp))
save(anal3_AllImp, file=paste0(Sys.Date(),"anal3_AllImp.RData"))
