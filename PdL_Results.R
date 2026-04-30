rm(list=ls())

library(caret)
library(dplyr)
library(shapviz)
library(mice)
library(sjPlot)

date_of_anal="2024-10-27"
date_of_anal2="2024-10-27"
#"2024-10-25" Sex weights #"2024-03-01" No weights #"2024-10-27" no weights but sure of dx

algo="NB"
load(paste0(date_of_anal2,"output_all_imp_",algo,".RData"))
load(paste0(date_of_anal,"_raw_df.RData"))
load(file=paste0(date_of_anal,"anal3_AllImp.RData"))
head(anal3_AllImp)

Length_df=121->Nfolds#121or 138
i_imp=1
N_imp=5

shv = list()
imp=list()

# Bivariate anal results ----

set.seed(12345)
anal3_miced = mice::mice(anal3 %>% select(-c(starts_with("L_"),pct_NA)) %>%
                           mutate_if(is.character,as.factor) , 
                          m=N_imp, method = "pmm") 
colSums(is.na(anal3_miced %>% complete()))
anal3_miced$data$average_stress=ifelse(anal3_miced$data$average_stress>1.5,1,0)
unique(anal3_miced$data$psych);unique(anal3_miced$data$prepar);unique(anal3_miced$data$contamfear_no);
anal3_miced$data$educ=relevel(anal3_miced$data$educ,ref="Pre_Bac")#8
anal3_miced$data$psych=relevel(anal3_miced$data$psych,ref="Nope")#8
anal3_miced$data$prepar=relevel(anal3_miced$data$prepar,ref="Self")#14
anal3_miced$data$fin=relevel(anal3_miced$data$fin,ref="non")#20
anal3_miced$data$contamfear_no=relevel(anal3_miced$data$contamfear_no,ref="Oui")#27
anal3_miced$data$socialrestrict_no=relevel(anal3_miced$data$socialrestrict_no,ref="Oui")#28
anal3_miced$data$socialrestrictfromot_no=relevel(anal3_miced$data$socialrestrictfromot_no,ref="Oui")#29

out1="average_stress"
out2="sum_WEMWBS"
predictors_miced=anal3_miced$data %>% 
  select(-c(average_stress,sum_WEMWBS,IA,recognize)) %>%
  colnames()
out=out1; family=binomial();exponentiate=TRUE
coefft_df=list()

for (i in c(6,8,14,20,27,28,29)) {#length(predictors_miced)
  var=predictors_miced[i]
formula=paste0(out, " ~ ", var)
fit_model_mi = function(formula,family) {
  with(data = anal3_miced, 
       exp= glm(as.formula(formula),
                family=family
       )
  )
}

coefft_df[[i]]=summary(pool(fit_model_mi(formula, family)),
                       conf.int = TRUE, 
                       exponentiate = exponentiate)[-1,c(1,2,7,8,6)] 
coefft_df[[i]]=coefft_df[[i]][,1] %>%
  cbind(round(coefft_df[[i]][,c(-1)],2))
}
do.call(rbind,coefft_df) %>% tab_df()

# AUC graph ----

all_imp_roc=list()
all_imp_probs2=list()

for (i_imp in 1:N_imp) {
  
all_imp_yhat[[i_imp]]=ifelse(all_imp_yhat[[i_imp]]==1,"No","Yes")
all_imp_probs[[i_imp]]=t(all_imp_probs[[i_imp]])

  dat=data.frame(obs=factor(all_imp_ytrue[[i_imp]]),
                 pred=factor(all_imp_yhat[[i_imp]]),
                 Yes=all_imp_probs[[i_imp]][,"Yes"],
                 No=all_imp_probs[[i_imp]][,"No"]) 
  
  twoClassSummary(data=dat,
                  lev = c("No","Yes")
  )

  library(pROC)
  probs=vector(length=Length_df)
    for (i in 1:Nfolds) {
      probs[i]=ifelse(as.vector(all_imp_ytrue[[i_imp]])[i]=="Yes",
                      all_imp_probs[[i_imp]][i,"Yes"],
                      1-all_imp_probs[[i_imp]][i,"No"])
    }
  all_imp_probs2[[i_imp]]=probs
  (all_imp_roc[[i_imp]]=pROC::roc(as.vector(all_imp_ytrue[[i_imp]]), probs))
  
}

# TO MAKE AN AVERAGE AUC CURVE
# predictions_100_samples <- data.frame(
#   Sample = rep(c(1:100), times = 195),
#   PredictionValues = sample(seq(0,1,length.out=19500)),
#   RealClass = c(rep("benign", times = 9750), rep("pathogenic", times = 9750))
# )
# 
# predictions_100_samples=data.frame(RealClass=do.call(c,all_imp_ytrue),
#                                    PredictionValues = do.call(c,all_imp_probs2),  
#                                    Sample = rep(c(1:Nfolds), times = N_imp)
# )
#            
# library(cutpointr)
# library(tidyverse)
# mean_roc <- function(data, cutoffs = seq(from = -5, to = 5, by = 0.5)) {
#   map_df(cutoffs, function(cp) {
#     out <- cutpointr(data = data, x = PredictionValues, class = RealClass,
#                      subgroup = Sample, method = oc_manual, cutpoint = cp
#                      
#                      )
#     data.frame(cutoff = cp, 
#                sensitivity = mean(out$sensitivity),
#                specificity = mean(out$specificity))
#   })
# }
# 
# mr <- mean_roc(data=predictions_100_samples)
  
all_imp_roc

ggroc(all_imp_roc, linetype=1, legacy.axes = FALSE) +
    scale_colour_manual(values = c("black", "black", "black", "black", "black")) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
    theme(legend.position = "none",
          legend.text=element_text(size=11),
          axis.text = element_text(size=15),
          axis.title = element_text(size = 17),
          panel.border = element_rect(colour = "black", fill=NA),
          axis.line.y.right  = element_line(colour="black"),
          panel.background = element_rect(fill = "white",colour = "black"),
          strip.text = element_blank(),
          strip.background = element_rect(fill = "white",colour = "black"),
          plot.title = element_text(colour = "black",face="bold", size=20)#,hjust = 0.5)#family, 
    ) 


want_cold_encode=FALSE
if (want_cold_encode) {
  
  
cold_encode <- function(df, encoded_prefix, keep_dummies = FALSE) {
  var <- sym(encoded_prefix)
  df <-
    df %>%
    rowwise() %>%
    mutate({{ var }} := which.max(c_across(starts_with(encoded_prefix)))) %>%
    ungroup %>%
    mutate({{ var }} := factor({{ var }}))
  if (!keep_dummies) {
    df <-
      df %>% select(-matches(paste0(encoded_prefix,1:9)))
  }
  return(df)
}

cold_encode <- function(df, encoded_prefix, keep_dummies = FALSE) {
  var <- sym(encoded_prefix)
  df <-
    df %>%
    rowwise() %>%
    mutate(age=case_when(sum(c_across(starts_with(encoded_prefix)))>1~10,
                                 TRUE~which.max(c_across(starts_with(encoded_prefix)))
                                 )
           ) 
  if (!keep_dummies) {
    df <-
      df %>% select(-matches(paste0(encoded_prefix,1:9)))
  }
  return(df)
}

  anal3_OneImp_cold_encode=cold_encode(anal3_OneImp, "age") %>%
    cold_encode("kids") %>%
    cold_encode("educ") %>%
    cold_encode("psych") %>%
    cold_encode("prepar") %>%
    cold_encode("chemsafety2") %>% rename(otherchemsafety=chemsafety2) %>%
    cold_encode("chemsafety") %>% rename(chemsafety2=otherchemsafety)
  
  table(anal3_OneImp_cold_encode$age, useNA = "always"); table(anal3_OneImp$age3)
  table(anal3_OneImp_cold_encode$kids, useNA = "always"); table(anal3_OneImp$kids1); table(anal3_OneImp$kids2_)
  table(anal3_OneImp_cold_encode$educ, useNA = "always"); table(anal3_OneImp$educPost_Bac); table(anal3_OneImp$educPre_Bac)
  
  table(anal3_OneImp_cold_encode$psych, useNA = "always"); table(anal3_OneImp$psychNope); table(anal3_OneImp$psychNow)
  table(anal3_OneImp_cold_encode$prepar, useNA = "always"); table(anal3_OneImp$preparOther); table(anal3_OneImp$preparSelf)
  table(anal3_OneImp_cold_encode$chemsafety, useNA = "always"); table(anal3_OneImp$chemsafetyNoPesticide); table(anal3_OneImp$chemsafetyOui)
  table(anal3_OneImp_cold_encode$chemsafety2, useNA = "always"); table(anal3_OneImp$chemsafety2); table(anal3_OneImp$chemsafety2)
}


# SHAP anal ----



## Importance plot ----


### Manual ----
    SHAP_compiled %>% as.data.frame() %>% tidyr::pivot_longer(cols = everything(),
                                                              names_to = "Feature",
                                                              values_to = "SHAP value") %>%
      dplyr::group_by(Feature) %>% #  dplyr::group_by(PSR,Feature) %>%
      dplyr::summarise(mean=mean(abs(`SHAP value`))) %>% 
      #dplyr::summarise(mean=mean(`SHAP value`)) %>% 
      
      dplyr::arrange(desc(mean)) %>%
      dplyr::mutate(Feature=factor(Feature,levels=as.character(Feature))) %>%
      ggplot()+
      geom_col(aes(y=Feature,x=mean))+#group = PSR, fill = PSR),position="stack")+
      xlab("Average impact on model output magnitude")+
      labs(title="Importance Plot") + 
      theme(legend.text=element_text(size=11),
            axis.text = element_text(size=13),
            axis.title = element_text(size = 15),
            panel.background = element_rect(fill = "white",colour = "black"),
            strip.background = element_rect(fill = "white",colour = "black"),
            plot.title = element_text(colour = "black",face="bold", size=20)#,hjust = 0.5)#family, 
      ) +
      labs(title="Importance Plot") + 
      scale_y_discrete(limits=rev) +  scale_x_continuous(expand = c(0,0))
    
    

### Importance with shapviz  ----
find_X_mat=function(i_imp) {
  anal3_AllImp %>% filter(.imp==i_imp) %>% 
    dplyr::select(all_of(predictors)) %>% 
    mutate(age=recode(age,
                      "1"="<25",
                      "2"="25-35",
                      "3"="35-45",
                      "4"="45-55",
                      "5"=">55"),
           socialrestrict_no=recode(socialrestrict_no,#reverse scale
                                    "Non"="Yes",
                                    "Oui"="No"),
           kids=recode(kids,
                       "2_"="2+"),
           where=recode(where,
                        "little"="1",
                        "lots"="2+"),
           effective=recode(effective,
                            "nope"="No",
                            "ok"="Yes"),
           fam=recode(fam,
                      "Nope"="No",
                      "Rel"="Yes"),
           contamfear_no=recode(contamfear_no, #reverse scale
                                "Non"="Yes",
                                "Oui"="No"),
           Pesticide=recode(Pesticide,
                            "No"="No",
                            "OnlyRules"="Cd",
                            "Rules&Health"="Health",
                            "NoRules"="No expl"),
           sex=recode(sex,
                      "Féminin"="Female",
                      "Masculin"="Male"),
           support_fam=recode(support_fam,
                              "No"="No",
                              "Yes"="Yes"),
           difficother_prepar=recode(difficother_prepar,
                                     "Nope"="No"),
           educ=recode(educ,
                       "Bac"="Bac to Bach",
                       "Pre_Bac"="Pre-Bac",
                       "Post_Bac"="Bach +"),
           educ=factor(educ, levels=c("Pre-Bac","Bac to Bach","Bach +")),
           
           socialrestrictfromot_no=recode(socialrestrictfromot_no,#reverse scale
                                          "Oui"="No",
                                          "Non"="Yes"),
           fin=recode(fin,
                      "bon"="Yes",
                      'non'="No"),
           psych=recode(psych,
                        "Before"="Past",
                        "Nope"="No",
                        "Now"="Current"),
           psych=factor(psych,levels=c("No","Past","Current"))
           
           
           
    )
}

p=NULL
for (i_imp in 1:N_imp) {
    
    shv[[i_imp]] <- shapviz(all_imp_SHAP[[i_imp]],
                    X = find_X_mat(i_imp),
                    #baseline = 0.986,
                    interactions = TRUE
    )
    imp[[i_imp]]=sv_importance(shv[[i_imp]], 'no')
    
    #p=p+sv_importance(shv[[i_imp]])
}   
    
# change labels in all_imp_SHAP and in anal3
shv_all=do.call(rbind,shv);colnames(shv_all)

pretty_col=c('Age','Sex','Employment','In a relationship','Children','Education',
             #'Regular Interactions',
             'Mdc issues', 'Psych issues', 
             'Traces', 'Confirmation','Spread of infestation','Start of infestation','Duration of infestation',
             'Preparation', 'Heat or cold treatment',
             'Destruction of materials', 'Had to move','Steam trt', 'Trt effectiveness', 'Financial support',
             'Diff w/ preparation', 
             'Diff w/ acceptance (start)','Diff w/ acceptance (end)', 'Support from friends/fam', 
             'Support from landlord', 'Support from pest cont', 'Fear to contaminate others',
             'Social isolation (self)', 
             'Social isolation (others)', 'Pesticide trt'
)
colnames(shv_all)=pretty_col

sv_importance(shv_all) + 
  theme(legend.position = "none",
        legend.text=element_text(size=11),
        axis.text = element_text(size=13),
        axis.title = element_text(size = 15),
        strip.text = element_text(size=13),
        panel.background = element_rect(fill = "white",colour = "black"),
        strip.background = element_rect(fill = "white",colour = "black"),
        plot.title = element_text(colour = "black",face="bold", size=17) ,
        #aspect.ratio = 0.85
  )


colnames(shv_all)=pretty_col
sv_importance(shv_all, kind="beeswarm", colour="red") + 
  theme(#legend.position = "none",
        legend.text=element_text(size=11),
        axis.text = element_text(size=13),
        axis.title = element_text(size = 15),
        strip.text = element_text(size=13),
        panel.background = element_rect(fill = "white",colour = "black"),
        strip.background = element_rect(fill = "white",colour = "black"),
        plot.title = element_text(colour = "black",face="bold", size=17) ,
        #aspect.ratio = 0.85
  )


imp_average=sv_importance(shv_all,'no') 
#take the 10 most important variables 
Ten_most=sort(imp_average, decreasing=TRUE) [1:15]

# imp_average=data.frame(Feature=names(imp_average), values=imp_average)
# 
# imp_all=do.call(rbind,imp) %>% as.data.frame() %>%
#   tidyr::pivot_longer(cols = everything(),
#                       names_to="Feature",
#                       values_to="values") %>%
#   group_by(Feature) %>%
#   summarise(lower=min(values), higher=max(values))
# 
# imp_all=imp_average %>% 
#   left_join(imp_all, by="Feature")
# 
# ggplot(imp_all,aes(Feature, values)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=lower, ymax=higher))

### shapviz & collapse ----
    collapse= list(age=c("age2","age3","age4","age5"),
                   kids=c("kids1","kids2_"),
                   educ=c("educPost_Bac","educPre_Bac"),
                   psych=c("psychNope","psychNow"),
                   prepar=c("preparOther","preparSelf"),
                   chemsafety2=c("chemsafety2NoPesticide","chemsafety2Oui"),
                   chemsafety=c("chemsafetyNoPesticide","chemsafetyOui")
                   
                   )
    X=dplyr::select(anal3,c(age,kids,educ,psych,prepar,chemsafety2,chemsafety)) %>%
      cbind(dplyr::select(anal3_OneImp,c(all_of(predictors),
                                         -all_of(starts_with("age")),
                                         -all_of(starts_with("kids")),
                                         -all_of(starts_with("educ")),
                                         -all_of(starts_with("psych")),
                                         -all_of(starts_with("prepar")),
                                         -all_of(starts_with("chemsafety2")),
                                         -all_of(starts_with("chemsafety"))
                                         
                                         )
                          )
            )
    
    shv <- shapviz(all_imp_SHAP[[i_imp]],
                   X = X,
                   #baseline = baseline,
                   collapse = collapse)
    sv_importance(shv)

    
  


    
    
    
## SHAP One Way ----
    
### Manual ----
    data_One_Way=all_imp_SHAP[[i_imp]] %>% as.data.frame() %>% tidyr::pivot_longer(cols = everything(),
                                                                 names_to = "Feature",
                                                                 values_to = "SHAP value") %>%
      dplyr::group_by(Feature)
    
    X_matrix_One_Way <- anal3_AllImp %>% filter(.imp==i_imp) %>% 
      dplyr::select(all_of(predictors)) %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "Feature",
                          values_to = "Feature value") #%>% filter(Feature %in% data_One_Way$Feature)
    
    data_bind <- cbind(data_One_Way,X_matrix_One_Way[,2])
    # data_bind$Feature = factor(data_bind$Feature,
    #                            levels = levels(data_imp$Feature))
    # tmp2=cbind(tmp,anal3_OneImp)
    
    
    ggplot(data=data_bind, aes(x=`Feature value`,y=`SHAP value`)) +#, colour=as.factor(`Feature value`))) +
      geom_point() + 
      geom_hline(yintercept = 0, colour='grey', linetype = 'dotdash')+
      #geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dotdash') +
      labs(title="One Way Dependence Plot")+#,": One Way Dependence Plot"))+#,colour="Feature value") +
      theme(legend.position = "none",
            legend.text=element_text(size=11),
            axis.text = element_text(size=13),
            axis.title = element_text(size = 15),
            strip.text = element_text(size=13),
            panel.background = element_rect(fill = "white",colour = "black"),
            strip.background = element_rect(fill = "white",colour = "black"),
            plot.title = element_text(colour = "black",face="bold", size=17) ,
            #aspect.ratio = 0.85
      ) +
     # scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
      #facet_wrap(~Feature, nrow=5, ncol=2,scales = "free_x")
      facet_wrap(~Feature, nrow=7, ncol=6,scales = "free_x")
    
    
    
    
### One Way with shapviz ----
    shv=list()
    find_X_mat=function(i_imp) {
      anal3_AllImp %>% filter(.imp==i_imp) %>% 
        dplyr::select(all_of(predictors)) %>% 
        mutate(age=recode(age,
                          "1"="<25",
                          "2"="25-35",
                          "3"="35-45",
                          "4"="45-55",
                          "5"=">55"),
               socialrestrict_no=recode(socialrestrict_no,#reverse scale
                                        "Non"="Yes",
                                        "Oui"="No"),
               kids=recode(kids,
                           "2_"="2+"),
               where=recode(where,
                            "little"="1 room",
                            "lots"="2+ rooms"),
               effective=recode(effective,
                                "nope"="No",
                                "ok"="Yes"),
               fam=recode(fam,
                          "Nope"="No",
                          "Rel"="Yes"),
               contamfear_no=recode(contamfear_no, #reverse scale
                                    "Non"="Yes",
                                    "Oui"="No"),
               Pesticide=recode(Pesticide,
                                "No"="No",
                                "OnlyRules"="Cd",
                                "Rules&Health"="Health",
                                "NoRules"="No expl"),
               sex=recode(sex,
                          "Féminin"="Female",
                          "Masculin"="Male"),
               support_fam=recode(support_fam,
                                  "No"="No",
                                  "Yes"="Yes"),
               difficother_prepar=recode(difficother_prepar,
                                         "Nope"="No"),
               educ=recode(educ,
                           "Bac"="Bac to Bach",
                           "Pre_Bac"="Pre-Bac",
                           "Post_Bac"="Bach +"),
               educ=factor(educ, levels=c("Pre-Bac","Bac to Bach","Bach +")),
               
               socialrestrictfromot_no=recode(socialrestrictfromot_no,#reverse scale
                                              "Oui"="No",
                                              "Non"="Yes"),
               fin=recode(fin,
                          "bon"="Yes",
                          'non'="No"),
               psych=recode(psych,
                            "Before"="Past",
                            "Nope"="No",
                            "Now"="Current"),
               psych=factor(psych,levels=c("No","Past","Current")),
               prepar=recode(prepar,
                             "Nil"="Not done"),
               prepar=factor(prepar,levels=c("Self","Other","Not done")),
               dxPdL=recode(dxPdL,
                            "insect"='Yes',
                            'oth'='No'),
               start=recode(start,
                            'less3M'='< 3M',
                            'more3M'='> 3M'),
               etuvage=recode(etuvage,
                              'Non'='No',
                              'Oui'='Yes'),
               moving=recode(moving,
                           'Non'='No',
                           'Oui'='Yes'),
               temperature=recode(temperature,
                          'Non'='No',
                          'Oui'='Yes'),
               pro=recode(pro,
                          'Nope'='No',
                          'yes'='Yes'),
               Destruction=recode(Destruction,
                                  'Non'='No',
                                  'Oui'='Yes'),
               difficother_accept=recode(difficother_accept,
                                        'Nope'='No'),
               mdc=recode(mdc,
                          'Non'='No',
                          'Oui'='Yes'),
               confirm=recode(confirm,
                              'no'='No',
                              'yes'='Yes'),
               difficother_accept_end=recode(difficother_accept_end,
                                       'Nope'='No'),
               duration=recode(duration,
                               'less3M'='< 3M',
                               'more3M'='> 3M')
               )
    }
    
    for (i_imp in 1:N_imp) {
      
      shv[[i_imp]] <- shapviz(all_imp_SHAP[[i_imp]],
                              X = find_X_mat(i_imp),
                              #baseline = 0.986,
                              interactions = TRUE
      )
      imp[[i_imp]]=sv_importance(shv[[i_imp]], 'no')
    } 
    
    shv_all=do.call(rbind,shv);colnames(shv_all)
    
    pretty_col=c('Age','Sex','Employment','In a relationship','Children','Education',
                 #'Regular Interactions',
                 'Mdc issues', 'Psych issues', 
                 'Traces of insects', 'Confirmation','Spread of infestation','Start of infestation','Duration of infestation',
                 'Preparation', 'Heat or cold trt',
                 'Destruction of materials', 'Had to move','Steam trt', 'Trt effectiveness', 'Financial support',
                 'Diff w/ preparation', 
                 'Diff w/ acceptance (start)','Diff w/ acceptance (end)', 'Support from friends/fam', 
                 'Support from landlord', 'Support from pest cont', 'Fear to contaminate others',
                 'Social isolation (self)', 
                 'Social isolation (others)', 'Pesticide trt'
    )
    colnames(shv_all)=pretty_col
    imp_average=sv_importance(shv_all,'no') 
    #take the 10 most important variables 

    Ten_most=sort(imp_average, decreasing=TRUE) [1:15]
    Remaining=sort(imp_average, decreasing=TRUE) [16:30]
    
    x <- names(Ten_most)#Ten_most Remaining
    x <- c("Social isolation (self)","Children","Spread of infestation","Trt effectiveness","In a relationship",
                  "Fear to contaminate others","Age","Pesticide trt","Sex","Support from friends/fam",
                  "Education","Diff w/ preparation","Social isolation (others)", "Financial support", "Psych issues")
    
    p=list()
    for (i in 1:length(x)) {
    p[[i]]=sv_dependence(shv_all, v = x[i], color_var = NULL, jitter_width=NULL) + 
      labs(title = x[i])+
      theme(legend.position = "none",
            legend.text=element_text(size=11),
            axis.text = element_text(size=13),
            axis.title.y = element_blank(),#element_text(size = 15),
            axis.title.x = element_blank(),
            strip.text = element_text(size=13),
            panel.background = element_rect(fill = "white",colour = "black"),
            strip.background = element_rect(fill = "white",colour = "black"),
            plot.title = element_text(colour = "black",face="bold", size=17, hjust=0.5) ,
            #aspect.ratio = 0.5
      ) 
    }
    library(patchwork)
    
    p=wrap_plots(p,ncol=3);p=p & ylim(-0.3,0.3)
    p
    
    
    
    #### Interaction plots ----
    #Family life
    v="Social restrictions";potential_interactions(shv_all,v);sv_dependence(shv_all,v) ####
    v="Children";potential_interactions(shv_all,v);sv_dependence(shv_all,v)
    v="Nb of rooms with bedbugs";potential_interactions(shv_all,v);sv_dependence(shv_all,v) ####
    v="Effectiveness";potential_interactions(shv_all,v);sv_dependence(shv_all,v) ####
    v="Family life";potential_interactions(shv_all,v);sv_dependence(shv_all,v) ####
    v="Issues w/ preparation";potential_interactions(shv_all,v);sv_dependence(shv_all,v)####
    v="Restrictions from others";potential_interactions(shv_all,v);sv_dependence(shv_all,v)####
    v="Age";potential_interactions(shv_all,v);sv_dependence(shv_all,v)
    
    v="Education";potential_interactions(shv_all,v);sv_dependence(shv_all,v)#NOPE
    v="Psych hx";potential_interactions(shv_all,v);sv_dependence(shv_all,v)####
    v="Fear of contamination";potential_interactions(shv_all,v);sv_dependence(shv_all,v) #NOPE
    v="Pesticides";potential_interactions(shv_all,v);sv_dependence(shv_all,v,"Children") #NOPE
    v="Support from fam";potential_interactions(shv_all,v);sv_dependence(shv_all,v)#NOPE
    potential_interactions(shv_all,"Sex");sv_dependence(shv_all,"Sex","Family life")
    v="Financial help";potential_interactions(shv_all,v);sv_dependence(shv_all,v,"Family life")####
    
    
    # 
    x=c("Support from friends/fam","Support from landlord", "Support from pest cont",
        "Financial support","In a relationship")
    xbis=c("Effectiveness","Nb of rooms with bedbugs",
        "Family life")
    
    v="Social restrictions";sv_dependence(shv_all,v,x) ####
    v="Restrictions from others";sv_dependence(shv_all,v,x) ####
    v="Age";sv_dependence(shv_all,v,x) ####
    v="Children";sv_dependence(shv_all,v,x) ####
    v="Spread of infestation";sv_dependence(shv_all,v,x) ####
    v="Trt effectiveness";sv_dependence(shv_all,v,x) ####
    v="Family life";sv_dependence(shv_all,v,x) ####
    v="Issues w/ preparation";sv_dependence(shv_all,v,x) ####
    

    library(patchwork)
    library(ggplot2)
    design="
    AAAABBBBCCCC
    ##DDDDEEEE##
    "
  
    for (i in 1:length(x)) {
      p[[i]]=sv_dependence(shv_all, v = v,  jitter_width=NULL, color_var = x[i]) + 
        labs(title = x[i])+
        theme(legend.position = c(0.8,0.2) ,#"none",
              legend.text=element_text(size=13),
              legend.title=element_blank(),#element_text(size=13),
              
              axis.text = element_text(size=13),
              axis.title.y = element_blank(),#element_text(size = 15),
              axis.title.x = element_blank(),
              strip.text = element_text(size=13),
              panel.background = element_rect(fill = "white",colour = "black"),
              strip.background = element_rect(fill = "white",colour = "black"),
              plot.title = element_text(colour = "black",face="bold", size=17, hjust=0.5) ,
              #aspect.ratio = 0.5
        ) 
    }
    # p[[1]]+p[[2]]+p[[3]]+p[[4]]+p[[5]]+
    #   plot_layout(design=design)
    p[[1]]+p[[2]]+p[[3]]+p[[5]]
    #  p[[4]]
    
    v="Fear of contamination";sv_dependence(shv_all,v,x) #NOPE
    v="Pesticides";sv_dependence(shv_all,v,x) #NOPE
    v="Support from fam";sv_dependence(shv_all,v,x) #NOPE
    v="Financial help";sv_dependence(shv_all,v,x) #NOPE
    v="Psych hx";sv_dependence(shv_all,v,x)#NOPE
    v="Education";sv_dependence(shv_all,v,x) #NOPE
    v="Sex";sv_dependence(shv_all,v,x) #NOPE




### shapviz & collapse ----
    collapse= list(age=c("age2","age3","age4","age5"),
                   kids=c("kids1","kids2_"),
                   educ=c("educPost_Bac","educPre_Bac"),
                   psych=c("psychNope","psychNow"),
                   prepar=c("preparOther","preparSelf"),
                   chemsafety2=c("chemsafety2NoPesticide","chemsafety2Oui"),
                   chemsafety=c("chemsafetyNoPesticide","chemsafetyOui")
                   
    )
    X=dplyr::select(anal3,c(age,kids,educ,psych,prepar,chemsafety2,chemsafety)) %>%
      cbind(dplyr::select(anal3_OneImp,c(all_of(predictors),
                                         -all_of(starts_with("age")),
                                         -all_of(starts_with("kids")),
                                         -all_of(starts_with("educ")),
                                         -all_of(starts_with("psych")),
                                         -all_of(starts_with("prepar")),
                                         -all_of(starts_with("chemsafety2")),
                                         -all_of(starts_with("chemsafety"))
                                         
      )
      )
      )
    
    shv <- shapviz(SHAP_compiled,
                   X = X,
                   #baseline = baseline,
                   collapse = collapse)
    v <- c("socialrestrict_noOui", "age")
    
    sv_dependence(shv,v, color_var = NULL, jitter_width = 0)
    
    
    
    
    
## Force plot ----
    
### Manual ----
    SHAP_compiled %>%  as.data.frame() %>%
      rename_with(~ paste(., "SHAP", sep = "_")) %>%
      #cbind(dplyr::select(anal3_OneImp,all_of(predictors))) %>%
      slice_max(contamfear_noOui_SHAP,n=1) %>%
      tidyr::pivot_longer(cols = ends_with("_SHAP"),
                          names_to = "Feature",
                          values_to = "Feature contribution") %>%
      arrange(desc(abs(`Feature contribution`))) %>%
      slice_max(`Feature contribution`,n=10)


    
### Force plot with shapviz ----
    id=46
    (jack.prob <- p_function(Model, newdata = dplyr::select(anal3_OneImp[id,],all_of(predictors))))
    (baseline <- mean(p_function(Model, newdata = dplyr::select(anal3_OneImp[id,],all_of(predictors)))))  
    (difference <- jack.prob - baseline)

    shv=list()
    
    
    id_max=rowSums(anal3[,"average_stress"]) %>% which.max()
    id_min=rowSums(anal3[,"average_stress"]) %>% which.min()
    
    id_max=rowSums(all_imp_SHAP[[i_imp]]) %>% which.max()
    id_min=rowSums(all_imp_SHAP[[i_imp]]) %>% which.min()
    
    
    find_X_mat=function(i_imp, id) {
      
      anal3_AllImp %>% filter(.imp==i_imp) %>% filter(.id==id) %>% 
        dplyr::select(all_of(predictors)) %>% 
        mutate(age=recode(age,
                          "1"="<=25",
                          "2"="25-35",
                          "3"="35-45",
                          "4"="45-55",
                          "5"=">55"),
               socialrestrict_no=recode(socialrestrict_no,#reverse scale
                                        "Non"="Yes",
                                        "Oui"="No"),
               kids=recode(kids,
                           "2_"="2+"),
               where=recode(where,
                            "little"="1",
                            "lots"="2+"),
               effective=recode(effective,
                                "nope"="No",
                                "ok"="Yes"),
               fam=recode(fam,
                          "Nope"="Single",
                          "Rel"="Rel."),
               contamfear_no=recode(contamfear_no, #reverse scale
                                    "Non"="Yes",
                                    "Oui"="No"),
               Pesticide=recode(Pesticide,
                                "No"="No",
                                "OnlyRules"="Rules",
                                "Rules&Health"="Health",
                                "NoRules"="No expl."),
               sex=recode(sex,
                          "Féminin"="Female",
                          "Masculin"="Male"),
               support_fam=recode(support_fam,
                                  "No"="No",
                                  "Yes"="Yes"),
               difficother_prepar=recode(difficother_prepar,
                                         "Nope"="No"),
               educ=recode(educ,
                           "Bac"="Bac to Bach.",
                           "Pre_Bac"="Pre-Bac",
                           "Post_Bac"="Bach.+"),
               educ=factor(educ, levels=c("Pre-Bac","Bac to Bach.","Bach.+")),
               
               socialrestrictfromot_no=recode(socialrestrictfromot_no,#reverse scale
                                              "Oui"="No",
                                              "Non"="Yes"),
               fin=recode(fin,
                          "bon"="Yes",
                          'non'="No"),
               psych=recode(psych,
                            "Before"="Past",
                            "Nope"="No",
                            "Now"="Current"),
               psych=factor(psych,levels=c("No","Past","Current"))
               )
    }
    
      
    for (i_imp in 1:N_imp) {
      
      shv[[i_imp]] <- shapviz(all_imp_SHAP[[i_imp]][id_min,],
                              X = find_X_mat(i_imp,id_min),
                              #baseline = 0.986,
                              interactions = TRUE
      )
    } 
    
    shv_all=do.call(rbind,shv);colnames(shv_all)
    
    pretty_col=c('Age','Sex','Employment','Family life','Children','Education',
                 #'Regular Interactions',
                 'Mdc hx', 'Psych hx', 
                 'Traces', 'Confirmation','Nb of rooms with bedbugs','Start','Duration', 'Preparation', 'Temperature',
                 'Destruction', 'Had to move','Steam', 'Effectiveness', 'Financial help', 'Issues w/ preparation', 
                 'Issues w/ acceptance (start)','Issues w/ acceptance (end)', 'Support from fam', 
                 'Support from landlord', 'Support from ', 'Fear of contamination', 'Social restrictions', 
                 'Restrictions from others', 'Pesticides'
    )
    colnames(shv_all)=pretty_col
    
    
    sv_waterfall(shv_all,max_display = 10)
    
    shv <- shapviz(all_imp_SHAP[[i_imp]][id_min,],
                   X = find_X_mat_id(id_min),
                   #baseline = 0.986
    )
    # change labels in all_imp_SHAP and in anal3
    colnames(shv)=c('Age',colnames(shv[,-1]))
    
    sv_waterfall(shv,max_display = 20L)
    
    shv <- shapviz(all_imp_SHAP[[i_imp]][id,],
                   X = find_X_mat_id(id),
                   #baseline = 0.986
    )
    # change labels in all_imp_SHAP and in anal3
    colnames(shv)=c('Age',colnames(shv[,-1]))
    
    sv_waterfall(shv,max_display = 20L)
    
    
    
### shapviz & collapse ----
    id=4
    shv <- shapviz(all_imp_SHAP[[i_imp]][id,],
                   X = dplyr::select(anal3_AllImp[[i_imp]],all_of(predictors)),
                   #baseline = 0.986
    )
    sv_waterfall(shv,max_display = 20L)
      
    

    

