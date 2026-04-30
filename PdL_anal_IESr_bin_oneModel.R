rm(list=ls())
library(caret)
library(catboost)
library(dplyr)
library(tidyverse)

options(warn = -1)

date_reg="2024-10-27" #"2024-02-29" "2024-10-27"
load(file=paste0(date_reg,"anal3_AllImp.RData"))
N_imp=5

#anal3_AllImp$weights <- ifelse(anal3_AllImp$sex == "Féminin", 0.246/0.667, 1)

cutoff=1.5

# predictors and outcomes ----
colnames(anal3_AllImp)
#.imp, .id, recognizePdL, either sum_WEMWBS OR average_stress
predictors=anal3_AllImp %>% 
  select(-all_of(c(".imp",".id","sum_WEMWBS","average_stress","recognize","IA"))) %>% #, "weights"
  colnames()
outcome="average_stress"#"average_stress" ; "sum_WEMWBS"


# caret and fastshap functions ----
N_inner=10#10, 25
tuneGrid=expand.grid(
  usekernel = TRUE,
  laplace = seq(0.5,10,0.5), # Laplace for non-zero probs
  adjust = 0 # Bandwidth, only when usekernel=TRUE
)
tuneGrid=NULL
tuneLength=10
L_tune=10
metric="ROC"#AUCROC
search="random"
metric2="AUC"#PR-AUC
method='naive_bayes' #catboost.caret earth ranger glmnet RRF svmRadial svmLinear glm naive_bayes
method2='ranger'
preProcess=c("corr","nzv")

seeds <- vector(mode = "list", length = N_inner+1)
for(i in 1:N_inner) seeds[[i]] <- seq(10^(i-1),10^(i-1)+L_tune-1,by=1)
## For the last model:
seeds[[N_inner+1]] <- 12345

trControl_bin = function(outcome_vector) {
trainControl(
  method = "boot", #repeats=1,
  number = N_inner,
  search = search,
  summaryFunction = twoClassSummary,#prSummary, twoClassSummary,
  savePredictions = 'final',
  index=createResample(outcome_vector, N_inner),#a list with elements for each resampling iteration. 
  #Each list element is a vector of integers corresponding to the rows used for training at that iteration.
  indexOut=NULL,#a list (the same length as index) that dictates which data are held-out for each resample
  #If NULL, then the unique set of samples not contained in index is used.
  indexFinal = NULL, #which samples are used to fit the final model after resampling. 
  #If NULL, then entire data set is used.
  seeds=seeds,
  classProbs = TRUE,
  preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5, freqCut = 95/5, uniqueCut =
                          10, cutoff = 0.9),
  predictionBounds = rep(FALSE, 2),
  adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE),
)
}

trControl_bin2 = function(outcome_vector) {
  trainControl(
    method = "boot", #repeats=1,
    number = N_inner,
    search = search,
    summaryFunction = multiClassSummary,#prSummary, twoClassSummary,
    savePredictions = 'final',
    index=createResample(outcome_vector, N_inner),#a list with elements for each resampling iteration. 
    #Each list element is a vector of integers corresponding to the rows used for training at that iteration.
    indexOut=NULL,#a list (the same length as index) that dictates which data are held-out for each resample
    #If NULL, then the unique set of samples not contained in index is used.
    indexFinal = NULL, #which samples are used to fit the final model after resampling. 
    #If NULL, then entire data set is used.
    seeds=seeds,
    classProbs = TRUE,
    preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5, freqCut = 95/5, uniqueCut =
                            10, cutoff = 0.9),
    predictionBounds = rep(FALSE, 2),
    adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE)
  )
}

calculateMMetrics <- function(y_true,y_hat) {
  out=postResample(y_hat,y_true)
  return(out)
}

p_function<- function(object, newdata) {
  caret::predict.train(object,newdata = newdata, type="prob")[,"Yes"]
}

p_functionClustA <- function(object, newdata) {
  caret::predict.train(object,newdata = newdata, type="prob")[,"A"]
}
p_functionClustB <- function(object, newdata) {
  caret::predict.train(object,newdata = newdata, type="prob")[,"B"]
}
p_functionClustC <- function(object, newdata) {
  caret::predict.train(object,newdata = newdata, type="prob")[,"C"]
}

# Declare variables for recording ----
all_imp_hyperparams=list()
all_imp_importance=list()
all_imp_SHAP=list()
all_imp_ytrue=list()
all_imp_yhat=list()
all_imp_probs=list()
all_imp_pred=list()

do_fastshap=TRUE
i_imp=1

# loop over imputed datasets ----
for (i_imp in 1:N_imp) {
  
  message(paste0("Doing analysis on imputation ",i_imp))
  
  anal3_OneImp=anal3_AllImp %>% filter(.imp==i_imp) %>% 
    filter(!is.na(outcome))
  if (outcome=="sum_WEMWBS") {
    anal3_OneImp=anal3_OneImp[!idx_NA_WEMWBS,]
    anal3_OneImp=anal3_OneImp %>% 
      mutate(sum2=case_when(sum_WEMWBS<=42~"low",
                            sum_WEMWBS>=60~"high",
                            sum_WEMWBS<60 & sum_WEMWBS>42 ~"med")
             )
    #outcome="sum2"
    table(anal3_OneImp$sum2,useNA = "always")
    }
  
  Nfolds=nrow(anal3_OneImp)
  set.seed(12345)
  idx_fold=caret::createFolds(anal3_OneImp %>% dplyr::select(outcome) %>% unlist(), 
                              k=Nfolds, 
                              returnTrain = TRUE)
  #Nfolds=5#comment here
  
  hyper_params_all=list()
  y_true_all=list()
  y_hat_all=list()
  prob_all=list()
  importance_all=list()
  shap_M1=list()
  i_fold=1
  
# loop over folds ----
  for (i_fold in 1:Nfolds){
    print(i_fold)
    message("doing training")
    
    #binarise outcome
    if (outcome=="sum_WEMWBS") {
      y=as.factor(anal3_OneImp[idx_fold[[i_fold]],"sum2"])
    } else {
      y=anal3_OneImp[idx_fold[[i_fold]],] %>%  dplyr::select(outcome) %>%
      mutate(bin_outcome=ifelse(average_stress > cutoff, "Yes", "No")) %>%
      dplyr::select(bin_outcome) %>% unlist()
    }
    
    set.seed(12345)
    hyper_params <- caret::train(y=y,
                                 x=dplyr::select(anal3_OneImp[idx_fold[[i_fold]],],
                                                 all_of(predictors)),#predictors
                                 tuneGrid=tuneGrid,
                                 tuneLength=tuneLength,
                                 metric=metric,
                                 method=method,
                                 #importance="permutation",
                                 preProcess=NULL,
                                 trControl=trControl_bin(y),
                                 weights=NULL#dplyr::select(anal3_OneImp[idx_fold[[i_fold]],],weights) %>% unlist
    )

    hyper_params
    hyper_params$bestTune#best model obtained from caret
    hyper_params$results#perf of all models tested by caret
    
    message("predicting on remaining observation")

    importance_all[[i_fold]]=varImp(hyper_params, scale=TRUE)$importance 
    
    #predict test set with model obtained from caret
    if (outcome=="sum_WEMWBS") {
      anal3_OneImp[-idx_fold[[i_fold]],] %>%  
        dplyr::select(outcome) -> y_true_all[[i_fold]]   
    } else {
      anal3_OneImp[-idx_fold[[i_fold]],] %>%  dplyr::select(outcome) %>%
        mutate(bin_outcome=ifelse(average_stress > cutoff, "Yes", "No")) %>%
        dplyr::select(bin_outcome) %>% unlist() -> y_true_all[[i_fold]]   
    }
    
    newdata=dplyr::select(anal3_OneImp[-idx_fold[[i_fold]],],
                          all_of(predictors))
    y_hat_all[[i_fold]]=predict(hyper_params,newdata=newdata,type = "raw")
    prob_all[[i_fold]]=predict(hyper_params,newdata=newdata,type = "prob")
    hyper_params_all[[i_fold]]=hyper_params
    
    ## record SHAP ----
    if (do_fastshap) {
    message("doing fastshap")
    set.seed(12345)
    (shap_M1[[i_fold]] <- fastshap::explain(object=hyper_params,
                                           X = dplyr::select(anal3_OneImp[idx_fold[[i_fold]],],all_of(predictors)),
                                           pred_wrapper = p_function,
                                           newdata=dplyr::select(anal3_OneImp[-idx_fold[[i_fold]],],all_of(predictors)),
                                           nsim = 1000, 
                                           adjust = TRUE
                                           ))
   
    
    }
    
  }
  
  (hyper_params_all -> all_imp_hyperparams[[i_imp]])
  #importance -> all_imp_importance[[i_imp]]
  all_imp_SHAP[[i_imp]]=do.call(rbind,shap_M1)
  all_imp_ytrue[[i_imp]]=do.call(rbind,y_true_all)
  all_imp_yhat[[i_imp]]=do.call(rbind,y_hat_all)
  all_imp_probs[[i_imp]]=do.call(rbind,prob_all) %>% t()
  
  #record importance
  ##########(importance_all[[i_imp]]=do.call(cbind,importance) %>% rowMeans(na.rm=TRUE) %>% sort())  
     #%>% as.data.frame() %>% arrange(desc(.)) %>% slice_head(n=20))

  #evaluate model
  calculate_here=FALSE
  if (calculate_here) {
  y_true_all=do.call(rbind,y_true_all)
  y_hat_all=do.call(rbind,y_hat_all); y_hat_all=ifelse(y_hat_all==1,"No","Yes")
  (prob_all=do.call(rbind,prob_all)); #prob_all=t(prob_all)
  dat=data.frame(obs=factor(y_true_all),
                  pred=factor(y_hat_all),
                  Yes=prob_all[,"Yes"],
                  No=prob_all[,"No"]) -> all_imp_pred[[i_imp]]
  
  twoClassSummary(data=dat,
                  lev = c("No","Yes")
                  )
 
  
  ### AUC graph ----
  library(pROC)
  probs=vector(length=Nfolds)
  for (i in 1:Nfolds) {
    probs[i]=ifelse(as.vector(y_true_all)[i]=="Yes",prob_all[i,"Yes"],1-prob_all[i,"No"])
  }
  roc(as.vector(y_true_all), probs)
  ggroc(roc(as.vector(y_true_all), probs)) + 
    theme_minimal() + ggtitle("My ROC curve") + 
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")
  
  
    
    }

  
  
  
  
  
  
  
  
  
  
  # Supervised clustering ----
  supervised_clustering=FALSE
  if (supervised_clustering) {
  mp2_3=anal3_AllImp %>% 
    filter(.imp==i_imp) %>% 
    filter(!is.na(outcome)) %>%
    dplyr::select(-c(.imp,.id,recognizePdL, sum_WEMWBS))%>% 
    mutate(bin_outcome=ifelse(average_stress > cutoff, "Yes", "No"))
  predictors
  print(head(mp2_3))
  
  
  ## calculate shapley values on whole dataset ----
  SHAP_on_whole=TRUE
  if (SHAP_on_whole) {
  set.seed(12345)
  y=mp2_3[,"bin_outcome"]
  hyper_params_M2[[i_imp]] <- caret::train(y=y,#outcome
                                           x=dplyr::select(mp2_3,all_of(predictors)),
                                           method = method,
                                           preProcess=preProcess,
                                           tuneGrid = tuneGrid,
                                           tuneLength=tuneLength,
                                           trControl = trControl_bin(y),
                                           metric = metric,
                                           maximize=TRUE #, importance="permutation"
  )
  hyper_params_M2[[i_imp]]$bestTune#best model obtained from caret
  hyper_params_M2[[i_imp]]$results#perf of all models tested by caret
  caret::varImp(hyper_params_M2[[i_imp]])
  
  warnings()
  
  #SHAP all
  set.seed(12345)
  shap_M2[[i_imp]] <- fastshap::explain(hyper_params_M2[[i_imp]],#[[kFold]],
                                        X = dplyr::select(mp2_3,all_of(predictors)),
                                        pred_wrapper = p_function,
                                        nsim = 10)
  
  shap_M2[[i_imp]] %>%  purrr::map(summary)
  
  # shap_M2[[i_imp]] %>% as.data.frame() %>% tidyr::pivot_longer(cols=everything(),
  #                                          names_to = "var",
  #                                          values_to="shap") %>%
  # ggplot() +
  #   geom_violin(aes(x=0,y="shap")) +
  #   facet_grid(~ var)
  }
  
  ## remove noisy values ----
  #remove shap values that are uninformative
  is_big = function(x) max(x)>0.05
  PP=shap_M2[[i_imp]] %>% preProcess("nzv","corr") #SHAP_all
  df=predict(PP,shap_M2[[i_imp]]) %>% as.data.frame() %>% select(where(is_big))
  PP=do.call(rbind,shap_M1) %>% preProcess("nzv","corr") #SHAP_all
  df=predict(PP,do.call(rbind,shap_M1)) %>% as.data.frame() %>% select(where(is_big))
  
  df %>%  purrr::map(summary)
  #df=shap_values_all
  
  ## mclust (model based cluster) ----
  try_mclust=TRUE
  if (try_mclust) {
    library(mclust)
    set.seed(12345)
    res.km=Mclust(df,G=3)
    res.km=Mclust(df)
    
    #clust_r[[i_imp]]=res.km
    summary(res.km)
    res.km$cluster=res.km$classification
    print(table(res.km$cluster))
    df$cluster=res.km$cluster
    #df$SHAP_sum=rowSums(df %>% select(-cluster))
  }
  
  ## attribute each obs its (SHAP) cluster membership ----
  mp2_4=mp2_3
  mp2_4$cluster=res.km$cluster
  mp2_4 %>% group_by(cluster) %>% summarise_if(is.numeric, mean, na.rm = TRUE)  %>% t()
  table(mp2_4$cluster)
  
  num2str=function(x)  chartr("123456789", "ABCDEFGHI", x)
  mp2_4=mp2_4 %>% mutate_at("cluster",~ as.factor(num2str(.)))
  table(mp2_4$cluster)
  
  ## Classify raw obs----
  set.seed(12345)
  colnames(mp2_4) <- make.names(colnames(mp2_4))
  Nfolds=nrow(anal3_OneImp)
  idx_fold=createFolds(mp2_4$cluster, k=Nfolds, returnTrain = TRUE)
  y_hat=list()
  probs=list()
  hyper_params_clust=list()
  shap_M3_A=list()
  shap_M3_B=list()
  shap_M3_C=list()
  for (i_fold in 1:Nfolds) { 
    print(i_fold)
    y=mp2_4[idx_fold[[i_fold]],"cluster"]
    set.seed(12345)
    message("training")
    hyper_params_clust[[i_fold]] <- train(y=y, 
                                          x=mp2_4[idx_fold[[i_fold]],] %>% 
                                            select(all_of(predictors[predictors %in% colnames(mp2_4)])), 
                                          method=method2,#naive_bayes ranger
                                          metric=metric2, 
                                          trControl=trControl_bin2(y),
                                          tuneLength=tuneLength
                                          ) 
    hyper_params_clust[[i_fold]]
    # varImp(hyper_params_clust[[i_fold]])
    
    # plot(hyper_params_clust[[i_fold]]$finalModel)
    # text(hyper_params_clust[[i_fold]]$finalModel)
    
    message("predicting")
    y_hat[[i_fold]]=predict(hyper_params_clust[[i_fold]],mp2_4[-idx_fold[[i_fold]],])
    probs[[i_fold]]=predict(hyper_params_clust[[i_fold]],mp2_4[-idx_fold[[i_fold]],],"prob")
    
    ## record SHAP ----
    if (do_fastshap) {
      message("doing fastshap")
      set.seed(12345)
      shap_M3_A[[i_fold]] <- fastshap::explain(object=hyper_params_clust[[i_fold]],
                                             X = dplyr::select(mp2_4[idx_fold[[i_fold]],],all_of(predictors)),
                                             pred_wrapper = p_functionClustA,
                                             newdata=dplyr::select(mp2_4[-idx_fold[[i_fold]],],all_of(predictors)),
                                             nsim = 10) %>%
        cbind(cluster="A")
      
      set.seed(12345)
      shap_M3_B[[i_fold]] <- fastshap::explain(object=hyper_params_clust[[i_fold]],
                                               X = dplyr::select(mp2_4[idx_fold[[i_fold]],],all_of(predictors)),
                                               pred_wrapper = p_functionClustB,
                                               newdata=dplyr::select(mp2_4[-idx_fold[[i_fold]],],all_of(predictors)),
                                               nsim = 10) %>%
        cbind(cluster="B")
      
      set.seed(12345)
      shap_M3_C[[i_fold]] <- fastshap::explain(object=hyper_params_clust[[i_fold]],
                                               X = dplyr::select(mp2_4[idx_fold[[i_fold]],],all_of(predictors)),
                                               pred_wrapper = p_functionClustC,
                                               newdata=dplyr::select(mp2_4[-idx_fold[[i_fold]],],all_of(predictors)),
                                               nsim = 10) %>%
        cbind(cluster="C")
    }

  }#end of folds loop
  
  y_hat_all=  do.call(rbind,y_hat);
  y_hat_all=y_hat_all %>%
    as.data.frame() %>%
    mutate(V1=recode(V1,'1'="A",
                        '2'="B",
                        '3'="C",
                        '4'="D",
                        '5'="E",
                        '6'="F",
                        '7'="G",
                        '8'="H",
                        '9'="I")
    ) %>%
    rename(pred=V1)

  probs_all=  do.call(rbind,probs)
  
  ## Classification performance ----
  Classif_perf=multiClassSummary(data=data.frame(obs=as.factor(mp2_4[,"cluster"]),
                                    pred=as.factor(y_hat_all$pred)) %>% cbind(probs_all),
                                 lev = colnames(probs_all))
  
  tmp=do.call(rbind,shap_M3_A) %>% select(-cluster)
  tmp=do.call(rbind,shap_M3_B) %>% select(-cluster)
  tmp=do.call(rbind,shap_M3_C) %>% select(-cluster)
  
  play_with_SHAP=FALSE 
  if (play_with_SHAP) {
    #Importance plot
    tmp %>% as.data.frame() %>% tidyr::pivot_longer(cols = everything(),
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
    
    
    # Individual plot per feature
    data_One_Way=tmp %>% as.data.frame() %>% tidyr::pivot_longer(cols = everything(),
                                                                 names_to = "Feature",
                                                                 values_to = "SHAP value") %>%
      dplyr::group_by(Feature)
    
    X_matrix_One_Way <- dplyr::select(anal3_OneImp,all_of(predictors)) %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "Feature",
                          values_to = "Feature value") #%>% filter(Feature %in% data_One_Way$Feature)
    
    data_bind <- cbind(data_One_Way,X_matrix_One_Way[,2])
   
    data_bind$Feature = factor(data_bind$Feature,
                               levels = levels(data_imp$Feature))
    tmp2=cbind(tmp,anal3_OneImp)
    
    
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
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
      #facet_wrap(~Feature, nrow=5, ncol=2,scales = "free_x")
      facet_wrap(~Feature, nrow=7, ncol=6,scales = "free_x")
    
    
    # Force plot
    tmp %>% as.data.frame() %>%
      slice_max(contamfear_noOui,n=1) %>%
      tidyr::pivot_longer(cols = everything(),#ends_with("_SHAP"),
                          names_to = "Feature",
                          values_to = "Feature contribution") %>%
      arrange(desc(abs(`Feature contribution`))) %>%
      slice_max(`Feature contribution`,n=10)
    
  }
  
  }
  
  
  ## 
  
}#end of imptutation loop


Sys.Date() 
save.image(file=paste0(Sys.Date(),"output_all_imp_NB.RData"))
