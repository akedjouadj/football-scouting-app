library(shiny)
options(shiny.maxRequestSize = 100*1024^2)

#-------- functions to use-------------------------------------
#---------------------- partie optimisation de constante--------------------------------
# Fonction Popular_dist_plot--------------------------------------
Popular_dist_plot= function(var, const)
{
  # moyenne et écart-type des variables
  var_mean= mean(Football_upperlevel_var[,var])
  var_std= sd(Football_upperlevel_var[,var])*sqrt(3002/3003)
  
  var_log_std= sd(log(Football_upperlevel_var[,var]+ const))*sqrt(3002/3003)
  var_log_mean= mean(log(Football_upperlevel_var[,var]+ const))
  
  var_sqrt_std= sd(sqrt(Football_upperlevel_var[,var]+const))*sqrt(3002/3003)
  var_sqrt_mean= mean(sqrt(Football_upperlevel_var[,var]+ const))
  
  # id : identité, vns : var none sort, vls: var log sort, vss: var sqrt sort
  id= round((data_popular[,var]-var_mean)/var_std, 2)
  vns= sort(id)
  
  var_log= round((log(data_popular[,var]+ const)-var_log_mean)/var_log_std,2)
  vls= sort(var_log)
  
  var_sqrt= round((sqrt(data_popular[,var]+ const)-var_sqrt_mean)/var_sqrt_std, 2)
  vss= sort(var_sqrt, index.return=T)[[1]]
  
  index= sort(id, index.return=T)[[2]]
  index_log= sort(var_log, index.return=T)[[2]]
  index_sqrt= sort(var_sqrt, index.return=T)[[2]]
  borne= range(c(vls,vss,vns))
  
  # log(x+c)
  plot(vls, rep(2,9), pch=20, col="red", 
       xaxt="n", yaxt="n", ann=F, ylim = c(0,10), xlim = borne)
  abline(h=2, col="blue")
  text(vls, rep(2.3,9), cex=0.6, 
       labels = name_popular[index_log], col="black")
  text(vls, rep(1.7,9), cex=0.6, 
       labels = as.character(vls), col="black")
  
  # sqrt (x+c)
  points(vss, rep(5,9), pch=20, col="red", 
         xaxt="n", yaxt="n", ann=F)
  abline(h=5, col="blue")
  text(vss, rep(5.3,9), cex=0.6, 
       labels = name_popular[index_sqrt], col="black")
  text(vss, rep(4.7,9), cex=0.6, 
       labels = as.character(vss), col="black")
  
  # none
  points(vns, rep(8,9), pch=20, col="red", 
         xaxt="n", yaxt="n", ann=F)
  abline(h=8, col="blue")
  text(vns, rep(8.3,9), cex=0.6, 
       labels = name_popular[index], col="black")
  text(vns, rep(7.7,9), cex=0.6, 
       labels = as.character(vns), col="black")
  
  axis(side=2, at=c(2,5,8), labels= c("log(x+c)","sqrt(x+c)", "none"),
       las=2, cex.axis= 0.8)
  title(var, outer=TRUE, line=-2.25)
}
#---------------------- fin optimisation de constante-----------------------------------------------------------------


#-------------------------- partie requête distance -----------------------------------------------
# pour concevoir la matrice de distance finale
AAD   <- function(x){
  mean(abs(x-median(x)))
}
Range <- function(x){
  diff(range(x))
}

#### standardisation des matrices de distance pour avoir une matrice finale avec des distances comparables entre elles
dist_final <-  function(x1,x2,x3,x4, fun, w1, w2, w3){
  #x1= Manhattan_dist
  #x2= dist_pos1
  #x3= dist_pos2
  #x4= Team_country_dist
  if(fun=="variance empirique"){
    s_man  <- attr(scale(as.vector(as.dist(x1))), "scaled:scale")
    s_pos1 <- attr(scale(as.vector(as.dist(x2))), "scaled:scale")
    s_pos2 <- attr(scale(as.vector(as.dist(x3))), "scaled:scale")
    s_tl   <- attr(scale(as.vector(as.dist(x4))), "scaled:scale")
  }
  else if (fun=="écart absolu médian"){
    s_man  <- AAD(as.vector(as.dist(x1))) # as.dist pour transformer la matrice de distance en un objet de type distance
    s_pos1 <- AAD(as.vector(as.dist(x2))) # as.dist ne considère que la partie utile de la matrice, par ex la partie triangulaire sup stricte
    s_pos2 <- AAD(as.vector(as.dist(x3)))
    s_tl   <- AAD(as.vector(as.dist(x4)))
  }
  else{
    s_man  <- Range(as.vector(as.dist(x1)))
    s_pos1 <- Range(as.vector(as.dist(x2)))
    s_pos2 <- Range(as.vector(as.dist(x3)))
    s_tl   <- Range(as.vector(as.dist(x4)))
  }
  return((w1*x1/s_man)+(w2*x2/s_pos1)+(w2*x3/s_pos2)+(w3*x4/s_tl))
}

shinyServer(function(input, output) {
  #---tableaux de données-------------------------------------
  # ligue et équipe 
  df= reactive({
    Football[, c('Name', 'League', 'Country_score', 'Team', 'Team_score', 'Team_coef')]
  })
  output$lg_et_eq = renderDT({
    df()
  })
  
  output$save_lg_et_eq= downloadHandler(
    filename= function(){
      paste(input$var,'2014-2015', '.csv')
    },
    content= function(filename)
    {
      write.csv(df(), filename)
    }
  )
  
  # position
  df1 = reactive({
    Football[,c(6,11:22,24:38)]
  }) 
  output$pos = renderDT({
    df1()
  })
  output$save_pos= downloadHandler(
    filename= function(){
      paste(input$var,'2014-2015', '.csv')
    },
    content= function(filename)
    {
      write.csv(df1(), filename)
    }
  )
  
  # VNS
  df2 = reactive({
    data.frame(Football$Name, Football_upperlevel_var)
  }) 
  
  output$vns = renderDT({
    df2()
  })
  
  output$save_vns= downloadHandler(
    filename= function(){
      paste(input$var,'2014-2015', '.csv')
    },
    content= function(filename)
    {
      write.csv(df2(), filename)
    }
  )
  
  # VNI
  df3 = reactive({
    data.frame(Football$Name, Football_lowerlevel_var)
  }) 
  output$vni = renderDT({
    df3()
  })
  output$save_vni= downloadHandler(
    filename= function(){
      paste(input$var,'2014-2015', '.csv')
    },
    content= function(filename)
    {
      write.csv(df3(), filename)
    }
  )
  
  # Autres variables
  df4 = reactive({
    Football[, c(6:9,39:40)]
  }) 
  output$autre = renderDT({
    df4()
  })
  output$save_autre= downloadHandler(
    filename= function(){
      paste(input$var,'2014-2015', '.csv')
    },
    content= function(filename)
    {
      write.csv(df4(), filename)
    }
  )
  
  # matrice de distance
  output$matrix_dist=renderDT({
    input$run_3
    isolate({
      read.csv(input$matrix_dist_file$datapath)[1:input$dim, 1:input$dim]
    })
  })
  
  #-------------Optimisation des constantes---------------------------- 
  output$var_dist_popular = renderPlot({
    Popular_dist_plot(input$var_dist_vns, input$var_c)
  })
  
  output$var_summary_id= renderPlot({
    hist(Football_upperlevel_var[,input$var_dist_vns], breaks=30, col="skyblue", xlab="", ylab="", main="Identité")
  })
  
  output$var_summary_sqrt= renderPlot({
    hist(sqrt(Football_upperlevel_var[,input$var_dist_vns]+input$var_c), breaks=30, col="skyblue", xlab="", ylab="", main="Trans-racine-carrée")
  })
  
  output$var_summary_log= renderPlot({
    hist(log(Football_upperlevel_var[,input$var_dist_vns]+input$var_c), breaks=30, col="skyblue", xlab="", ylab="", main="Trans-logarithme")
  })
  
  #----------------Calcul de la matrice de dissimilarité----------------------------------------
  output$matrix_explain=renderText({
    print("En cliquant sur Enregistrer, vous lancer le calcul de la matrice 
          de dissimilarité avec les poids que vous avez entrés. Attention, cette
          opération peut durer jusqu'à 3 minutes !!")
  })
  
  output$save_matrix_dist= downloadHandler(
    filename= function(){
      paste('dist_matrix', '.csv', sep="")
    },
    content= function(filename)
    {
      input$run_1
      isolate({
        ### Final Manhattan Performance distance
        Manhattan_dist <- Other_dist+input$interception_w*INTERCEPTION_dist+input$fouled_w*FOULED_dist+input$foul_w*FOUL_dist+input$offside_w*OFFSIDE_dist+input$clearance_w*CLEARANCE_dist+
          input$unstch_w*UNSTCH_dist+input$disp_w*DISPOSSESSED_dist+input$tackle_w*TACKLE_dist+input$aerial_w*AERIAL_dist+input$dribble_w*DRIBBLE_dist+input$block_w*BLOCK_dist+
          ((4+input$shot_w)/2)*SHOT_dist+((3+input$goal_w)/2)*GOAL_dist+((4+input$pass_w)/2)*PASS_dist+((2+input$keypass_w)/2)*KEYPASS_dist+input$assist_w*ASSIST_dist+
          (input$tackle_w/2)*TACKLE_suc_dist+(input$aerial_w/2)*AERIAL_suc_dist+(input$dribble_w/2)*DRIBBLE_suc_dist+(input$block_w/3)*BLOCK_pro_dist+
          input$shot_w*SHOT_ZONE_pro_dist+(input$goal_w/2)*GOAL_ZONE_pro_dist+(input$goal_w/12)*(GOAL_SYB_suc_dist+
                                                                                                   GOAL_PA_suc_dist+GOAL_OOB_suc_dist)+input$shot_w*SHOT_SITUATION_pro_dist+
          (input$goal_w/2)*GOAL_SITUATION_pro_dist+(input$goal_w/16)*(GOAL_OP_suc_dist+GOAL_C_suc_dist+
                                                                        GOAL_SP_suc_dist+GOAL_PT_suc_dist)+input$shot_w*SHOT_BODYPART_pro_dist+
          (input$goal_w/2)*GOAL_BODYPART_pro_dist+(input$goal_w/16)*(GOAL_RF_suc_dist+GOAL_LF_suc_dist+
                                                                       GOAL_Head_suc_dist+GOAL_Other_suc_dist)+input$shot_w*SHOT_ACCURACY_pro_dist+
          (3/8)*input$goal_w*(GOAL_suc_dist+GOAL_suc1_dist)+input$pass_w*PASS_suc_dist+input$pass_w*PASS_LENGTH_pro_dist+
          (1/4)*input$pass_w*(PASS_Long_suc_dist+PASS_Short_suc_dist)+input$pass_w*PASS_TYPE_pro_dist+
          (1/6)*input$pass_w*(PASS_Cross_suc_dist+PASS_Corner_suc_dist+PASS_Freekick_suc_dist)+
          input$keypass_w*KEYPASS_LENGTH_pro_dist+input$keypass_w*KEYPASS_TYPE_pro_dist+(input$assist_w/2)*ASSIST_pro_dist+
          (input$assist_w/4)*ASSIST_suc_dist+(input$assist_w/20)*(ASSIST_Cross_suc_dist+ASSIST_Corner_suc_dist+
                                                                    ASSIST_Freekick_suc_dist+ASSIST_Thrb_suc_dist+ASSIST_ThrI_suc_dist)
        
        w1<- input$interception_w+input$offside_w+input$fouled_w+input$foul_w+input$unstch_w+input$disp_w+input$clearance_w
        +1.5*input$tackle_w+1.5*input$block_w+1.5*input$aerial_w+1.5*input$dribble_w+2*input$assist_w+(4.5*input$shot_w+2)+ (3.5*input$goal_w+1.5)
        +(4.5*input$pass_w+2)+(2.5*input$keypass_w+1)
        
        d_fin <- dist_final(Manhattan_dist, dist_pos1, dist_pos2, Team_country_dist, fun=input$stand_method, w1=w1, w2= input$position_w, w3= input$team_league_w)
      })
      write.csv(round(as.matrix(d_fin), 2), filename, row.names = F)
    }
  )
  
  #----------------Requête distances----------------------------------------------
  output$entete= renderText({
    print("Ce message d'erreur apparaît car vous n'avez pas encore chargé de fichier.  
          Veuillez charger votre fichier et valider vos choix !")
  })
  
  output$top_nb=renderPrint({
    input$run_2
    isolate({
      index<- which((Football$Age<=input$age_filter) & (Football$Height<=input$height_filter) & (Football$Weight<=input$weight_filter) & (Football$Apps>=input$apps_filter) & (Football$Mins>=input$mins_filter))
      n= which(Football$Name==input$name_player) # trouver l'indice du joueur choisi
      infile=input$matrix_dist_file
      d_fin= read.csv(infile$datapath)
      dist_n_sort= sort(as.numeric(d_fin[n,index]), index.return=T) # ranger par ordre croissant les distances de ce joueur aux autres joueurs
      top_nb= dist_n_sort[[2]][1:input$nb_player] # indices des joueurs les plus proches
      dissimilarity=round(dist_n_sort[[1]][1:input$nb_player],2)
      k=1
      for (i in top_nb){
        for (j in 1:9){
          print(paste(colnames(Football[, c(6,108,109,7:10,39,40)])[j]," : ", Football[index, ][i, c(6,108,109,7:10,39,40)][j], sep=""))
        }
        print(paste("Dissimilarity", " : ", dissimilarity[k], sep=""))
        print("------------------------------------")
        k=k+1
      }
    })
  })
  
})
