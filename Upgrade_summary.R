rm(list=ls())
load("Football1.RData")
attach(Football)


#> str(Football)
#'data.frame':	3003 obs. of  107 variables:
# $ Team_ID             : num  15 15 15 15 15 15 15 15 73 15 ...
# $ Player_ID           : num  38128 15338 25931 70 11981 ...
# $ Country_score       : num  80.4 80.4 80.4 80.4 80.4 ...
# $ Team_score          : num  2.29 2.29 2.29 2.29 2.29 ...
# $ Team_coef           : num  142 142 142 142 142 ...
# $ Name                : Factor w/ 3799 levels "Aar?n","Aaron Cresswell",..: 2665 547 626 1789 1300 1217 2046 1787 1889 630 ...
# $ Height              : num  194 185 178 187 193 188 190 188 176 175 ...
# $ Weight              : num  84 91 75 90 86 77 92 83 66 74 ...
# $ Age                 : num  26 31 25 34 29 29 20 28 26 27 ...
# $ positionText        : Factor w/ 5 levels "Defender","Forward",..: 4 1 1 1 1 1 1 4 4 4 ...
# $ GK                  : num  0 0 0 0 0 0 0 0 0 0 ...
# $ DC                  : num  0 1 0 1 1 0 1 0 0 0 ...
# $ DL                  : num  0 0 1 0 0 1 0 0 0 0 ...
# $ DR                  : num  0 1 1 0 0 0 1 0 1 0 ...
# $ DMC                 : num  1 0 0 0 0 0 0 1 0 0 ...
# $ MC                  : num  0 0 0 0 0 0 0 0 1 1 ...
# $ ML                  : num  0 0 0 0 0 0 0 0 1 1 ...
# $ MR                  : num  0 0 0 0 0 0 0 0 1 1 ...
# $ AMC                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ AML                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ AMR                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ FW                  : num  0 0 0 0 0 0 0 0 1 1 ...
# $ GK1                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ DC1                 : num  0 0 0 1 1 ...
# $ DL1                 : num  0 0 0.966 0 0 ...
# $ DR1                 : num  0 1 0.0345 0 0 ...
# $ DMC1                : num  0.974 0 0 0 0 ...
# $ DML1                : num  0 0 0 0 0 0 0 0 0 0 ...
# $ DMR1                : num  0 0 0 0 0 ...
# $ MC1                 : num  0.0263 0 0 0 0 ...
# $ ML1                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MR1                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ AMC1                : num  0 0 0 0 0 ...
# $ AML1                : num  0 0 0 0 0 0 0 0 0 0 ...
# $ AMR1                : num  0 0 0 0 0 ...
# $ FW1                 : num  0 0 0 0 0 ...
# $ FWL1                : num  0 0 0 0 0 ...
# $ FWR1                : num  0 0 0 0 0 ...
# $ Apps                : num  38 39 29 39 36 17 15 16 29 37 ...
# $ Mins                : num  3357 3510 2484 3510 3020 ...
# $ Tackle_suc          : num  152 90 87 41 34 61 18 20 27 101 ...
# $ Tackle_unsuc        : num  82 14 33 11 12 9 3 10 17 79 ...
# $ Interception        : num  78 41 60 33 27 20 8 10 18 32 ...
# $ Foulgiven           : num  25 29 17 19 19 13 1 4 99 44 ...
# $ Foulcommited        : num  62 45 20 19 26 23 6 14 29 40 ...
# $ offsideGiven        : num  6 4 0 1 0 1 0 0 3 4 ...
# $ offsideWon          : num  2.81 2.82 4.7 22.88 34.94 ...
# $ Clearance           : num  114 137 96 215 221 22 52 16 6 33 ...
# $ Blockshot           : num  13 4 14 34 35 2 5 2 1 5 ...
# $ Blockcross          : num  12 25 22 3 9 1 2 1 4 3 ...
# $ Blockpass           : num  55 18 33 8 7 14 4 14 14 48 ...
# $ Shot_SYB_1          : num  4 5 0 5 4 0 0 1 5 1 ...
# $ Shot_PA_1           : num  5 27 2 16 19 3 2 1 27 15 ...
# $ Shot_OOB_1          : num  18 10 3 1 2 2 1 2 36 26 ...
# $ Shot_OP_2           : num  17 25 5 2 2 5 1 2 55 34 ...
# $ Shot_C_2            : num  0 2 0 1 0 0 0 0 5 0 ...
# $ Shot_SP_2           : num  10 15 0 19 23 0 2 2 8 7 ...
# $ Shot_PT_2           : num  0 0 0 0 0 0 0 0 0 1 ...
# $ Shot_OffT_3         : num  11 23 1 8 8 2 2 3 22 11 ...
# $ Shot_OnP_3          : num  0 0 0 0 0 0 0 0 1 0 ...
# $ Shot_OnT_3          : num  10 12 3 11 7 1 1 1 27 15 ...
# $ Shot_Block_3        : num  6 7 1 3 10 2 0 0 19 16 ...
# $ Shot_RF_4           : num  0 24 5 3 9 0 1 3 50 36 ...
# $ Shot_LF_4           : num  25 5 0 5 0 5 0 0 15 4 ...
# $ Shot_Head_4         : num  2 13 0 14 16 0 2 1 3 2 ...
# $ Shot_Other_4        : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Goal_SYB_1          : num  1 2 0 3 1 0 0 1 2 0 ...
# $ Goal_PA_1           : num  1 3 0 2 1 0 0 0 2 5 ...
# $ Goal_OOB_1          : num  1 0 0 0 0 0 0 0 1 0 ...
# $ Goal_OP_2           : num  1 3 0 0 0 0 0 0 4 4 ...
# $ Goal_C_2            : num  0 0 0 1 0 0 0 0 1 0 ...
# $ Goal_SP_2           : num  2 2 0 4 2 0 0 1 0 0 ...
# $ Goal_PT_2           : num  0 0 0 0 0 0 0 0 0 1 ...
# $ Goal_RF_3           : num  0 3 0 0 2 0 0 1 4 5 ...
# $ Goal_LF_3           : num  2 1 0 3 0 0 0 0 1 0 ...
# $ Goal_Head_3         : num  1 1 0 2 0 0 0 0 0 0 ...
# $ Goal_Other_3        : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Dribblelost         : num  17 25 7 0 0 5 1 1 50 17 ...
# $ Dribblewon          : num  65 31 11 0 3 15 5 7 52 39 ...
# $ UnsTch              : num  20 23 16 4 7 6 0 7 45 44 ...
# $ Dispossessed        : num  50 25 7 1 7 11 0 5 46 58 ...
# $ Aerialwon           : num  84 108 41 106 117 16 25 10 9 31 ...
# $ Aeriallost          : num  47 70 34 42 50 18 12 7 20 38 ...
# $ Pass_LBA_1          : num  133 60 56 69 85 37 23 20 47 215 ...
# $ Pass_LBIA_1         : num  74 91 74 79 86 29 31 16 19 109 ...
# $ Pass_SBA_1          : num  2090 1311 1121 1560 1217 ...
# $ Pass_SBIA_1         : num  268 219 155 102 108 70 25 30 105 337 ...
# $ Pass_CroA_2         : num  5 18 6 2 0 5 0 0 11 66 ...
# $ Pass_CroIA_2        : num  18 73 31 2 3 32 1 2 51 163 ...
# $ Pass_CorA_1         : num  0 0 0 0 0 0 0 0 4 66 ...
# $ Pass_CorIA_1        : num  0 0 0 0 0 0 0 0 6 108 ...
# $ Pass_FKA_1          : num  34 21 16 38 28 22 6 5 6 107 ...
# $ Pass_FKIA_1         : num  2 1 4 2 1 1 0 0 0 25 ...
# $ keyPassLong_1       : num  2 5 1 3 0 1 0 0 4 27 ...
# $ keyPassShort_1      : num  24 33 15 6 5 11 1 2 42 82 ...
# $ keyPassCross_2      : num  2 8 5 1 0 2 0 0 6 33 ...
# $ keyPassCorner_2     : num  0 0 0 0 0 0 0 0 1 27 ...
# $ keyPassThroughball_2: num  1 1 1 0 0 1 0 0 1 15 ...
# $ keyPassFreekick_2   : num  0 0 0 1 0 0 0 0 0 9 ...
#  [list output truncated]


###################################
######### REPRESENTATION ##########
Var_per <- function(x){
  90*(x/Mins)
}

# fonction appliquant l'AMB avec le prior de Serhat, elle normalise aussi les données
zeros <- function(x){
  ## x=count variables
  y <- matrix(NaN, nrow=dim(x)[1], ncol=dim(x)[2])
  for(i in 1:dim(x)[1]){
    if(sum(x[i,])!=0) # on travaille sur les lignes non essentiellement nulles
      {
      for(j in 1: dim(x)[2]){
        if(x[i,j]==0) # on agit sur les éléments nuls de la ligne i (AMB) avec prior de Serhat
        { # on calcule le x[i,j] correspondant avec la formule 3.27
          y[i,j] <- colMeans(x/rowSums(x), na.rm=TRUE)[j]/(1+sum(x[i,]))
        } 
        else{
          next
        }
      }
      m1 <- which(x[i,]!=0)
      m2 <- which(x[i,]==0) # les indices des zéros sur la ligne i
      for(t in m1) # boucle sur les éléments non nuls
      { 
        a1 <- (x[i,t]/sum(x[i,])) # on normalise 
        a2 <- a1*(1-sum(y[i,m2])) # on applique la formule 3.27
        y[i,t] <- ifelse(length(m2)==0, a1, a2) # s'il n'y a aucun élément nul, on normalise sinon on applique l'AMB
      }
    }
    else{
      next
    }
  }
  colnames(y) <- paste(colnames(x), "_pro", sep="")
  return (y)
}

#### 1) INTERCEPTION, FOUL, FOULED, OFFSIDE, CLEARANCE, UNSTCH, DISPOSSESSED ####
INTERCEPTION_per <- Var_per(Interception)
FOULED_per       <- Var_per(Foulgiven)
FOUL_per         <- Var_per(Foulcommited)
OFFSIDE_per      <- Var_per(offsideGiven)
CLEARANCE_per    <- Var_per(Clearance)
UNSTCH_per       <- Var_per(UnsTch)
DISPOSSESSED_per <- Var_per(Dispossessed)

#### 2) TACKLE, AERIAL, DRIBBLE ####
TACKLE       <- Tackle_suc+Tackle_unsuc
TACKLE_per   <- Var_per(TACKLE)
TACKLE_suc   <- zeros(data.frame(Tackle_suc, Tackle_unsuc))[,1] # on considère uniquement le taux de tacle réussi

AERIAL       <- Aerialwon+Aeriallost
AERIAL_per   <- Var_per(AERIAL)
AERIAL_suc   <- zeros(data.frame(Aerialwon, Aeriallost))[,1]

DRIBBLE       <- Dribblewon+Dribblelost
DRIBBLE_per   <- Var_per(DRIBBLE)
DRIBBLE_suc   <- zeros(data.frame(Dribblewon, Dribblelost))[,1]

#### 3) BLOCK ####
BLOCK      <- Blockshot+Blockcross+Blockpass
Block_data <- data.frame(Blockshot, Blockcross, Blockpass)
BLOCK_per  <- Var_per(BLOCK)
BLOCK_pro  <- zeros(Block_data)

#### 3) SHOT AND GOAL ####
SHOT       <- Shot_SYB_1+Shot_PA_1+Shot_OOB_1
SHOT_per   <- Var_per(SHOT)
GOAL       <- Goal_SYB_1+Goal_PA_1+Goal_OOB_1
GOAL_per   <- Var_per(GOAL)

## a)Zone
SHOT_ZONE     <- data.frame(Shot_SYB_1, Shot_PA_1, Shot_OOB_1)
GOAL_ZONE     <- data.frame(Goal_SYB_1, Goal_PA_1, Goal_OOB_1)
SHOT_ZONE_pro <- zeros(SHOT_ZONE)
GOAL_ZONE_pro <- zeros(GOAL_ZONE)
# ratio : buts marqués par zone/ nombre de tirs par zone
GOAL_SYB_suc  <- zeros(data.frame(Goal_SYB_1, Shot_SYB_1-Goal_SYB_1))[,1]        
GOAL_PA_suc   <- zeros(data.frame(Goal_PA_1,  Shot_PA_1-Goal_PA_1))[,1]            
GOAL_OOB_suc  <- zeros(data.frame(Goal_OOB_1, Shot_OOB_1-Goal_OOB_1))[,1]
GOAL_ZONE_suc <- data.frame(GOAL_SYB_suc, GOAL_PA_suc, GOAL_OOB_suc)
# après ces opérations, on obtient encore des valeurs manquantes (NaN) dans nos jeu de données, dues aux essentials zeros



## b) Situation
SHOT_SITUATION     <- data.frame(Shot_OP_2, Shot_C_2, Shot_SP_2, Shot_PT_2)
GOAL_SITUATION     <- data.frame(Goal_OP_2, Goal_C_2, Goal_SP_2, Goal_PT_2)
SHOT_SITUATION_pro <- zeros(SHOT_SITUATION)
GOAL_SITUATION_pro <- zeros(GOAL_SITUATION)
# ratio buts marqués par situation de jeu/ tirs effectués par situation de jeu
GOAL_OP_suc        <- zeros(data.frame(Goal_OP_2, Shot_OP_2-Goal_OP_2))[,1]        
GOAL_C_suc         <- zeros(data.frame(Goal_C_2,  Shot_C_2-Goal_C_2))[,1]            
GOAL_SP_suc        <- zeros(data.frame(Goal_SP_2, Shot_SP_2-Goal_SP_2))[,1]
GOAL_PT_suc        <- zeros(data.frame(Goal_PT_2, Shot_PT_2-Goal_PT_2))[,1]
GOAL_SITUATION_suc <- data.frame(GOAL_OP_suc, GOAL_C_suc, GOAL_SP_suc, GOAL_PT_suc)

## c) Body Parts
SHOT_BODYPART     <- data.frame(Shot_RF_4, Shot_LF_4, Shot_Head_4, Shot_Other_4)
GOAL_BODYPART     <- data.frame(Goal_RF_3, Goal_LF_3, Goal_Head_3, Goal_Other_3)
SHOT_BODYPART_pro <- zeros(SHOT_BODYPART)
GOAL_BODYPART_pro <- zeros(GOAL_BODYPART)
GOAL_RF_suc       <- zeros(data.frame(Goal_RF_3, Shot_RF_4-Goal_RF_3))[,1]        
GOAL_LF_suc       <- zeros(data.frame(Goal_LF_3, Shot_LF_4-Goal_LF_3))[,1]            
GOAL_Head_suc     <- zeros(data.frame(Goal_Head_3, Shot_Head_4-Goal_Head_3))[,1]
GOAL_Other_suc    <- zeros(data.frame(Goal_Other_3, Shot_Other_4-Goal_Other_3))[,1]
GOAL_BODYPART_suc <- data.frame(GOAL_RF_suc, GOAL_LF_suc, GOAL_Head_suc, GOAL_Other_suc)

## d) Accuracy
SHOT_ACCURACY     <- data.frame(Shot_OffT_3, Shot_OnT_3, Shot_Block_3)
SHOT_ACCURACY_pro <- zeros(SHOT_ACCURACY)
# ratio total buts/total tirs
GOAL_suc          <- zeros(data.frame(GOAL, SHOT-GOAL))[,1] 
# ratio total buts/total tirs cadrés
GOAL_suc1         <- zeros(data.frame(GOAL,  Shot_OnT_3-GOAL))[,1]
# taux de buts pas tirs/par tirs cadrés
GOAL_ACCURACY_suc <- data.frame(GOAL_suc, GOAL_suc1)

#### 4) PASS ####
PASS_acc      <- Pass_SBA_1+Pass_LBA_1
PASS_inacc    <- Pass_SBIA_1+Pass_LBIA_1
PASS          <- PASS_acc+PASS_inacc
PASS_per      <- Var_per(PASS)
PASS_suc      <- zeros(data.frame(PASS_acc, PASS_inacc))[,1]

## a) Length
PASS_Long       <- Pass_LBA_1 + Pass_LBIA_1
PASS_Short      <- Pass_SBA_1 + Pass_SBIA_1
PASS_LENGTH_pro <- zeros(data.frame(PASS_Long, PASS_Short))
PASS_Long_suc   <- zeros(data.frame(Pass_LBA_1, Pass_LBIA_1))[,1]
PASS_Short_suc  <- zeros(data.frame(Pass_SBA_1, Pass_SBIA_1))[,1]
PASS_LENGTH_suc <- data.frame(PASS_Long_suc, PASS_Short_suc)

## b) Type
PASS_Cross        <- Pass_CroA_2+Pass_CroIA_2
PASS_Corner       <- Pass_CorA_1+Pass_CorIA_1
PASS_Freekick     <- Pass_FKA_1+Pass_FKIA_1
PASS_Other        <- PASS-(PASS_Cross+PASS_Corner+PASS_Freekick)
PASS_TYPE         <- data.frame(PASS_Cross, PASS_Corner, PASS_Freekick, PASS_Other)
PASS_TYPE_pro     <- zeros(PASS_TYPE)[,1:3]
PASS_Cross_suc    <- zeros(data.frame(Pass_CroA_2, Pass_CroIA_2))[,1]
PASS_Corner_suc   <- zeros(data.frame(Pass_CorA_1, Pass_CorIA_1))[,1]
PASS_Freekick_suc <- zeros(data.frame(Pass_FKA_1,  Pass_FKIA_1))[,1]
PASS_TYPE_suc     <- data.frame(PASS_Cross_suc, PASS_Corner_suc, PASS_Freekick_suc)

#### 5) KEY PASS AND ASSIST ####
KEYPASS      <- keyPassLong_1+keyPassShort_1
KEYPASS_per  <- Var_per(KEYPASS)
ASSIST       <- assistCross+assistCorner+assistThroughball+assistFreekick+assistThrowin+assistOther
ASSIST_per   <- Var_per(ASSIST)
ASSIST_suc   <- zeros(data.frame(ASSIST, KEYPASS))[,1]

## a) Length
KEYPASS_LENGTH_pro <- zeros(data.frame(keyPassLong_1, keyPassShort_1))

## a) Type
KEYPASS_TYPE_data <- data.frame(keyPassCross_2, keyPassCorner_2, keyPassFreekick_2, keyPassThroughball_2, keyPassThrowin_2, keyPassOther_2)
ASSIST_data       <- data.frame(assistCross, assistCorner, assistFreekick, assistThroughball, assistThrowin, assistOther)
KEYPASS_TYPE_pro  <- zeros(KEYPASS_TYPE_data)[,1:5]
ASSIST_pro        <- zeros(ASSIST_data)[,1:5]

# assist= taux de passes clés réussies
ASSIST_Cross_suc  <- zeros(data.frame(assistCross, keyPassCross_2-assistCross))[,1]
ASSIST_Corner_suc <- zeros(data.frame(assistCorner, keyPassCorner_2-assistCorner))[,1]
ASSIST_FK_suc     <- zeros(data.frame(assistFreekick, keyPassFreekick_2-assistFreekick))[,1]
ASSIST_Thrb_suc   <- zeros(data.frame(assistThroughball, keyPassThroughball_2-assistThroughball))[,1]
ASSIST_ThrI_suc   <- zeros(data.frame(assistThrowin, keyPassThrowin_2-assistThrowin))[,1]
ASSIST_TYPE_suc   <- data.frame(ASSIST_Cross_suc, ASSIST_Corner_suc, ASSIST_FK_suc, ASSIST_Thrb_suc, ASSIST_ThrI_suc)

#### Top level count variables
Football_tl_rep <- data.frame(INTERCEPTION_per, FOULED_per, FOUL_per, OFFSIDE_per , CLEARANCE_per, 
                              UNSTCH_per , DISPOSSESSED_per, TACKLE_per, AERIAL_per, DRIBBLE_per, 
                              BLOCK_per, SHOT_per, GOAL_per, PASS_per, KEYPASS_per, ASSIST_per)
#### lower level count variables
Football_ll_rep <- data.frame(TACKLE_suc, AERIAL_suc, DRIBBLE_suc, BLOCK_pro, 
                              SHOT_ZONE_pro, GOAL_ZONE_pro, GOAL_ZONE_suc, 
                              SHOT_SITUATION_pro, GOAL_SITUATION_pro, GOAL_SITUATION_suc, 
                              SHOT_BODYPART_pro, GOAL_BODYPART_pro, GOAL_BODYPART_suc,
                              SHOT_ACCURACY_pro, GOAL_ACCURACY_suc, 
                              PASS_LENGTH_pro, PASS_LENGTH_suc, PASS_TYPE_pro, PASS_TYPE_suc,
                              KEYPASS_LENGTH_pro, KEYPASS_TYPE_pro, 
                              ASSIST_pro, ASSIST_TYPE_suc, ASSIST_suc, PASS_suc)

write.csv(Football_ll_rep, "Football-lowerlevel-var.csv", row.names = FALSE)
write.csv(Football_tl_rep, "Football-upperlevel-var.csv", row.names = FALSE)


###################################
######### TRANSFORMATION ##########

load("Football-upperlevel-var.Rdata")
attach(Football_upperlevel_var)

TACKLE_per_trans       <- log(TACKLE_per+1.9816)
OFFSIDE_per_trans      <- log(OFFSIDE_per+0.0060)
INTERCEPTION_per_trans <- log(INTERCEPTION_per+0.3184)
FOUL_per_trans         <- log(FOUL_per+1.3315)
FOULED_per_trans       <- log(FOULED_per+0.7735)
CLEARANCE_per_trans    <- log(CLEARANCE_per+0.5505)
BLOCK_per_trans        <- log(BLOCK_per+0.7182)
SHOT_per_trans         <- log(SHOT_per+0.3811)
UNSTCH_per_trans       <- log(UNSTCH_per+1.1567)
DISPOSSESSED_per_trans <- log(DISPOSSESSED_per+0.3101)
AERIAL_per_trans       <- log(AERIAL_per+1.3739)
DRIBBLE_per_trans      <- log(DRIBBLE_per+0.4557)
PASS_per_trans         <- log(PASS_per+0.0001)
KEYPASS_per_trans      <- log(KEYPASS_per+0.8093)

Football_tl_trans <- data.frame(INTERCEPTION_per_trans, FOULED_per_trans, FOUL_per_trans, OFFSIDE_per_trans, CLEARANCE_per_trans, 
                                UNSTCH_per_trans, DISPOSSESSED_per_trans, TACKLE_per_trans, AERIAL_per_trans, DRIBBLE_per_trans, 
                                BLOCK_per_trans, SHOT_per_trans, GOAL_per, PASS_per_trans, KEYPASS_per_trans, ASSIST_per)

save(Football_tl_trans, file= "Football-upperlevel-var-trans.Rdata")

Football_tl_not_trans <- data.frame(INTERCEPTION_per, FOULED_per, FOUL_per, OFFSIDE_per, CLEARANCE_per, 
                                    UNSTCH_per, DISPOSSESSED_per, TACKLE_per, AERIAL_per, DRIBBLE_per, 
                                    BLOCK_per, SHOT_per, GOAL_per, PASS_per, KEYPASS_per, ASSIST_per)


###################################
######### STANDARDISATION #########
Aad <- function(x){
  x <- as.matrix(x)
  y <- matrix(NA, ncol=NCOL(x), nrow=NROW(x))
  for(j in 1:NCOL(x)){
    y[,j] <- x[,j]/mean(abs(x[,j]-median(x[,j], na.rm=TRUE)), na.rm=TRUE)
  }
  return(y)
}

pooled.dev <- function(x){
  c <- vector()
  for(j in 1:NCOL(x)){
    c[j] <- mean(abs(x[,j]-median(x[,j], na.rm=TRUE)), na.rm=TRUE)
  }
  y <- x/(mean(c))
  # : à clarifier--(mean(c)*NCOL(x)) ### NCOL(x) represents weights of compositions
  return(y)
}

# stand of other variables
Football_other_stand <- Aad(data.frame(Age, Height, Weight, Apps, Mins))
colnames(Football_other_stand) <- c("Age_stand", "Height_stand", "Weight_stand", "Apps_stand", "Mins_stand")
attach(data.frame(Football_other_stand))
save(Football_other_stand, file="Football-other-var-stand.Rdata")

# stand of trans and not trans var
Football_tl_stand           <- Aad(Football_tl_trans)
Football_tl_stand_not_trans <- Aad(Football_tl_not_trans)

colnames(Football_tl_stand) <- c("INTERCEPTION_per_stand", "FOULED_per_stand", "FOUL_per_stand", 
                                 "OFFSIDE_per_stand", "CLEARANCE_per_stand", "UNSTCH_per_stand",
                                 "DISPOSSESSED_per_stand", "TACKLE_per_stand", "AERIAL_per_stand",
                                 "DRIBBLE_per_stand", "BLOCK_per_stand", "SHOT_per_stand", 
                                 "GOAL_per_stand","PASS_per_stand", "KEYPASS_per_stand", 
                                 "ASSIST_per_stand")
save(Football_tl_stand, file="Football-upperlevel-var-trans-stand.Rdata")

colnames(Football_tl_stand_not_trans) <- c("INTERCEPTION_per_stand_not_trans", "FOULED_per_stand_not_trans", 
                                           "FOUL_per_stand_not_trans", "OFFSIDE_per_stand_not_trans", 
                                           "CLEARANCE_per_stand_not_trans", "UNSTCH_per_stand_not_trans",
                                           "DISPOSSESSED_per_stand_not_trans", "TACKLE_per_stand_not_trans", 
                                           "AERIAL_per_stand_not_trans","DRIBBLE_per_stand_not_trans", 
                                           "BLOCK_per_stand_not_trans", "SHOT_per_stand_not_trans", 
                                           "GOAL_per_stand_not_trans","PASS_per_stand_not_trans", 
                                           "KEYPASS_per_stand_not_trans", "ASSIST_per_stand_not_trans")
save(Football_tl_stand_not_trans, file="Football-upperlevel-var-not-trans-stand.Rdata")

attach(data.frame(Football_tl_stand))
attach(data.frame(Football_tl_stand_not_trans))

#stand of lower level counts var : percentage
load("Football-lowerlevel-var.Rdata")
attach("Football-lowerlevel-var.Rdata")

Football_ll_stand <- data.frame(Aad(TACKLE_suc), Aad(AERIAL_suc), Aad(DRIBBLE_suc), 
                                pooled.dev(BLOCK_pro), pooled.dev(SHOT_ZONE_pro), 
                                pooled.dev(GOAL_ZONE_pro), Aad(GOAL_ZONE_suc), 
                                pooled.dev(SHOT_SITUATION_pro), pooled.dev(GOAL_SITUATION_pro),
                                Aad(GOAL_SITUATION_suc), pooled.dev(SHOT_BODYPART_pro), 
                                pooled.dev(GOAL_BODYPART_pro), Aad(GOAL_BODYPART_suc),
                                pooled.dev(SHOT_ACCURACY_pro), Aad(GOAL_ACCURACY_suc), 
                                pooled.dev(PASS_LENGTH_pro), Aad(PASS_LENGTH_suc),
                                pooled.dev(PASS_TYPE_pro), Aad(PASS_TYPE_suc), 
                                pooled.dev(KEYPASS_LENGTH_pro), pooled.dev(KEYPASS_TYPE_pro), 
                                pooled.dev(ASSIST_pro), Aad(ASSIST_TYPE_suc),  
                                Aad(ASSIST_suc), Aad(PASS_suc))
nm1 <- vector() 
nm  <- names(Football_ll_rep)
for(i in 1:length(nm)){
  nm1[i] <- paste(nm[i], "_stand", sep="")
}
colnames(Football_ll_stand) <- nm1
save(Football_ll_stand, file= "Football-lowerlevel-var-stand.Rdata") 

attach(Football_ll_stand)
summary(Football_ll_stand)
hist(as.matrix(Football_ll_stand$TACKLE_suc_stand), col='skyblue')

##########################################
######### WEIGHTING AND DISTANCE #########

#### Other variables ####
Other_dist <- as.matrix(dist(Football_other_stand), method="manhattan")

#### Top-level ####
INTERCEPTION_dist <- as.matrix(dist(INTERCEPTION_per_stand), method="manhattan")
FOULED_dist       <- as.matrix(dist(FOULED_per_stand),       method="manhattan")
FOUL_dist         <- as.matrix(dist(FOUL_per_stand),         method="manhattan") 
OFFSIDE_dist      <- as.matrix(dist(OFFSIDE_per_stand),      method="manhattan")
CLEARANCE_dist    <- as.matrix(dist(CLEARANCE_per_stand),    method="manhattan")
UNSTCH_dist       <- as.matrix(dist(UNSTCH_per_stand),       method="manhattan")
DISPOSSESSED_dist <- as.matrix(dist(DISPOSSESSED_per_stand), method="manhattan")
TACKLE_dist       <- as.matrix(dist(TACKLE_per_stand),       method="manhattan")
AERIAL_dist       <- as.matrix(dist(AERIAL_per_stand),       method="manhattan")
DRIBBLE_dist      <- as.matrix(dist(DRIBBLE_per_stand),      method="manhattan")
BLOCK_dist        <- as.matrix(dist(BLOCK_per_stand),        method="manhattan")
SHOT_dist         <- as.matrix(dist(SHOT_per_stand),         method="manhattan")
GOAL_dist         <- as.matrix(dist(GOAL_per_stand),         method="manhattan")
PASS_dist         <- as.matrix(dist(PASS_per_stand),         method="manhattan")
KEYPASS_dist      <- as.matrix(dist(KEYPASS_per_stand),      method="manhattan")
ASSIST_dist       <- as.matrix(dist(ASSIST_per_stand),       method="manhattan")

#### Top-level no transformation ####
#### non considéré pour l'instant
INTERCEPTION_not_trans_dist <- as.matrix(dist(INTERCEPTION_per_stand_not_trans), method="manhattan")
FOULED_not_trans_dist       <- as.matrix(dist(FOULED_per_stand_not_trans),       method="manhattan")
FOUL_not_trans_dist         <- as.matrix(dist(FOUL_per_stand_not_trans),         method="manhattan") 
OFFSIDE_not_trans_dist      <- as.matrix(dist(OFFSIDE_per_stand_not_trans),      method="manhattan")
CLEARANCE_not_trans_dist    <- as.matrix(dist(CLEARANCE_per_stand_not_trans),    method="manhattan")
UNSTCH_not_trans_dist       <- as.matrix(dist(UNSTCH_per_stand_not_trans),       method="manhattan")
DISPOSSESSED_not_trans_dist <- as.matrix(dist(DISPOSSESSED_per_stand_not_trans), method="manhattan")
TACKLE_not_trans_dist       <- as.matrix(dist(TACKLE_per_stand_not_trans),       method="manhattan")
AERIAL_not_trans_dist       <- as.matrix(dist(AERIAL_per_stand_not_trans),       method="manhattan")
DRIBBLE_not_trans_dist      <- as.matrix(dist(DRIBBLE_per_stand_not_trans),      method="manhattan")
BLOCK_not_trans_dist        <- as.matrix(dist(BLOCK_per_stand_not_trans),        method="manhattan")
SHOT_not_trans_dist         <- as.matrix(dist(SHOT_per_stand_not_trans),         method="manhattan")
GOAL_not_trans_dist         <- as.matrix(dist(GOAL_per_stand_not_trans),         method="manhattan")
PASS_not_trans_dist         <- as.matrix(dist(PASS_per_stand_not_trans),         method="manhattan")
KEYPASS_not_trans_dist      <- as.matrix(dist(KEYPASS_per_stand_not_trans),      method="manhattan")
ASSIST_not_trans_dist       <- as.matrix(dist(ASSIST_per_stand_not_trans),       method="manhattan")

#### Distance for use in lower level ####
### ces distances servent à appliquer la formule 4.4 sur les compositions manquantes 
SHOT_SYB_dist <- as.matrix(dist(Aad(Var_per(Shot_SYB_1)), method="manhattan"))
SHOT_PA_dist  <- as.matrix(dist(Aad(Var_per(Shot_PA_1)),  method="manhattan"))
SHOT_OOB_dist <- as.matrix(dist(Aad(Var_per(Shot_OOB_1)), method="manhattan"))

SHOT_OP_dist  <- as.matrix(dist(Aad(Var_per(Shot_OP_2)), method="manhattan"))
SHOT_C_dist   <- as.matrix(dist(Aad(Var_per(Shot_C_2)),  method="manhattan"))
SHOT_SP_dist  <- as.matrix(dist(Aad(Var_per(Shot_SP_2)), method="manhattan"))
SHOT_PT_dist  <- as.matrix(dist(Aad(Var_per(Shot_PT_2)), method="manhattan"))

SHOT_RF_dist     <- as.matrix(dist(Aad(Var_per(Shot_RF_4)),    method="manhattan"))
SHOT_LF_dist     <- as.matrix(dist(Aad(Var_per(Shot_LF_4)),    method="manhattan"))
SHOT_Head_dist   <- as.matrix(dist(Aad(Var_per(Shot_Head_4)),  method="manhattan"))
SHOT_Other_dist  <- as.matrix(dist(Aad(Var_per(Shot_Other_4)), method="manhattan"))
SHOT_OnT_dist    <- as.matrix(dist(Aad(Var_per(Shot_OnT_3)),   method="manhattan"))

PASS_Cross_dist     <- as.matrix(dist(Aad(Var_per(Pass_CroA_2+Pass_CroIA_2)), method="manhattan"))
PASS_Corner_dist    <- as.matrix(dist(Aad(Var_per(Pass_CorA_1+Pass_CorIA_1)), method="manhattan"))
PASS_Freekick_dist  <- as.matrix(dist(Aad(Var_per(Pass_FKA_1+Pass_FKIA_1)),   method="manhattan"))

KEYPASS_Cross_dist   <- as.matrix(dist(Aad(Var_per(keyPassCross_2)),       method="manhattan"))
KEYPASS_Corner_dist  <- as.matrix(dist(Aad(Var_per(keyPassCorner_2)),      method="manhattan"))
KEYPASS_Freekick_dist <- as.matrix(dist(Aad(Var_per(keyPassFreekick_2)),    method="manhattan"))
KEYPASS_Thrb_dist     <- as.matrix(dist(Aad(Var_per(keyPassThroughball_2)), method="manhattan"))
KEYPASS_ThrI_dist     <- as.matrix(dist(Aad(Var_per(keyPassThrowin_2)),     method="manhattan"))

#### Lower level ####
### grâce à cette fonction, on détermine une distance entre des compositions même comprenant des zeros essentiels grâce à la formule 4.4 page 80
overall_dist <- function(x, y){
  ## x is the variable to be computed
  ## y is the distance for replacing
  x_dist <- as.matrix(dist(x, method="manhattan"))
  n <- which(is.na(x_dist), arr.ind=TRUE)
  for(i in 1:NROW(n)){
    x_dist[n[i,1],n[i,2]] <- y[n[i,1],n[i,2]]
  }
  return(x_dist)
}

TACKLE_suc_dist     <- overall_dist(TACKLE_suc_stand, TACKLE_dist)
AERIAL_suc_dist     <- overall_dist(AERIAL_suc_stand, AERIAL_dist)
DRIBBLE_suc_dist    <- overall_dist(DRIBBLE_suc_stand, DRIBBLE_dist)
BLOCK_pro_dist      <- overall_dist(Football_ll_stand[,4:6], BLOCK_dist)

SHOT_ZONE_pro_dist  <- overall_dist(Football_ll_stand[,7:9], SHOT_dist)
GOAL_ZONE_pro_dist  <- overall_dist(Football_ll_stand[,10:12], GOAL_dist)
GOAL_SYB_suc_dist   <- overall_dist(GOAL_SYB_suc_stand, SHOT_SYB_dist)
GOAL_PA_suc_dist    <- overall_dist(GOAL_PA_suc_stand,  SHOT_PA_dist)
GOAL_OOB_suc_dist   <- overall_dist(GOAL_OOB_suc_stand, SHOT_OOB_dist)

SHOT_SITUATION_pro_dist  <- overall_dist(Football_ll_stand[,16:19], SHOT_dist)
GOAL_SITUATION_pro_dist  <- overall_dist(Football_ll_stand[,20:23], GOAL_dist)
GOAL_OP_suc_dist   <- overall_dist(GOAL_OP_suc_stand, SHOT_OP_dist)
GOAL_C_suc_dist    <- overall_dist(GOAL_C_suc_stand,  SHOT_C_dist)
GOAL_SP_suc_dist   <- overall_dist(GOAL_SP_suc_stand, SHOT_SP_dist)
GOAL_PT_suc_dist   <- overall_dist(GOAL_PT_suc_stand, SHOT_PT_dist)

SHOT_BODYPART_pro_dist  <- overall_dist(Football_ll_stand[,28:31], SHOT_dist)
GOAL_BODYPART_pro_dist  <- overall_dist(Football_ll_stand[,32:35], GOAL_dist)
GOAL_RF_suc_dist        <- overall_dist(GOAL_RF_suc_stand,    SHOT_RF_dist)
GOAL_LF_suc_dist        <- overall_dist(GOAL_LF_suc_stand,    SHOT_LF_dist)
GOAL_Head_suc_dist      <- overall_dist(GOAL_Head_suc_stand,  SHOT_Head_dist)
GOAL_Other_suc_dist     <- overall_dist(GOAL_Other_suc_stand, SHOT_Other_dist)

SHOT_ACCURACY_pro_dist  <- overall_dist(Football_ll_stand[,40:42], SHOT_dist)
GOAL_suc_dist           <- overall_dist(Football_ll_stand[,40:42], SHOT_dist)
GOAL_suc1_dist          <- overall_dist(Football_ll_stand[,40:42], SHOT_OnT_dist)

PASS_suc_dist         <- as.matrix(dist(PASS_suc_stand, method="manhattan"))
PASS_LENGTH_pro_dist  <- as.matrix(dist(Football_ll_stand[,45:46], method="manhattan"))
PASS_Long_suc_dist    <- overall_dist(PASS_Long_suc_stand, PASS_dist)
PASS_Short_suc_dist   <- as.matrix(dist(PASS_Short_suc_stand, method="manhattan"))

PASS_TYPE_pro_dist     <- as.matrix(dist(Football_ll_stand[,49:51], method="manhattan"))
PASS_Cross_suc_dist    <- overall_dist(PASS_Cross_suc_stand,    PASS_Cross_dist)
PASS_Corner_suc_dist   <- overall_dist(PASS_Corner_suc_stand,   PASS_Corner_dist)
PASS_Freekick_suc_dist <- overall_dist(PASS_Freekick_suc_stand, PASS_Freekick_dist)

KEYPASS_LENGTH_pro_dist <- overall_dist(Football_ll_stand[,55:56], KEYPASS_dist)
KEYPASS_TYPE_pro_dist   <- overall_dist(Football_ll_stand[,57:61], KEYPASS_dist)

ASSIST_pro_dist         <- overall_dist(Football_ll_stand[,62:66], ASSIST_dist)
ASSIST_suc_dist         <- overall_dist(ASSIST_suc_stand,          KEYPASS_dist)

ASSIST_Cross_suc_dist    <- overall_dist(ASSIST_Cross_suc_stand,  KEYPASS_Cross_dist)
ASSIST_Corner_suc_dist   <- overall_dist(ASSIST_Corner_suc_stand, KEYPASS_Corner_dist)
ASSIST_Freekick_suc_dist <- overall_dist(ASSIST_FK_suc_stand,     KEYPASS_Freekick_dist)
ASSIST_Thrb_suc_dist     <- overall_dist(ASSIST_Thrb_suc_stand,   KEYPASS_Thrb_dist)
ASSIST_ThrI_suc_dist     <- overall_dist(ASSIST_ThrI_suc_stand,   KEYPASS_ThrI_dist)

#### FINAL MANHATTAN DISTANCE

### Final Manhattan distance
Manhattan_dist <- Other_dist+INTERCEPTION_dist+FOULED_dist+FOUL_dist+OFFSIDE_dist+CLEARANCE_dist+
                  UNSTCH_dist+DISPOSSESSED_dist+TACKLE_dist+AERIAL_dist+DRIBBLE_dist+BLOCK_dist+
                  2.5*SHOT_dist+1.5*GOAL_dist+2.5*PASS_dist+1.5*KEYPASS_dist+ASSIST_dist+
                  TACKLE_suc_dist+AERIAL_suc_dist+DRIBBLE_suc_dist+BLOCK_pro_dist+
                  SHOT_ZONE_pro_dist+0.5*GOAL_ZONE_pro_dist+(1/12)*(GOAL_SYB_suc_dist+
                  GOAL_PA_suc_dist+GOAL_OOB_suc_dist)+SHOT_SITUATION_pro_dist+
                  (0.5)*GOAL_SITUATION_pro_dist+(1/16)*(GOAL_OP_suc_dist+GOAL_C_suc_dist+
                  GOAL_SP_suc_dist+GOAL_PT_suc_dist)+SHOT_BODYPART_pro_dist+
                  0.5*GOAL_BODYPART_pro_dist+(1/16)*(GOAL_RF_suc_dist+GOAL_LF_suc_dist+
                  GOAL_Head_suc_dist+GOAL_Other_suc_dist)+SHOT_ACCURACY_pro_dist+
                  (3/8)*(GOAL_suc_dist+GOAL_suc1_dist)+PASS_suc_dist+PASS_LENGTH_pro_dist+
                  (1/4)*(PASS_Long_suc_dist+PASS_Short_suc_dist)+PASS_TYPE_pro_dist+
                  (1/6)*(PASS_Cross_suc_dist+PASS_Corner_suc_dist+PASS_Freekick_suc_dist)+
                  KEYPASS_LENGTH_pro_dist+KEYPASS_TYPE_pro_dist+0.5*ASSIST_pro_dist+
                  (0.25)*ASSIST_suc_dist+(1/20)*(ASSIST_Cross_suc_dist+ASSIST_Corner_suc_dist+
                  ASSIST_Freekick_suc_dist+ASSIST_Thrb_suc_dist+ASSIST_ThrI_suc_dist)

Manhattan_not_trans_dist <- Other_dist+INTERCEPTION_not_trans_dist+FOULED_not_trans_dist+FOUL_not_trans_dist+
                            OFFSIDE_not_trans_dist+CLEARANCE_not_trans_dist+UNSTCH_not_trans_dist+
                            DISPOSSESSED_not_trans_dist+TACKLE_not_trans_dist+AERIAL_not_trans_dist+
                            DRIBBLE_not_trans_dist+BLOCK_not_trans_dist+2.5*SHOT_not_trans_dist+
                            1.5*GOAL_not_trans_dist+2.5*PASS_not_trans_dist+1.5*KEYPASS_not_trans_dist+
                            ASSIST_not_trans_dist+TACKLE_suc_dist+AERIAL_suc_dist+DRIBBLE_suc_dist+BLOCK_pro_dist+
                            SHOT_ZONE_pro_dist+0.5*GOAL_ZONE_pro_dist+(1/12)*(GOAL_SYB_suc_dist+
                            GOAL_PA_suc_dist+GOAL_OOB_suc_dist)+SHOT_SITUATION_pro_dist+
                            (0.5)*GOAL_SITUATION_pro_dist+(1/16)*(GOAL_OP_suc_dist+GOAL_C_suc_dist+
                            GOAL_SP_suc_dist+GOAL_PT_suc_dist)+SHOT_BODYPART_pro_dist+
                            0.5*GOAL_BODYPART_pro_dist+(1/16)*(GOAL_RF_suc_dist+GOAL_LF_suc_dist+
                            GOAL_Head_suc_dist+GOAL_Other_suc_dist)+SHOT_ACCURACY_pro_dist+
                            (3/8)*(GOAL_suc_dist+GOAL_suc1_dist)+PASS_suc_dist+PASS_LENGTH_pro_dist+
                            (1/4)*(PASS_Long_suc_dist+PASS_Short_suc_dist)+PASS_TYPE_pro_dist+
                            (1/6)*(PASS_Cross_suc_dist+PASS_Corner_suc_dist+PASS_Freekick_suc_dist)+
                            KEYPASS_LENGTH_pro_dist+KEYPASS_TYPE_pro_dist+0.5*ASSIST_pro_dist+
                            (0.25)*ASSIST_suc_dist+(1/20)*(ASSIST_Cross_suc_dist+ASSIST_Corner_suc_dist+
                            ASSIST_Freekick_suc_dist+ASSIST_Thrb_suc_dist+ASSIST_ThrI_suc_dist)
#save(Manhattan_not_trans_dist, file = "Manhattan_not_trans_dist.RData")
#save(Manhattan_dist, file = "Manhattan_dist.RData")
#load("Manhattan_dist.RData")
#load("C:\\Users\\User\\Desktop\\Semesters\\UCL\\R codes\\Manhattan_not_trans_dist.RData")

###########################################
### POSITION AND TEAM&LEAGUE VARIABLES ####
library(geosphere)

############################
### position 1 distance
position <- function(A){
  # stockage des côtés moyens par position
  coor  <- list(D  = matrix(, nrow=dim(A)[1], ncol=3), 
                DM = matrix(, nrow=dim(A)[1], ncol=3),
                M  = matrix(, nrow=dim(A)[1], ncol=3),
                AM = matrix(, nrow=dim(A)[1], ncol=3),
                FW = matrix(, nrow=dim(A)[1], ncol=3))
  
  xy  <- cbind(a=c(0,0,90), b=c(90,0,0))
  xyz <- matrix(, nrow=dim(A)[1], ncol=3) # stockage du côté et de la position finale moyenne
  A <- as.matrix(A)
  
  for(i in 1:dim(A)[1]){
    
    ### Defender
    d <- c(A[i,1], A[i,2], A[i,3])
    if(sum(d)!=0){
      coor[[1]][i, 1:2] <- geomean(xy, d)
      coor[[1]][i, 3]   <- 0
    }
    else{
      coor[[1]][i, 1:3] <- NA
    }
    
    ## Defensive Midfielder
    dm <- c(A[i,4], A[i,5], A[i,6])
    if(sum(dm)!=0){
      coor[[2]][i, 1:2] <- geomean(xy, dm) # (x,y) dans le grand cercle
      coor[[2]][i, 3]   <- 1 #z (voir tab 4.18 pg 90)
    }
    else{
      coor[[2]][i, 1:3] <- NA
    }
    
    ## Midfielder
    m <- c(A[i,7], A[i,8], A[i,9])
    if(sum(m)!=0){
      coor[[3]][i, 1:2] <- geomean(xy, m)
      coor[[3]][i, 3]   <- 2
    }
    else{
      coor[[3]][i, 1:3] <- NA
    }
    
    ## Attacking Midfielder
    am <- c(A[i,10], A[i,11], A[i,12]) 
    if(sum(am)!=0){
      coor[[4]][i, 1:2] <- geomean(xy, am)
      coor[[4]][i, 3]   <- 3
    }
    else{
      coor[[4]][i, 1:3] <- NA
    }
    
    ## Forward
    fw <- c(A[i,13], A[i,14], A[i,15])
    if(sum(fw)!=0){
      coor[[5]][i, 1:2] <- geomean(xy, fw)
      coor[[5]][i, 3]   <- 4
    }
    else{
      coor[[5]][i, 1:3] <- NA
    }
    
    #coordonnées du centre pour chaque position 
    coor1 <- c(coor[[1]][i,1], coor[[2]][i,1], coor[[3]][i,1], coor[[4]][i,1], coor[[5]][i,1])
    #coordonnées du côté gauche pour chaque position 
    coor2 <- c(coor[[1]][i,2], coor[[2]][i,2], coor[[3]][i,2], coor[[4]][i,2], coor[[5]][i,2])
    #coordonnées du côté droit pour chaque position
    coor3 <- c(coor[[1]][i,3], coor[[2]][i,3], coor[[3]][i,3], coor[[4]][i,3], coor[[5]][i,3])
    
    wt   <- c(sum(d), sum(dm), sum(m), sum(am), sum(fw)) # vecteurs des poids par position
    
    # formule (4.20) page 89
    xyz[i,1] <- as.vector(na.omit(coor1))%*%wt[which(!is.na(coor1))] 
    xyz[i,2] <- as.vector(na.omit(coor2))%*%wt[which(!is.na(coor2))]
    xyz[i,3] <- as.vector(na.omit(coor3))%*%wt[which(!is.na(coor3))]
    
  }
  d1  <- distm(round(xyz[,1:2], 10))/10018754
  d2  <- as.matrix(dist(xyz[,3], diag=TRUE, upper=TRUE, method="manhattan"))
  (d1+d2)/2
}

dist_pos1 <- position(Football[,24:38])
save(dist_pos1, file="dist_pos1.Rdata")

################################
### position 2 distance : binary
position2 <- function(A){
  # coordonnées des positions Y_11, suivant les colonnes de p, resp on a:
  p <- matrix(c(0.5, sqrt(3)/2, 0, #1:DC
                0, 0, 0, #2:DL
                1, 0, 0, #3:DR
                0.5, sqrt(3)/2, 1, #4:DMC 
                0.5, sqrt(3)/2, 2, #5:MC
                0, 0, 2, #6:ML
                1, 0, 2, #7:MR
                0.5, sqrt(3)/2, 3, #8:AMC 
                0, 0, 3, #9:AML
                1, 0, 3, #10:AMR
                0.5, sqrt(3)/2, 4), #11:FWC
              ncol=11, nrow=3)
  
  
  #abc_mean <- matrix(, nrow=dim(A)[1], ncol=dim(A)[1])
  abc_min  <- matrix(, nrow=dim(A)[1], ncol=dim(A)[1])
  
  options(warn=-1)
  for(i in 1: dim(A)[1]){ # boucle sur les joueurs i
    for(j in 1: dim(A)[1]){ #boucle sur les joueurs j
      
      bc <- matrix(, nrow=11, ncol=11)
      for (b in which(A[i,]==1)){ # boucles sur les positions connues du joueur i
        for(c in which(A[j,]==1)){ # boucles sur les positions connues du joueur j
          bc[b, c] <- as.numeric(dist(rbind(p[,b], p[,c]))) # on calcule la distance eucludienne entre ces deux positions
          #print(b)
          #print(c)
        }
      }
      
      # formule 4.23 page 113
      bc1 <- apply(bc, 1, min, na.rm=TRUE)  # pour chaque position connue de i, on trouve la position connue de j la plus proche et on évalue la distance entre les deux positions 
      bc2 <- apply(bc, 2, min, na.rm=TRUE) # la même chose que ci-haut mais de j à i
      r2 <- sum(bc1[is.finite(bc1)])/length(which(A[i,]==1))
      c2 <- sum(bc2[is.finite(bc2)])/length(which(A[j,]==1))
      abc_min [i,j] <- mean(c(r2,c2))
      
      # ces formules ne correspondent pas à celles énoncées à la page 113 (4.23)
      #r1 <- sum(rowMeans(bc, na.rm=TRUE)[which(A[i,]==1 & A[j,]==0)])/length(which(A[i,]==1))
      #c1 <- sum(colMeans(bc, na.rm=TRUE)[which(A[i,]==0 & A[j,]==1)])/length(which(A[j,]==1))
      #abc_mean[i,j] <- mean(c(r1,c1))
    }
    print(i)
  }
  #options(warn=0)
  #list(Mean=abc_mean, Min=abc_min)
  return(abc_min)
}
### cette commande met un temps fou à s'exécuter
require(parallel)
detectCores()

install.packages("doParallel")
require(doParallel)

cl=makeCluster(6)
registerDoParallel(cl)

dist_pos2 <- position2(Football[,12:22])

stopCluster(cl)
save(dist_pos2, file="dist_pos2.Rdata")

############################
### team and league distance
AAD <- function(x){
  mean(abs(x-median(x)))
}

Team_country <- function(A){
  x <- Aad(A[,1]) ###Country_score_stand
  y <- Aad(A[,2]) ###Team_score_stand
  z <- Aad(A[,3]) ###Team_coef_stand
  
  D1     <- matrix( ,nrow=dim(A)[1], ncol=dim(A)[1])
  
  for(i in 1:dim(A)[1]){
    for(j in 1:dim(A)[1]){
      if(is.na(z[i]) | is.na(z[j])){
        D1[i,j] <- abs(x[i]-x[j]+y[i]-y[j])
      }
      else{
        D1[i,j] <- (2/3)*abs(x[i]-x[j]+y[i]-y[j])+(1/3)*abs(z[i]-z[j])
      }
    }
  }
  return(D1)
}
Team_country_dist <- Team_country(Football[, 3:5])
save(Team_country_dist, file = "Team_country_dist.RData")
load("Team_country_dist.Rdata")

#save(Manhattan_dist, dist_pos1, dist_pos2, Team_country_dist, file = "C:\\Users\\SERHAT\\Desktop\\Semesters\\UCL\\R codes\\Alldistances.RData")
#load("C:\\Users\\User\\Desktop\\SAemesters\\UCL\\R codes\\Alldistances.RData")
AAD   <- function(x){
  mean(abs(x-median(x)))
}
Range <- function(x){
  diff(range(x))
}

#### standardisation des matrices de distance pour avoir une matrice finale avec des distances comparables entre elles
dist_final <-  function(x1,x2,x3,x4, fun, FUN=""){
  #x1= Manhattan_dist
  #x2= dist_pos1
  #x3= dist_pos2
  #x4= Team_country_dist
  if(FUN=="scale"){
    s_man  <- attr(fun(as.vector(as.dist(x1))), "scaled:scale")
    s_pos1 <- attr(fun(as.vector(as.dist(x2))), "scaled:scale")
    s_pos2 <- attr(fun(as.vector(as.dist(x3))), "scaled:scale")
    s_tl   <- attr(fun(as.vector(as.dist(x4))), "scaled:scale")
  }
  else{
    s_man  <- fun(as.vector(as.dist(x1))) # as.dist pour transformer la matrice de distance en un objet de type distance
    s_pos1 <- fun(as.vector(as.dist(x2))) # as.dist ne considère que qu'une partie utile de la matrice, par ex la partie triangulaire sup stricte
    s_pos2 <- fun(as.vector(as.dist(x3)))
    s_tl   <- fun(as.vector(as.dist(x4)))
  }
  return((43*x1/s_man)+(15*x2/s_pos1)+(11*x3/s_pos2)+(6*x4/s_tl))
}

#### calcul des distances finales avec différentes techniques de standardisation
### variables transformées
d_fin_AAD <- dist_final(Manhattan_dist, dist_pos1, dist_pos2, Team_country_dist, fun=AAD)
d_fin_Range <- dist_final(Manhattan_dist, dist_pos1, dist_pos2, Team_country_dist, fun=Range)
d_fin_unit <- dist_final(Manhattan_dist, dist_pos1, dist_pos2, Team_country_dist, fun=scale, FUN="scale")
save(d_fin_AAD, file="d_fin_AAD.Rdata")
save(d_fin_Range, file="d_fin_Range.Rdata")
save(d_fin_unit, file="d_fin_unit.Rdata")

### variables non transformées
d_fin_AAD_not_trans <- dist_final(Manhattan_not_trans_dist, dist_pos1, dist_pos2, Team_country_dist, fun=AAD)
d_fin_Range_not_trans <- dist_final(Manhattan_not_trans_dist, dist_pos1, dist_pos2, Team_country_dist, fun=Range)
d_fin_unit_not_trans <- dist_final(Manhattan_not_trans_dist, dist_pos1, dist_pos2, Team_country_dist, fun=scale, FUN="scale")
save(d_fin_AAD_not_trans, file="d_fin_AAD_not_trans.Rdata")
save(d_fin_Range_not_trans, file="d_fin_Range_not_trans.Rdata")
save(d_fin_unit_not_trans, file="d_fin_unit_not_trans.Rdata")

#### Corrélations entre les 4 matrices de distances
all_dist <- data.frame(as.vector(as.dist(Manhattan_dist)), as.vector(as.dist(dist_pos1)), 
                       as.vector(as.dist(dist_pos2)), as.vector(as.dist(Team_country_dist)))
colnames(all_dist) <- c("Performance", "Current Pos", "Historical Pos", "Team_League")
## erreur : rownames(all_dist) <- c("Performance", "Current Pos", "Historical Pos", "Team_League")
cor(all_dist)

#### Histogramme, boxplot et résumé des distances finales
Summary <- function(x, Name=""){
  s        <- round(c(summary(x), sd(x, na.rm=TRUE)),3)
  names(s) <- c(names(summary(x)), "St.Dev")
  layout(matrix(c(1,2,3,3),2,2,byrow = TRUE), c(6,4), c(6,4), TRUE)
  par(mar=c(4, 2, 2, 0.1))
  hist(x, breaks=20, col="lightblue", xlab="", ylab="", main="", cex.axis=2)
  par(mar=c(1,0, 0, 1))
  plot(0, type="n", xaxt="n", yaxt="n", main="", ylab="", xlab="", axes=FALSE)
  legend("left",   cex=1.85, bty = "n", legend=names(s))
  legend("center", cex=1.85, bty = "n", legend=rep(paste(":"),length(s)))
  legend("right",  cex=1.85, bty = "n", legend=s)
  par(mar=c(3.5, 1, 1.5, 1))
  boxplot(x, horizontal=TRUE, col="lightblue", cex.axis=2)
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  title(Name, outer=TRUE, cex.main=2, line=-2.25)
  par(mfrow=c(1,1))
}

par(mfrow=c(3,1))
Summary(as.dist(d_fin_AAD),   "Final distance (AAD)")
Summary(as.dist(d_fin_Range), "Final distance (Range)")
Summary(as.dist(d_fin_unit),  "Final distance (Unit)")
par(mfrow=c(1,1))


#########################################
#### VISULASATION AND COMPARISON ########
library(cluster)
library(smacof)
load("C:/Users/User/Desktop/Semesters/UCL/R codes/Save Data/Football1.RData")
cc <- as.numeric(rownames(as.matrix(FOOT_RESULT_MIN$Distance)))
Football1 <- Football[cc,]
attach(Football1)

s1 <- which(Name=="David Alaba"       | Name=="Sergio Ramos"        | Name=="Luke Shaw"         | Name=="Gerard Piqu?"      |
           Name=="Diego God?n"         | Name=="Ricardo Rodriguez"   | Name=="Leonardo Bonucci"   | Name=="David Luiz"        |
           Name=="Thiago Silva"        | Name=="Arda Turan"          | Name=="Jerome Boateng"     | Name=="Javier Mascherano" |
           Name=="Giorgio Chiellini"   |  Name=="Arturo Vidal"       |
           Name=="?lvaro Morata"       | Name=="Kevin De Bruyne"    | Name=="Ivan Rakitic"      |
           Name=="Eden Hazard"         | Name=="Hakan Calhanoglu"    | Name=="Andr?s Iniesta"     | Name=="Paul Pogba"        | 
           Name=="James Rodr?guez"     | Name=="Luis Su?rez"         | Name=="Alexis S?nchez"     | Name=="Sergio Ag?ero"     |
           Name=="Lionel Messi"        | Name=="Cristiano Ronaldo"   | Name=="Neymar"             | Name=="Gareth Bale"       |
           Name=="Thomas M?ller"       | Name=="Zlatan Ibrahimovic"  | Name=="Antoine Griezmann" |
           Name=="Karim Benzema"       | Name=="Mesut ?zil"          | Name=="Arjen Robben"      )

s2 <- which(Name=="Luke Shaw"   | Name=="Gerard Piqu?"   | Name=="Diego God?n"   | 
            Name=="Ricardo Rodriguez"   |Name=="Eden Hazard"    | Name=="Kevin De Bruyne"    | 
            Name=="Paul Pogba"          | Name=="James Rodr?guez"      | Name=="Sergio Ag?ero" |
            Name=="Lionel Messi"        | Name=="Cristiano Ronaldo"    | Name=="Neymar"         | 
            Name=="Zlatan Ibrahimovic"  )

d_reg <- dist(scale(Football[,-c(1,2,5,6,10,11,23,47)]))

c1 <- pam(as.dist(d_reg),       6)$clustering
c2 <- pam(as.dist(d_fin_AAD),   6)$clustering
c3 <- pam(as.dist(d_fin_Range), 6)$clustering
c4 <- pam(as.dist(d_fin_unit),  6)$clustering
data.frame(Name, c1, c2, c3, c4)[s2,]

football.map <- function(d, s1, s2, k=6, Main="", x_min=1, x_max=1, y_min=1, y_max=1){
  c <- pam(as.dist(d), k)$clustering
  COL1 <- isoMDS(as.dist(d))
  x1   <- COL1$points[,1]
  y1   <- COL1$points[,2]
  
  plot(x1[s1],y1[s1], pch=16, col=c[s1], 
       cex=1.5, cex.main=0.75, cex.lab=0.75,
       main=Main,
       xlim=c(min(x1)*x_min, max(x1)*x_max),
       ylim=c(min(y1)*y_min, max(y1)*y_max),
       xlab="First principle coordinate",
       ylab="Second principle coordinate")
  grid()
  text(x1[s2], y1[s2], Name[s2], cex=0.75, adj=c(1, 1.25))
}

football.map1 <- function(d, s1, s2, k=6, Main="", x_min=1, x_max=1, y_min=1, y_max=1){
  c <- pam(as.dist(d), k)$clustering
  COL1 <- mds(as.dist(d), type="ordinal")
  x1   <- COL1$conf[,1]
  y1   <- COL1$conf[,2]
  
  plot(x1[s1],y1[s1], pch=16, col=c[s1], 
       cex=1.5, cex.main=0.75, cex.lab=0.75,
       main=Main,
       xlim=c(min(x1)*x_min, max(x1)*x_max),
       ylim=c(min(y1)*y_min, max(y1)*y_max),
       xlab="First principle coordinate",
       ylab="Second principle coordinate")
  grid()
  text(x1[s2], y1[s2], Name[s2], cex=0.65, adj=c(1, 1.25))
}


par(mfrow=c(2,2))
football.map(d_reg,       s1, s2, x_min=2, y_min=0.75, Main="Non-metric MDS of plain standardised Euclidean (pam clustering)")
football.map(d_fin_AAD,   s1, s2, Main="Non-metric MDS of distances as explained (pam clustering)")
football.map(d_fin_Range, s1, s2, x_min=1.25, y_min=0.75, y_max=0.75, Main="Non-metric MDS of distances as explained (pam clustering)")
football.map(d_fin_unit,  s1, s2, Main="Non-metric MDS of distances as explained (pam clustering)")
par(mfrow=c(1,1))

























