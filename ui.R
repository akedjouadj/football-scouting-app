library(shiny)
library(DT)
library(shinythemes) 
library(bslib)

classe_var= c('Ligue et equipe', 'Position', 'VNS', 'VNI', 'Autres variables', 'Matrice de distance')
id= function(x)x

# initiaux des joueurs populaires
name_popular= c("D.A","S.R","G.P","A.I","P.P","J.R","L.M","C.R","Ney.")

# chargement des données------------------------------------------------------------
# data_popular= read.csv("popular-player-vns.csv")
# Football_upperlevel_var= read.csv("Football_upperlevel_var.csv")
# Football_lowerlevel_var=read.csv("Football_lowerlevel_var.csv")
# Football= read.csv("Football.csv")
# # 
# # matrices de distance--------------------------------------
# ## Other variables ####
# Other_dist= read.csv("Other_dist.csv")
# 
# # top level distance------------------
# INTERCEPTION_dist= read.csv("INTERCEPTION_dist.csv")
# FOULED_dist= read.csv("FOULED_dist.csv")
# FOUL_dist= read.csv("FOUL_dist.csv")
# OFFSIDE_dist= read.csv("OFFSIDE_dist.csv")
# CLEARANCE_dist= read.csv("CLEARANCE_dist.csv")
# UNSTCH_dist= read.csv("UNSTCH_dist.csv")
# DISPOSSESSED_dist= read.csv("DISPOSSESSED_dist.csv")
# TACKLE_dist= read.csv("TACKLE_dist.csv")
# AERIAL_dist= read.csv("AERIAL_dist.csv")
# DRIBBLE_dist= read.csv("DRIBBLE_dist.csv")
# BLOCK_dist= read.csv("BLOCK_dist.csv")
# SHOT_dist= read.csv("SHOT_dist.csv")
# GOAL_dist= read.csv("GOAL_dist.csv")
# PASS_dist= read.csv("PASS_dist.csv")
# KEYPASS_dist= read.csv("KEYPASS_dist.csv")
# ASSIST_dist= read.csv("ASSIST_dist.csv")
# 
# #### Lower level ####
# TACKLE_suc_dist= read.csv("TACKLE_suc_dist.csv")
# AERIAL_suc_dist= read.csv("AERIAL_suc_dist.csv")
# DRIBBLE_suc_dist= read.csv("DRIBBLE_suc_dist.csv")
# BLOCK_pro_dist= read.csv("BLOCK_pro_dist.csv")
# SHOT_ZONE_pro_dist= read.csv("SHOT_ZONE_pro_dist.csv")
# GOAL_ZONE_pro_dist= read.csv("GOAL_ZONE_pro_dist.csv")
# GOAL_SYB_suc_dist= read.csv("GOAL_SYB_suc_dist.csv")
# GOAL_PA_suc_dist= read.csv("GOAL_PA_suc_dist.csv")
# GOAL_OOB_suc_dist= read.csv("GOAL_OOB_suc_dist.csv")
# SHOT_SITUATION_pro_dist= read.csv("SHOT_SITUATION_pro_dist.csv")
# GOAL_SITUATION_pro_dist= read.csv("GOAL_SITUATION_pro_dist.csv")
# GOAL_OP_suc_dist= read.csv("GOAL_OP_suc_dist.csv")
# GOAL_C_suc_dist= read.csv("GOAL_C_suc_dist.csv")
# GOAL_SP_suc_dist= read.csv("GOAL_SP_suc_dist.csv")
# GOAL_PT_suc_dist= read.csv("GOAL_PT_suc_dist.csv")
# SHOT_BODYPART_pro_dist= read.csv("SHOT_BODYPART_pro_dist.csv")
# GOAL_BODYPART_pro_dist= read.csv("GOAL_BODYPART_pro_dist.csv")
# GOAL_RF_suc_dist= read.csv("GOAL_RF_suc_dist.csv")
# GOAL_LF_suc_dist= read.csv("GOAL_LF_suc_dist.csv")
# GOAL_Head_suc_dist= read.csv("GOAL_Head_suc_dist.csv")
# GOAL_Other_suc_dist= read.csv("GOAL_Other_suc_dist.csv")
# SHOT_ACCURACY_pro_dist= read.csv("SHOT_ACCURACY_pro_dist.csv")
# GOAL_suc_dist= read.csv("GOAL_suc_dist.csv")
# GOAL_suc1_dist= read.csv("GOAL_suc1_dist.csv")
# PASS_suc_dist= read.csv("PASS_suc_dist.csv")
# PASS_LENGTH_pro_dist= read.csv("PASS_LENGTH_pro_dist.csv")
# PASS_Long_suc_dist= read.csv("PASS_Long_suc_dist.csv")
# PASS_Short_suc_dist= read.csv("PASS_Short_suc_dist.csv")
# PASS_TYPE_pro_dist= read.csv("PASS_TYPE_pro_dist.csv")
# PASS_Cross_suc_dist= read.csv("PASS_Cross_suc_dist.csv")
# PASS_Corner_suc_dist= read.csv("PASS_Corner_suc_dist.csv")
# PASS_Freekick_suc_dist= read.csv("PASS_Freekick_suc_dist.csv")
# KEYPASS_LENGTH_pro_dist= read.csv("KEYPASS_LENGTH_pro_dist.csv")
# KEYPASS_TYPE_pro_dist= read.csv("KEYPASS_TYPE_pro_dist.csv")
# ASSIST_pro_dist= read.csv("ASSIST_pro_dist.csv")
# ASSIST_suc_dist= read.csv("ASSIST_suc_dist.csv")
# ASSIST_Cross_suc_dist= read.csv("ASSIST_Cross_suc_dist.csv")
# ASSIST_Corner_suc_dist= read.csv("ASSIST_Corner_suc_dist.csv")
# ASSIST_Freekick_suc_dist= read.csv("ASSIST_Freekick_suc_dist.csv")
# ASSIST_Thrb_suc_dist= read.csv("ASSIST_Thrb_suc_dist.csv")
# ASSIST_ThrI_suc_dist= read.csv("ASSIST_ThrI_suc_dist.csv")
# 
# # on upload les matrices de distance de ligue et équipe et de position
# dist_pos1= read.csv("dist_pos1.csv")
# dist_pos2= read.csv("dist_pos2.csv")
# Team_country_dist= read.csv("Team_country_dist.csv")
#---------------------fin chargement des données ---------------------------------------------


shinyUI(fluidPage(
  theme = bs_theme(bg = "white", fg = "blue", primary = "red"),
  h3("Détection de profil de footballeurs similaires", style="color: blue"),
  h5("auteur : Achraff ADJILEYE"),
  tags$a("Cliquer ici pour mon profil linkedin", href= "https://www.linkedin.com/in/achraff-adjileye-3740a5211/"),
  navbarPage("Football saison 2014-2015",
             tabPanel("Calcul de la matrice de distance",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("stand_method", "Choix de standardisation", choices=c("écart absolu médian", "variance empirique", "Min-max")),
                          h4("Poids des actions"),
                          fluidRow(column(width = 6, sliderInput("offside_w", "Hors-jeu", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("interception_w", "Interceptions", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("foul_w", "Fautes commises", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("fouled_w", "Fautes subies", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("unstch_w", "Contôles ratés", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("disp_w", "Ballons perdus", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("clearance_w", "Dégagements", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("tackle_w", "Tacle", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("block_w", "Contres", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("aerial_w", "Duels aériens", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("dribble_w", "Dribbles", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("assist_w", "Passes décisives", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("shot_w", "Tirs", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("goal_w", "Buts", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("pass_w", "Passes", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("keypass_w", "Passes clées", value=1, min=0, max=10)),
                                   column(width = 6, sliderInput("position_w", "Positions", value=13, min=0, max=20)),
                                   column(width = 6, sliderInput("team_league_w", "Ligues et équipes", value=6, min=0, max=20))
                          ),
                          actionButton('run_1', 'Entrer')
                        ),
                        mainPanel(
                          h5("Calcul de la matrice de dissimilarité", align="center"),
                          textOutput("matrix_explain"),
                          hr(),
                          downloadButton('save_matrix_dist', 'Enrégistrer au format csv')
                        )
                      )
             ),
             tabPanel("Requête Dissimilarité",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("matrix_dist_file", "Importer la matrice de dissimilarité en .csv", accept = ".csv"),
                          selectInput("name_player", "Choisissez le joueur qui vous intéresse", choices=Football$Name, selected = "Lionel Messi"),
                          selectInput("nb_player", "Combien de joueurs voulez-vous ? ", choices = c(4:30), selected = 6),
                          h4("Appliquer un filtre"),
                          sliderInput("age_filter", "Maximum d'âge", value =39, min= 16, max=45, step=1),
                          sliderInput("height_filter", "Maximum de taille", value =203, min= 150, max=210, step=1),
                          sliderInput("weight_filter", "Maximum de poids", value =98, min= 50, max=100, step=1),
                          sliderInput("apps_filter", "Minimum apps", value =10, min= 1, max=50, step=1),
                          sliderInput("mins_filter", "Minimum mins", value =900, min= 200, max=3800, step=1),
                          actionButton('run_2', 'Valider vos sélections')
                        ),
                        mainPanel(
                          h5("Liste des joueurs ", align="center"),
                          textOutput('entete'),
                          hr(),
                          verbatimTextOutput('top_nb')
                        )
                      )
             ),
             tabPanel("Tableaux de données",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("var", "Choisis une classe de variable : ", choices = classe_var),
                          conditionalPanel(condition="input.var=='Matrice de distance' ",
                                           sliderInput("dim", "Dimension d'affichage", min=10, max=3003, value=10, step=1),
                                           actionButton('run_3', 'Entrer la dimension')
                          )
                        ),
                        mainPanel(
                          tabPanel( title= 'Données',
                                    conditionalPanel(
                                      condition = "input.var == 'Ligue et equipe' ", 
                                      DTOutput('lg_et_eq'),
                                      downloadButton('save_lg_et_eq', 'Enrégistrer au format csv')),
                                    
                                    conditionalPanel(
                                      condition = "input.var == 'Position' ", 
                                      DTOutput('pos'),
                                      downloadButton('save_pos', 'Enrégistrer au format csv')),
                                    
                                    conditionalPanel(
                                      condition = "input.var == 'VNS' ", 
                                      DTOutput('vns'),
                                      downloadButton('save_vns', 'Enrégistrer au format csv')),
                                    
                                    conditionalPanel(
                                      condition = "input.var == 'VNI' ", 
                                      DTOutput('vni'),
                                      downloadButton('save_vni', 'Enrégistrer au format csv')),
                                    
                                    conditionalPanel(
                                      condition = "input.var == 'Autres variables' ", 
                                      DTOutput('autre'),
                                      downloadButton('save_autre', 'Enrégistrer au format csv')),
                                    
                                    conditionalPanel(
                                      condition = "input.var == 'Matrice de distance' ", 
                                      DTOutput('matrix_dist'))
                          )
                        )
                      )),
             tabPanel("Transformations des VNS",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("var_dist_vns", "Choisis une variable", choices = colnames(Football_upperlevel_var)[-c(13,16)]),
                          sliderInput("var_c", "Valeur de la constante c", value =1, min= 0.01, max=100, step=0.01)
                        ),
                        mainPanel(
                          fluidRow(column(width = 12, "Ecarts entre joueurs populaires", plotOutput("var_dist_popular", height = "400px")),
                                   column(width = 4, plotOutput("var_summary_id", height = "200px")),
                                   column(width = 4, plotOutput("var_summary_sqrt", height = "200px")),
                                   column(width = 4, plotOutput("var_summary_log", height = "200px"))
                          )
                        )
                      )
             )
  )
))
