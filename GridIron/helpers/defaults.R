#Defaults
#colors
teamColors <- c('#87B5FF','#010014','#D3D3D3','#03002B', '#3932ff', '#f7f3be')

#metadata
tableMeta <- c(HASH = "HASH", OFF_FORM = "OFF FORM",
               PERSONNEL = "PERS", DEF_FORM = "DEF FORM", PLAY_TYPE = "TYPE", 
               RESULT = "RES", GN_LS = "GN LS", OLINE = "O-LINE", OFF_PLAY = "OFF PLAY",  COVERAGE = "COVGE",
               BLITZ = "BLTZ", FRONT = "FRONT")

d_tableMeta <- c(tableMeta, DEF_PLAY="DEF PLAY")

scoreboardMeta <- c(id = "PLAY", ODK = "ODK", QTR ="QTR",DRIVE = "DRIVE", O_SCORE = "O SCORE", OPP_SCORE = "OPP SCORE", DN = "DN", DIST = "DIST",
                    YARD_LN = "YDLN", SIDE = "SIDE")

driveSummaryMetaO <- c(id ="PLAY", DN="DN",DIST="DST",PERSONNEL="PERS",  OFF_FORM = "OFF FORM", PLAY_TYPE = "TYPE", OLINE = "O-LINE",
                      OFF_PLAY = "OFF PLAY",DEF_FORM = "DEF FORM",COVERAGE = "COVGE",FRONT = "FRONT",BLITZ = "BLTZ")

driveSummaryMetaD <- c(id ="PLAY", DN="DN",DIST="DST",PERSONNEL="PERS",  OFF_FORM = "OFF FORM", PLAY_TYPE = "TYPE",
                       OFF_PLAY = "PLAY",DEF_FORM = "DEF FORM",DEF_PLAY = "DEF PLAY")

meta <- c(scoreboardMeta,d_tableMeta)

scoreboardDefault <- list(id ="0", ODK = "O", QTR = 1, DRIVE = 1, O_SCORE = 0,
                          OPP_SCORE = 0, DN = 1, DIST = 10,
                          YARD_LN = 20, SIDE = "-")

tableDefault <- list(HASH = "M", OFF_FORM = "", PERSONNEL = "", DEF_FORM = "", PLAY_TYPE = "", RESULT ="",  GN_LS = 0,  OLINE = "", OFF_PLAY = "", COVERAGE = "",
                     BLITZ = "", FRONT = "", DEF_PLAY = "")

default <- c(scoreboardDefault,tableDefault)

#preset list path
preSetDDPath <- "data/lists/dropdowns.csv"
preSetDDTemplatePath <- "data/templates/dropdowns.csv"

#preset play data paths
preSetPDPath <- "data/plays/plays.csv"
preSetPDTemplatePath <- "data/templates/plays.csv"


#preset header
preSetHeader <- c("O_OFF_FORM","O_OFF_PLAY","O_LINE","D_DEF_FORM","O_DEF_FORM","O_DEF_FRONT","D_OFF_FORM","D_OFF_PLAY","D_DEF_PLAY","O_PERSONNEL","D_PERSONNEL","O_DEF_COVERAGE","O_DEF_BLITZ")

preSetHeaderFriendly <- c("Team O Formations","Team O Blocking","Team O Plays","Team D Formations","Opp D Formations","Opp D Fronts","Opp O Formations","Opp O Plays","Team D Plays","Team O Personnel","Opp O Perssonel","Opp D Coverage","Opp D Blitz")

