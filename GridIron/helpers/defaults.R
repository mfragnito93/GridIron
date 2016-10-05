#Defaults
#colors
teamColors <- c('#87B5FF','#010014','#D3D3D3','#120b72', '#3932ff', '#133163')

#metadata
# tableMeta <- c(HASH = "HASH", OFF_FORM = "OFF FORM",
#                PERSONNEL = "PERS", DEF_FORM = "DEF FORM", PLAY_TYPE = "TYPE", 
#                RESULT = "RES", GN_LS = "GN LS", OLINE = "O-LINE", OFF_PLAY = "OFF PLAY",  COVERAGE = "COVGE",
#                BLITZ = "BLTZ", FRONT = "FRONT")


# d_tableMeta <- c(tableMeta, DEF_PLAY="DEF PLAY")
idMap <- list("offense" = "id_o", "defense" = "id_d", "scoreboard" = "id")
odkMap <- list("offense" = "ODK_O", "defense" = "ODK_D", "scoreboard" = "ODK")

scoreboardMeta <- c(id = "PLAY", ODK = "ODK", QTR ="QTR",DRIVE = "DRIVE", O_SCORE = "O SCORE", OPP_SCORE = "OPP SCORE", DN = "DN", DIST = "DIST", 
                    YARD_LN = "YDLN", SIDE = "SIDE", HASH = "HASH", PLAY_TYPE = "TYPE", RESULT = "RESULT", GN_LS = "GN LS")
offenseMeta <- c(id_o = "PLAY", ODK_O = "ODK", OFF_FORM = "OFF FORM", OLINE = "O-LINE", OFF_PLAY = "OFF PLAY", PERSONNEL = "PERS")
defenseMeta <- c(id_d = "PLAY", ODK_D = "ODK", DEF_FORM = "DEF FORM", DEF_PLAY = "DEF PLAY", COVERAGE = "COVGE", BLITZ = "BLITZ", FRONT = "FRONT")


driveSummaryMetaO <- c(id ="PLAY", DN="DN",DIST="DST",PERSONNEL="PERS",  OFF_FORM = "OFF FORM", PLAY_TYPE = "TYPE", OLINE = "O-LINE",
                      OFF_PLAY = "OFF PLAY",DEF_FORM = "DEF FORM",COVERAGE = "COVGE",FRONT = "FRONT",BLITZ = "BLTZ")

driveSummaryMetaD <- c(id ="PLAY", DN="DN",DIST="DST",PERSONNEL="PERS",  OFF_FORM = "OFF FORM", PLAY_TYPE = "TYPE",
                       OFF_PLAY = "PLAY",DEF_FORM = "DEF FORM",DEF_PLAY = "DEF PLAY")

meta <- c(scoreboardMeta,offenseMeta,defenseMeta)

scoreboardDefault <- list(id ="0", ODK = "O", QTR = 1, DRIVE = 1, O_SCORE = 0,
                          OPP_SCORE = 0, DN = 1, DIST = 10,
                          YARD_LN = 20, SIDE = "-", HASH = "M", PLAY_TYPE = "", RESULT = "", GN_LS = 0)
offenseDefault <- list(id_o = "0", ODK_O = "O", OFF_FORM = "", OLINE = "", OFF_PLAY = "", PERSONNEL = "")
defenseDefault <- list(id_d = "0", ODK_D = "O", DEF_FORM = "", DEF_PLAY = "", COVERAGE = "", BLITZ = "", FRONT = "")



default <- c(scoreboardDefault,offenseDefault,defenseDefault)

#preset list path
preSetDDPath <- "data/lists/dropdowns.csv"
preSetDDTemplatePath <- "data/templates/dropdowns.csv"

#preset play data paths
preSetPDPath <- "data/plays/plays.csv"
preSetPDTemplatePath <- "data/templates/plays.csv"

playArchive <- "data/plays/archive/"

#preset scoredboard path
preSetScorePath <- "data/entry/scoreboard.csv"
preSetScoreTemplatePath <- "data/templates/scoreboard.csv"

#preset offense path
preSetOffensePath <- "data/entry/offense.csv"
preSetOffenseTemplatePath <- "data/templates/offense.csv"

#preset defense path
preSetDefensePath <- "data/entry/defense.csv"
preSetDefenseTemplatePath <- "data/templates/defense.csv"

dataPathMap <- list("scoreboard" = preSetScorePath, "offense" = preSetOffensePath, "defense" = preSetDefensePath, "plays" = preSetPDPath)
templatePathMap <- list("scoreboard" = preSetScoreTemplatePath, "offense" = preSetOffenseTemplatePath, "defense" = preSetDefenseTemplatePath, "plays" = preSetPDTemplatePath)

#preset header
preSetHeader <- c("O_OFF_FORM", "O_LINE","O_OFF_PLAY","D_DEF_FORM","O_DEF_FORM","O_DEF_FRONT","D_OFF_FORM","D_OFF_PLAY","D_DEF_PLAY","O_PERSONNEL","D_PERSONNEL","O_DEF_COVERAGE","O_DEF_BLITZ")

preSetHeaderFriendly <- c("Team O Formations","Team O Blocking","Team O Plays","Team D Formations","Opp D Formations","Opp D Fronts","Opp O Formations","Opp O Plays","Team D Plays","Team O Personnel","Opp O Perssonel","Opp D Coverage","Opp D Blitz")

