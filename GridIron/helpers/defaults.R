#Defaults
#colors
teamColors <- c('#87B5FF','#010014','#D3D3D3','#03002B', '#0C00FF', '#C9C9C9')

#metadata
tableMeta <- c(HASH = "HASH", OFF_FORM = "OFF FORM",
               PERSONNEL = "PERS", DEF_FORM = "DEF FORM", PLAY_TYPE = "TYPE", 
               RESULT = "RES", GN_LS = "GN LS", OFF_PLAY = "PLAY", COVERAGE = "COVGE",
               BLITZ = "BLTZ", FRONT = "FRONT")

d_tableMeta <- c(tableMeta, DEF_PLAY="DEF PLAY")

scoreboardMeta <- c(id = "PLAY", ODK = "ODK", QTR ="QTR",DRIVE = "DRIVE", O_SCORE = "O SCORE", OPP_SCORE = "OPP SCORE", DN = "DN", DIST = "DIST",
                    YARD_LN = "YDLN", SIDE = "SIDE")

driveSummaryMeta <- c(id ="PLAY", DN="DN",DIST="DST",PERSONNEL="PERS",  OFF_FORM = "OFF FORM", PLAY_TYPE = "TYPE",
                      OFF_PLAY = "PLAY",DEF_FORM = "DEF FORM",COVERAGE = "COVGE",FRONT = "FRONT",BLITZ = "BLTZ")

meta <- c(scoreboardMeta,d_tableMeta)

scoreboardDefault <- list(id ="0", ODK = "O", QTR = 1, DRIVE = 1, O_SCORE = 0,
                          OPP_SCORE = 0, DN = 1, DIST = 10,
                          YARD_LN = 20, SIDE = "-")

tableDefault <- list(HASH = "M", OFF_FORM = "", PERSONNEL = "", DEF_FORM = "", PLAY_TYPE = "", RESULT ="",  GN_LS = 0,  OFF_PLAY = "", COVERAGE = "",
                     BLITZ = "", FRONT = "", DEF_PLAY = "")

default <- c(scoreboardDefault,tableDefault)

#preset list path
preSetDDPath <- "data/presets/dropdown_lists.csv"
