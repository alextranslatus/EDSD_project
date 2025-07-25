



for(
  pkg in c(
    "haven", # stata whatever
    "ggplot2", # graphs
    "patchwork", # putting plots together
    "ggtext", # bold ggplot annotation
    "viridis", # colors
    "dplyr", # basics
    "tidyr", # basics
    "survey", # survey weights
    "srvyr", # survey weights?
    "gtsummary", # summary tables
    "kableExtra", # styling of tables
    "kable",
    "forcats", # fct_reorder
    "beepr", # helps save time
    "stringr", # in pivoting: separate the suffix to get variables per year
    "data.table", # data.table 
    "stargazer" # regression tables
  )
){
  if(!require(pkg, quietly = TRUE, character.only = TRUE)){
    install.packages(pkg)
  }
}

# Functions ####
source("scripts/0_functions.R")

# Data ####
load(file = "data/analysisdata/elfemini.Rdata")
load(file = "data/analysisdata/mcsmini.Rdata")

# Non factorised (for regressions)

elfe5 <- elfemini %>%
  drop_na(wgt5y) %>%
  drop_na(meduc3)
welfe5 <- svydesign(ids = ~1, data = elfe5, weights = ~ elfe5$wgt5y)

mcs5 <- mcsmini %>%
  filter(inwave5y == 1) %>%
  drop_na(meduc3)
wmcs5 <- svydesign(ids = ~1, data = mcs5, weights = ~ mcs5$wgt5y)

## Tasks sample 

elfewhodoes <- elfe5 %>% 
  drop_na(doctor2m3, nappies2m3, laundry2m3, night2m3, clean2m3, cook2m3, diy2m3)
welfewhodoes <- svydesign(ids = ~1, data = elfewhodoes, weights = ~ elfewhodoes$wgt5y)

mcswhodoes <- mcs5 %>%
  drop_na(doctor9m3, nappies9m3, laundry9m3, night9m3, clean9m3, cook9m3, diy9m3)
wmcswhodoes <- svydesign(ids = ~1, data = mcswhodoes, weights = ~ mcswhodoes$wgt5y)

# Attitudes expectations sample 

elfeexpect <- elfe5 %>% 
  drop_na(socialsuccess2m, lovelife2m, interestingjob2m, passion2m, calmlife2m, bigfamily2m, lotsoffriends2m, fairerworld2m, goodhealth2m, otherwish2m)
welfeexpect <- svydesign(ids = ~1, data = elfeexpect, weights = ~ elfeexpect$wgt5y)

mcsexpect <- mcs5 %>%
  drop_na(independence3y, obedience3y, negotiation3y, respectelders3y, dowellatschool3y, instillreligiousvalues3y,
          bewellliked3y, thinkforself3y, workhard3y, helpothers3y, obeyparents3y, qualityreligiousvalues3y)
wmcsexpect <- svydesign(ids = ~1, data = mcsexpect, weights = ~ mcsexpect$wgt5y)

# Children's access to resources

elferesources <- elfe5 %>% 
  drop_na(frpaint3y, frread3y, music3y, readplus3y, frcounting3y, writing3y, puzzle3y, anyactivity3y,
          swimming3y, gymnastics3y, circus3y, sportsinit3y, musicclass3y, danceclass3y, visualarts3y, horseriding3y)
welferesources <- svydesign(ids = ~1, data = elferesources, weights = ~ elferesources$wgt5y)

mcsresources <- mcs5 %>%
  drop_na(read3y, library3y, counting3y, songs3y, alphabet3y, paint3y, physical3y,
          read5y, stories5y, songs5y, paint5y, physical5y, indoor5y, park5y)
wmcsresources <- svydesign(ids = ~1, data = mcsresources, weights = ~ mcsresources$wgt5y)

em1a <- lm(doctor2m3 ~ sex, elfewhodoes)
em1b <- lm(nappies2m3 ~ sex, elfewhodoes)
em1c <- lm(laundry2m3 ~ sex, elfewhodoes)
em1d <- lm(night2m3 ~ sex, elfewhodoes)
em1e <- lm(clean2m3 ~ sex, elfewhodoes)
em1f <- lm(cook2m3 ~ sex, elfewhodoes)
em1g <- lm(diy2m3 ~ sex, elfewhodoes)

mm1a <- lm(doctor9m3 ~ sex, mcswhodoes)
mm1b <- lm(nappies9m3 ~ sex, mcswhodoes)
mm1c <- lm(laundry9m3 ~ sex, mcswhodoes)
mm1d <- lm(night9m3 ~ sex, mcswhodoes)
mm1e <- lm(clean9m3 ~ sex, mcswhodoes)
mm1f <- lm(cook9m3 ~ sex, mcswhodoes)
mm1g <- lm(diy9m3 ~ sex, mcswhodoes)

em1h <- lm(socialsuccess2m ~ sex, elfeexpect)
em1i <- lm(lovelife2m ~ sex, elfeexpect)
em1j <- lm(interestingjob2m ~ sex, elfeexpect)
em1k <- lm(passion2m ~ sex, elfeexpect)
em1l <- lm(calmlife2m ~ sex, elfeexpect)
em1m <- lm(bigfamily2m ~ sex, elfeexpect)
em1n <- lm(lotsoffriends2m ~ sex, elfeexpect)
em1o <- lm(fairerworld2m ~ sex, elfeexpect)
em1p <- lm(goodhealth2m ~ sex, elfeexpect)

mm1h <- lm(independence3y ~ sex, mcsexpect)
mm1i <- lm(obedience3y ~ sex, mcsexpect)
mm1j <- lm(negotiation3y ~ sex, mcsexpect)
mm1k <- lm(respectelders3y ~ sex, mcsexpect)
mm1l <- lm(dowellatschool3y ~ sex, mcsexpect)
mm1m <- lm(instillreligiousvalues3y ~ sex, mcsexpect)
mm1n <- lm(bewellliked3y ~ sex, mcsexpect)
mm1o <- lm(thinkforself3y ~ sex, mcsexpect)
mm1p <- lm(workhard3y ~ sex, mcsexpect)
mm1q <- lm(helpothers3y ~ sex, mcsexpect)
mm1r <- lm(obeyparents3y ~ sex, mcsexpect)
mm1s <- lm(qualityreligiousvalues3y ~ sex, mcsexpect)

em1q <- lm(frpaint3y ~ sex, elferesources)
em1r <- lm(frread3y ~ sex, elferesources)
em1s <- lm(music3y ~ sex, elferesources)
em1t <- lm(readplus3y ~ sex, elferesources)
em1u <- lm(frcounting3y ~ sex, elferesources)
em1v <- lm(writing3y ~ sex, elferesources)
em1w <- lm(puzzle3y ~ sex, elferesources)
em1x <- lm(anyactivity3y ~ sex, elferesources)
em1y <- lm(swimming3y ~ sex, elferesources)
em1z <- lm(gymnastics3y ~ sex, elferesources)
em1aa <- lm(circus3y ~ sex, elferesources)
em1ab <- lm(sportsinit3y ~ sex, elferesources)
em1ac <- lm(musicclass3y ~ sex, elferesources)
em1ad <- lm(danceclass3y ~ sex, elferesources)
em1ae <- lm(visualarts3y ~ sex, elferesources)
em1af <- lm(horseriding3y ~ sex, elferesources)

mm1t <- lm(read3y ~ sex, mcsresources)
mm1u <- lm(library3y ~ sex, mcsresources)
mm1v <- lm(counting3y ~ sex, mcsresources)
mm1w <- lm(songs3y ~ sex, mcsresources)
mm1x <- lm(alphabet3y ~ sex, mcsresources)
mm1y <- lm(paint3y ~ sex, mcsresources)
mm1z <- lm(physical3y ~ sex, mcsresources)
mm1aa <- lm(read5y ~ sex, mcsresources)
mm1ab <- lm(stories5y ~ sex, mcsresources)
mm1ac <- lm(songs5y ~ sex, mcsresources)
mm1ad <- lm(paint5y ~ sex, mcsresources)
mm1ae <- lm(physical5y ~ sex, mcsresources)
mm1af <- lm(indoor5y ~ sex, mcsresources)
mm1ag <- lm(park5y ~ sex, mcsresources)


em2a <- lm(doctor2m3 ~ sex + meduc3, elfewhodoes)
em2b <- lm(nappies2m3 ~ sex + meduc3, elfewhodoes)
em2c <- lm(laundry2m3 ~ sex + meduc3, elfewhodoes)
em2d <- lm(night2m3 ~ sex + meduc3, elfewhodoes)
em2e <- lm(clean2m3 ~ sex + meduc3, elfewhodoes)
em2f <- lm(cook2m3 ~ sex + meduc3, elfewhodoes)
em2g <- lm(diy2m3 ~ sex + meduc3, elfewhodoes)

mm2a <- lm(doctor9m3 ~ sex + meduc3, mcswhodoes)
mm2b <- lm(nappies9m3 ~ sex + meduc3, mcswhodoes)
mm2c <- lm(laundry9m3 ~ sex + meduc3, mcswhodoes)
mm2d <- lm(night9m3 ~ sex + meduc3, mcswhodoes)
mm2e <- lm(clean9m3 ~ sex + meduc3, mcswhodoes)
mm2f <- lm(cook9m3 ~ sex + meduc3, mcswhodoes)
mm2g <- lm(diy9m3 ~ sex + meduc3, mcswhodoes)

em2h <- lm(socialsuccess2m ~ sex + meduc3, elfeexpect)
em2i <- lm(lovelife2m ~ sex + meduc3, elfeexpect)
em2j <- lm(interestingjob2m ~ sex + meduc3, elfeexpect)
em2k <- lm(passion2m ~ sex + meduc3, elfeexpect)
em2l <- lm(calmlife2m ~ sex + meduc3, elfeexpect)
em2m <- lm(bigfamily2m ~ sex + meduc3, elfeexpect)
em2n <- lm(lotsoffriends2m ~ sex + meduc3, elfeexpect)
em2o <- lm(fairerworld2m ~ sex + meduc3, elfeexpect)
em2p <- lm(goodhealth2m ~ sex + meduc3, elfeexpect)

mm2h <- lm(independence3y ~ sex + meduc3, mcsexpect)
mm2i <- lm(obedience3y ~ sex + meduc3, mcsexpect)
mm2j <- lm(negotiation3y ~ sex + meduc3, mcsexpect)
mm2k <- lm(respectelders3y ~ sex + meduc3, mcsexpect)
mm2l <- lm(dowellatschool3y ~ sex + meduc3, mcsexpect)
mm2m <- lm(instillreligiousvalues3y ~ sex + meduc3, mcsexpect)
mm2n <- lm(bewellliked3y ~ sex + meduc3, mcsexpect)
mm2o <- lm(thinkforself3y ~ sex + meduc3, mcsexpect)
mm2p <- lm(workhard3y ~ sex + meduc3, mcsexpect)
mm2q <- lm(helpothers3y ~ sex + meduc3, mcsexpect)
mm2r <- lm(obeyparents3y ~ sex + meduc3, mcsexpect)
mm2s <- lm(qualityreligiousvalues3y ~ sex + meduc3, mcsexpect)

em2q <- lm(frpaint3y ~ sex + meduc3, elferesources)
em2r <- lm(frread3y ~ sex + meduc3, elferesources)
em2s <- lm(music3y ~ sex + meduc3, elferesources)
em2t <- lm(readplus3y ~ sex + meduc3, elferesources)
em2u <- lm(frcounting3y ~ sex + meduc3, elferesources)
em2v <- lm(writing3y ~ sex + meduc3, elferesources)
em2w <- lm(puzzle3y ~ sex + meduc3, elferesources)
em2x <- lm(anyactivity3y ~ sex + meduc3, elferesources)
em2y <- lm(swimming3y ~ sex + meduc3, elferesources)
em2z <- lm(gymnastics3y ~ sex + meduc3, elferesources)
em2aa <- lm(circus3y ~ sex + meduc3, elferesources)
em2ab <- lm(sportsinit3y ~ sex + meduc3, elferesources)
em2ac <- lm(musicclass3y ~ sex + meduc3, elferesources)
em2ad <- lm(danceclass3y ~ sex + meduc3, elferesources)
em2ae <- lm(visualarts3y ~ sex + meduc3, elferesources)
em2af <- lm(horseriding3y ~ sex + meduc3, elferesources)

mm2t <- lm(read3y ~ sex + meduc3, mcsresources)
mm2u <- lm(library3y ~ sex + meduc3, mcsresources)
mm2v <- lm(counting3y ~ sex + meduc3, mcsresources)
mm2w <- lm(songs3y ~ sex + meduc3, mcsresources)
mm2x <- lm(alphabet3y ~ sex + meduc3, mcsresources)
mm2y <- lm(paint3y ~ sex + meduc3, mcsresources)
mm2z <- lm(physical3y ~ sex + meduc3, mcsresources)
mm2aa <- lm(read5y ~ sex + meduc3, mcsresources)
mm2ab <- lm(stories5y ~ sex + meduc3, mcsresources)
mm2ac <- lm(songs5y ~ sex + meduc3, mcsresources)
mm2ad <- lm(paint5y ~ sex + meduc3, mcsresources)
mm2ae <- lm(physical5y ~ sex + meduc3, mcsresources)
mm2af <- lm(indoor5y ~ sex + meduc3, mcsresources)
mm2ag <- lm(park5y ~ sex + meduc3, mcsresources)


em3a <- lm(doctor2m3 ~ sex * meduc3, elfewhodoes)
em3b <- lm(nappies2m3 ~ sex * meduc3, elfewhodoes)
em3c <- lm(laundry2m3 ~ sex * meduc3, elfewhodoes)
em3d <- lm(night2m3 ~ sex * meduc3, elfewhodoes)
em3e <- lm(clean2m3 ~ sex * meduc3, elfewhodoes)
em3f <- lm(cook2m3 ~ sex * meduc3, elfewhodoes)
em3g <- lm(diy2m3 ~ sex * meduc3, elfewhodoes)

mm3a <- lm(doctor9m3 ~ sex * meduc3, mcswhodoes)
mm3b <- lm(nappies9m3 ~ sex * meduc3, mcswhodoes)
mm3c <- lm(laundry9m3 ~ sex * meduc3, mcswhodoes)
mm3d <- lm(night9m3 ~ sex * meduc3, mcswhodoes)
mm3e <- lm(clean9m3 ~ sex * meduc3, mcswhodoes)
mm3f <- lm(cook9m3 ~ sex * meduc3, mcswhodoes)
mm3g <- lm(diy9m3 ~ sex * meduc3, mcswhodoes)

em3h <- lm(socialsuccess2m ~ sex * meduc3, elfeexpect)
em3i <- lm(lovelife2m ~ sex * meduc3, elfeexpect)
em3j <- lm(interestingjob2m ~ sex * meduc3, elfeexpect)
em3k <- lm(passion2m ~ sex * meduc3, elfeexpect)
em3l <- lm(calmlife2m ~ sex * meduc3, elfeexpect)
em3m <- lm(bigfamily2m ~ sex * meduc3, elfeexpect)
em3n <- lm(lotsoffriends2m ~ sex * meduc3, elfeexpect)
em3o <- lm(fairerworld2m ~ sex * meduc3, elfeexpect)
em3p <- lm(goodhealth2m ~ sex * meduc3, elfeexpect)

mm3h <- lm(independence3y ~ sex * meduc3, mcsexpect)
mm3i <- lm(obedience3y ~ sex * meduc3, mcsexpect)
mm3j <- lm(negotiation3y ~ sex * meduc3, mcsexpect)
mm3k <- lm(respectelders3y ~ sex * meduc3, mcsexpect)
mm3l <- lm(dowellatschool3y ~ sex * meduc3, mcsexpect)
mm3m <- lm(instillreligiousvalues3y ~ sex * meduc3, mcsexpect)
mm3n <- lm(bewellliked3y ~ sex * meduc3, mcsexpect)
mm3o <- lm(thinkforself3y ~ sex * meduc3, mcsexpect)
mm3p <- lm(workhard3y ~ sex * meduc3, mcsexpect)
mm3q <- lm(helpothers3y ~ sex * meduc3, mcsexpect)
mm3r <- lm(obeyparents3y ~ sex * meduc3, mcsexpect)
mm3s <- lm(qualityreligiousvalues3y ~ sex * meduc3, mcsexpect)

em3q <- lm(frpaint3y ~ sex * meduc3, elferesources)
em3r <- lm(frread3y ~ sex * meduc3, elferesources)
em3s <- lm(music3y ~ sex * meduc3, elferesources)
em3t <- lm(readplus3y ~ sex * meduc3, elferesources)
em3u <- lm(frcounting3y ~ sex * meduc3, elferesources)
em3v <- lm(writing3y ~ sex * meduc3, elferesources)
em3w <- lm(puzzle3y ~ sex * meduc3, elferesources)
em3x <- lm(anyactivity3y ~ sex * meduc3, elferesources)
em3y <- lm(swimming3y ~ sex * meduc3, elferesources)
em3z <- lm(gymnastics3y ~ sex * meduc3, elferesources)
em3aa <- lm(circus3y ~ sex * meduc3, elferesources)
em3ab <- lm(sportsinit3y ~ sex * meduc3, elferesources)
em3ac <- lm(musicclass3y ~ sex * meduc3, elferesources)
em3ad <- lm(danceclass3y ~ sex * meduc3, elferesources)
em3ae <- lm(visualarts3y ~ sex * meduc3, elferesources)
em3af <- lm(horseriding3y ~ sex * meduc3, elferesources)

mm3t <- lm(read3y ~ sex * meduc3, mcsresources)
mm3u <- lm(library3y ~ sex * meduc3, mcsresources)
mm3v <- lm(counting3y ~ sex * meduc3, mcsresources)
mm3w <- lm(songs3y ~ sex * meduc3, mcsresources)
mm3x <- lm(alphabet3y ~ sex * meduc3, mcsresources)
mm3y <- lm(paint3y ~ sex * meduc3, mcsresources)
mm3z <- lm(physical3y ~ sex * meduc3, mcsresources)
mm3aa <- lm(read5y ~ sex * meduc3, mcsresources)
mm3ab <- lm(stories5y ~ sex * meduc3, mcsresources)
mm3ac <- lm(songs5y ~ sex * meduc3, mcsresources)
mm3ad <- lm(paint5y ~ sex * meduc3, mcsresources)
mm3ae <- lm(physical5y ~ sex * meduc3, mcsresources)
mm3af <- lm(indoor5y ~ sex * meduc3, mcsresources)
mm3ag <- lm(park5y ~ sex * meduc3, mcsresources)

em4a <- lm(doctor2m3 ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfewhodoes)
em4b <- lm(nappies2m3 ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfewhodoes)
em4c <- lm(laundry2m3 ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfewhodoes)
em4d <- lm(night2m3 ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfewhodoes)
em4e <- lm(clean2m3 ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfewhodoes)
em4f <- lm(cook2m3 ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfewhodoes)
em4g <- lm(diy2m3 ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfewhodoes)

mm4a <- lm(doctor9m3 ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcswhodoes)
mm4b <- lm(nappies9m3 ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcswhodoes)
mm4c <- lm(laundry9m3 ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcswhodoes)
mm4d <- lm(night9m3 ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcswhodoes)
mm4e <- lm(clean9m3 ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcswhodoes)
mm4f <- lm(cook9m3 ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcswhodoes)
mm4g <- lm(diy9m3 ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcswhodoes)

em4h <- lm(socialsuccess2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)
em4i <- lm(lovelife2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)
em4j <- lm(interestingjob2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)
em4k <- lm(passion2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)
em4l <- lm(calmlife2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)
em4m <- lm(bigfamily2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)
em4n <- lm(lotsoffriends2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)
em4o <- lm(fairerworld2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)
em4p <- lm(goodhealth2m ~ sex * meduc3 + emp2m + leave2m + twopar2m, elfeexpect)

mm4h <- lm(independence3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4i <- lm(obedience3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4j <- lm(negotiation3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4k <- lm(respectelders3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4l <- lm(dowellatschool3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4m <- lm(instillreligiousvalues3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4n <- lm(bewellliked3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4o <- lm(thinkforself3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4p <- lm(workhard3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4q <- lm(helpothers3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4r <- lm(obeyparents3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)
mm4s <- lm(qualityreligiousvalues3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsexpect)

em4q <- lm(frpaint3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4r <- lm(frread3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4s <- lm(music3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4t <- lm(readplus3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4u <- lm(frcounting3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4v <- lm(writing3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4w <- lm(puzzle3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4x <- lm(anyactivity3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4y <- lm(swimming3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4z <- lm(gymnastics3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4aa <- lm(circus3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4ab <- lm(sportsinit3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4ac <- lm(musicclass3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4ad <- lm(danceclass3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4ae <- lm(visualarts3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)
em4af <- lm(horseriding3y ~ sex * meduc3 + emp2m + leave2m + twopar2m, elferesources)

mm4t <- lm(read3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4u <- lm(library3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4v <- lm(counting3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4w <- lm(songs3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4x <- lm(alphabet3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4y <- lm(paint3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4z <- lm(physical3y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4aa <- lm(read5y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4ab <- lm(stories5y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4ac <- lm(songs5y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4ad <- lm(paint5y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4ae <- lm(physical5y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4af <- lm(indoor5y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)
mm4ag <- lm(park5y ~ sex * meduc3 + emp9m + leave9m + twopar9m, mcsresources)

# Replace with your actual model list
modellingfr_models_list <- list(
  "doctor2m3" = list(em1a, em2a, em3a, em4a),
  "nappies2m3" = list(em1b, em2b, em3b, em4b),
  "laundry2m3" = list(em1c, em2c, em3c, em4c),
  "night2m3" = list(em1d, em2d, em3d, em4d),
  "clean2m3" = list(em1e, em2e, em3e, em4e),
  "cook2m3" = list(em1f, em2f, em3f, em4f),
  "diy2m3" = list(em1g, em2g, em3g, em4g)
)

modellinguk_models_list <- list(
  "doctor9m3" = list(mm1a, mm2a, mm3a, mm4a),
  "nappies9m3" = list(mm1b, mm2b, mm3b, mm4b),
  "laundry9m3" = list(mm1c, mm2c, mm3c, mm4c),
  "night9m3" = list(mm1d, mm2d, mm3d, mm4d),
  "clean9m3" = list(mm1e, mm2e, mm3e, mm4e),
  "cook9m3" = list(mm1f, mm2f, mm3f, mm4f),
  "diy9m3" = list(mm1g, mm2g, mm3g, mm4g)
)

attitudefr_models_list <- list(
  "socialsuccess2m" = list(em1h, em2h, em3h, em4h),
  "lovelife2m" = list(em1i, em2i, em3i, em4i),
  "interestingjob2m" = list(em1j, em2j, em3j, em4j),
  "passion2m" = list(em1k, em2k, em3k, em4k),
  "calmlife2m" = list(em1l, em2l, em3l, em4l),
  "bigfamily2m" = list(em1m, em2m, em3m, em4m),
  "lotsoffriends2m" = list(em1n, em2n, em3n, em4n),
  "fairerworld2m" = list(em1o, em2o, em3o, em4o),
  "goodhealth2m" = list(em1p, em2p, em3p, em4p)
)

attitudeuk_models_list <- list(
  "independence3y" = list(mm1h, mm2h, mm3h, mm4h),
  "obedience3y" = list(mm1i, mm2i, mm3i, mm4i),
  "negotiation3y" = list(mm1j, mm2j, mm3j, mm4j),
  "respectelders3y" = list(mm1k, mm2k, mm3k, mm4k),
  "dowellatschool3y" = list(mm1l, mm2l, mm3l, mm4l),
  "instillreligiousvalues3y" = list(mm1m, mm2m, mm3m, mm4m),
  "bewellliked3y" = list(mm1n, mm2n, mm3n, mm4n),
  "thinkforself3y" = list(mm1o, mm2o, mm3o, mm4o),
  "workhard3y" = list(mm1p, mm2p, mm3p, mm4p),
  "helpothers3y" = list(mm1q, mm2q, mm3q, mm4q),
  "obeyparents3y" = list(mm1r, mm2r, mm3r, mm4r),
  "qualityreligiousvalues3y" = list(mm1s, mm2s, mm3s, mm4s)
)

resourcesfr_models_list <- list(
  "frpaint3y" = list(em1q, em2q, em3q, em4q),
  "frread3y" = list(em1r, em2r, em3r, em4r),
  "music3y" = list(em1s, em2s, em3s, em4s),
  "readplus3y" = list(em1t, em2t, em3t, em4t),
  "frcounting3y" = list(em1u, em2u, em3u, em4u),
  "writing3y" = list(em1v, em2v, em3v, em4v),
  "puzzle3y" = list(em1w, em2w, em3w, em4w),
  "anyactivity3y" = list(em1x, em2x, em3x, em4x),
  "swimming3y" = list(em1y, em2y, em3y, em4y),
  "gymnastics3y" = list(em1z, em2z, em3z, em4z),
  "circus3y" = list(em1aa, em2aa, em3aa, em4aa),
  "sportsinit3y" = list(em1ab, em2ab, em3ab, em4ab),
  "musicclass3y" = list(em1ac, em2ac, em3ac, em4ac),
  "danceclass3y" = list(em1ad, em2ad, em3ad, em4ad),
  "visualarts3y" = list(em1ae, em2ae, em3ae, em4ae),
  "horseriding3y" = list(em1af, em2af, em3af, em4af)
)

resourcesuk_models_list <- list(
  "read3y" = list(mm1t, mm2t, mm3t, mm4t),
  "library3y" = list(mm1u, mm2u, mm3u, mm4u),
  "counting3y" = list(mm1v, mm2v, mm3v, mm4v),
  "songs3y" = list(mm1w, mm2w, mm3w, mm4w),
  "alphabet3y" = list(mm1x, mm2x, mm3x, mm4x),
  "paint3y" = list(mm1y, mm2y, mm3y, mm4y),
  "physical3y" = list(mm1z, mm2z, mm3z, mm4z),
  "read5y" = list(mm1aa, mm2aa, mm3aa, mm4aa),
  "stories5y" = list(mm1ab, mm2ab, mm3ab, mm4ab),
  "songs5y" = list(mm1ac, mm2ac, mm3ac, mm4ac),
  "paint5y" = list(mm1ad, mm2ad, mm3ad, mm4ad),
  "physical5y" = list(mm1ae, mm2ae, mm3ae, mm4ae),
  "indoor5y" = list(mm1af, mm2af, mm3af, mm4af),
  "park5y" = list(mm1ag, mm2ag, mm3ag, mm4ag)
)

dir.create("./app/data", recursive = TRUE, showWarnings = FALSE)

save(
  attitudefr_models_list,
  attitudeuk_models_list,
  modellingfr_models_list,
  modellinguk_models_list,
  resourcesfr_models_list,
  resourcesuk_models_list,
  file = "./app/data/models_lists.RData"
)
