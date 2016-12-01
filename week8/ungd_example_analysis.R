rm(list = ls(all = TRUE))

library(foreign)
library(rworldmap)
library(RColorBrewer)
library(classInt)
library(ggplot2)
library(dplyr)
library(readr)
library(quanteda)
library(vegan)
library(ca)
library(topicmodels)
library(countrycode)

library(plm)
library(lmtest)
library(texreg)

library(haven)


library(pls)
library(boot)


setwd("~/Dropbox/Slavka/Research/UN General Debate/Analysis")

##### Preprocessing   ######

texts <- textfile("./input/*.txt",
                  docvarsfrom="filenames", 
                  dvsep="_",   
                  docvarnames=c("Country", "Session", "Year"))

debates.corpus <- corpus(texts, notes="UNGA General Debate 1970-2014")

dtm <- dfm(debates.corpus, 
           ignoredFeatures=stopwords("english"), 
           stem=TRUE,
           language="english")


#Creating reduced dtm
reduced.dtm <- trim(dtm,minCount=10,minDoc=5)

#We can also use weights here.
#We are not going to use weighted dtm below, but you can easily take it up
weighted.dtm <- weight(dtm, type = "tfidf")

#Creating a data frame from reduced DTM and adding Country (ISO) and Year variables
df.dtm <- as.data.frame(reduced.dtm)
df.dtm$Year <- as.numeric(docvars(debates.corpus,field="Year"))
df.dtm$Country <- docvars(debates.corpus,field="Country")



#Loading idealpoints data from Voeten et al.
idealpoints <- read.dta("./text analysis/Idealpoints.dta")

#converting COW country codes to ISO
idealpoints$Country <- countrycode(idealpoints$ccode, "cown", "iso3c", warn = TRUE)

#Correcting some codes that don't match
idealpoints$Country[idealpoints$CountryAbb=="GFR"] <- "DEU"
idealpoints$Country[idealpoints$CountryAbb=="YPR"] <- "YDYE"
idealpoints$Country[idealpoints$CountryAbb=="YAR"] <- "YEM"


#Merging Idealpoints data and DTM data

df.combined <- inner_join(df.dtm,idealpoints, by=c("Country", "Year"))


##### Correspondence Analysis   ######

#Simple CA
system.time(cca.ideal.notime <- cca(df.combined[,1:11902] ~ df.combined$Idealpoint + Condition(df.combined$Year)))

system.time(cca.notime <- cca(df.combined[,1:11902] ~ 1 + Condition(df.combined$Year)))



#Constrained effect of Idealpoint with time trend removed
cca.ideal.notime


#cca.ideal.notime is the focus here: it's speeches that are explained by votes and time removed
# a residual from that (cca.resid.idealnotime) is what's left orthogonal from cca.ideal.notime

#for heatmaps

cca.notime



######## Predicting scores from CA ###########

scores <- as.data.frame(scores(cca.ideal.notime,choices=c(1:11), display="wa", scaling=1))

scores.standard <- as.data.frame(scores(cca.standard,choices=c(1:2), display="wa", scaling=1))

scores.results <- data.frame(cbind(df.combined$Country, df.combined$Year, scores, df.combined$Idealpoint, scores.standard))

names(scores.results)[1] <- "Country"
names(scores.results)[2] <- "Year"
names(scores.results)[14] <- "Idealpoint"
names(scores.results)[15] <- "CA1standard"
names(scores.results)[16] <- "CA2standard"

write.csv(scores.results,file="scores.csv")



########### Plot CCA1 vs ideal  ########

ggplot(scores.results, aes(CCA1, Idealpoint)) + 
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color="red", size=.5) +
  geom_point(alpha = .2) + 
  theme_bw() + 
  labs(title = "Positional information from speeches and roll-call data", x = "Constrained correspondence analysis", y = "Ideal point estimate") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  theme(plot.title = element_text(size=18)) +
  annotate("text", x = 0.5, y = 2, label = "r==-0.8", parse = TRUE)

ggsave("correlation.pdf")



############### Explaining position on first constrained dimension (CCA1)  ######################

#Econ data
econ_data <- read_dta("~/Dropbox/Slavka/Research/AfricaChina/data/Master DS_May2016.dta")

names(econ_data)[1] <- "Year"
names(econ_data)[3] <- "Country"

#Merging econ data with CA scores data
est_data <- inner_join(econ_data,scores.results, by=c("Country", "Year"))

#Twoways fixed effects estimation
cca1_expl <- plm(CCA1 ~ polity2 + 
                    Left + 
                    log_GDPpc + 
                    WB_tradeopeness  + 
                    conflict +
                    State_Dept, 
                  data = est_data, 
                  index = c("Country", "Year"), 
                  model = "within", 
                  effect = "twoways")

summary(cca1_expl)

#Correcting SEs for serial correlation
cca1_expl_cluster <- coeftest(cca1_expl, 
         vcov = vcovHC(cca1_expl, 
                       method = "arellano", 
                       type = "HC3"))




ca1_expl <- plm(CA1 ~ polity2 + 
                     Left + 
                     log_GDPpc + 
                     WB_tradeopeness  + 
                     conflict + 
                     State_Dept, 
                  data = est_data, 
                  index = c("Country", "Year"), 
                  model = "within", 
                  effect = "twoways")

summary(ca1_expl)

ca1_expl_cluster <- coeftest(ca1_expl, 
         vcov = vcovHC(ca1_expl, 
                       method = "arellano", 
                       type = "HC3"))



ideal_expl <- plm(Idealpoint.y ~ polity2 + 
                     Left + 
                     log_GDPpc + 
                     WB_tradeopeness  + 
                     conflict + 
                     State_Dept, 
                   data = est_data, 
                   index = c("Country", "Year"), 
                   model = "within", 
                   effect = "twoways")

summary(ideal_expl)

ideal_expl_cluster <- coeftest(ideal_expl, 
         vcov = vcovHC(ideal_expl, 
                       method = "arellano", 
                       type = "HC3"))

#Table of results for LaTeX
texreg(list(cca1_expl_cluster, ca1_expl_cluster, ideal_expl_cluster), digits = 3)


#######   TS graphs CCA1/CA1  ########


#Dimension 1: Position
ggplot(data=subset(scores.results,Country=="USA" | Country=="RUS"), 
            aes(x=Year, y=CCA1, group=Country, colour=Country)) +
  theme_bw() +
  ggtitle("USA and Russia: Position") + ylab("Constrained Dimension 1")+
  theme(plot.title = element_text(lineheight=.8, face="bold", size=15),
        axis.title.x = element_text(face="bold", size=11),
        axis.title.y = element_text(face="bold", size=11),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=8)) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(1970,2014,2)) +
  geom_smooth() + 
  geom_point(aes(shape=Country))
  

ggsave("posDim1.pdf")


#Dimension 1: Salience
ggplot(data=subset(scores.results,Country=="USA" | Country=="RUS"), 
            aes(x=Year, y=CA1, group=Country, colour=Country)) +theme_bw() +
  ggtitle("USA and Russia: Salience & Position") + ylab("Unconstrained Dimension 1")+
  theme(plot.title = element_text(lineheight=.8, face="bold", size=15),
        axis.title.x = element_text(face="bold", size=11),
        axis.title.y = element_text(face="bold", size=11),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=8)) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(1970,2014,2)) +
  geom_smooth() +
  geom_point(aes(shape=Country)) 

ggsave("salDim1.pdf")


#######   Ukraine Case: wordscores for 2013  ###########

#Wordscores 2013

dtm.2013 <- dfm(subset(debates.corpus, Year==2013), 
                ignoredFeatures=stopwords("english"), 
                stem=TRUE,
                language="english")

#Creating reduced dtm
reduced.dtm.2013 <- trim(dtm.2013,minCount=10,minDoc=5)

rownames(reduced.dtm.2013)

refscores.2013 <- rep(NA,nrow(reduced.dtm.2013))
#Russia
refscores.2013[146] <- -1
#USA
refscores.2013[180] <- 1

#classical wordscores
ws.2013 <- textmodel(reduced.dtm.2013, 
                        refscores.2013,
                        model="wordscores", 
                        scale="linear", 
                        smooth=1)

#using Martin-Vanberg transform
wordscore.2013.mv <- predict(ws.2013, rescaling="mv")


# saving scores into data frame for plotting and analysis

wordscores.2013 <- data.frame(cbind(subset(docvars(debates.corpus),Year==2013),
                                       wordscore.2013.mv@textscores$textscore_mv))


names(wordscores.2013)

write.csv(wordscores.2013,file="wordscores.2013.csv")


# Map with WORDSCORES

sPDF <- joinCountryData2Map(subset(wordscores.2013,Year==2013),
                            joinCode="ISO3",nameJoinColumn="Country")

#Setting up class intervals for continuous variable
classInt <- classIntervals(sPDF$wordscore.2013.mv.textscores.textscore_mv,
                           style="bclust")


catMethod=classInt$brks

#Selecting diverging palette
colourPalette <- brewer.pal(9,"Blues")
#colourPalette <- brewer.pal(9,"PuOr")

#Drawing the map

mapParams <- mapCountryData(sPDF,nameColumnToPlot="wordscore.2013.mv.textscores.textscore_mv", 
                            #catMethod="pretty", 
                            catMethod=catMethod,
                            mapTitle="Russia vs USA from the 2013 UN General Debate",
                            colourPalette=colourPalette,
                            #oceanCol="lightblue",
                            missingCountryCol="grey", 
                            addLegend="FALSE"
                            #borderCol="black"
)

#adding legend
do.call( addMapLegend, c( mapParams, legendLabels="limits", 
                          labelFontSize=0.7,legendShrink=0.7,
                          legendMar=5.7, legendWidth=0.6))


#######   Ukraine Case: stat analysis  ###########

ukraine <- read.dta("./New Analysis April 2015/Ukraine.dta")


glm.resolution <- glm(resolutionalt ~ log_newGDPpc + 
                        p_polity2 + 
                        ws2013cl_mvsmoothRUkr_tsmv +
                        rusgasdepend +
                        NATO, 
               data=ukraine,
               family = binomial)

summary(glm.resolution)


glm.sanctions <- glm(sanctions ~ log_newGDPpc + 
                        p_polity2 + 
                        ws2013cl_mvsmoothRUkr_tsmv +
                        rusgasdepend +
                        NATO, 
                      data=ukraine,
                      family = binomial)

summary(glm.sanctions)


glm.resolution2 <- glm(resolutionalt2 ~ log_newGDPpc + 
                        p_polity2 + 
                        ws2013cl_mvsmoothRUkr_tsmv +
                        rusgasdepend +
                        NATO, 
                      data=ukraine,
                      family = binomial)

summary(glm.resolution2)


texreg(list(glm.resolution, glm.sanctions, glm.resolution2), 
       stars = c(0.001, 0.01, 0.05, 0.10))

#Coefficient plots
arm::coefplot(glm.resolution, main = "Vote on Resolution 68/262",
              varnames = c("intercept", "GDPpc", "Polity", "Wordscore", "Gas", "NATO"), 
              col.pts="red")


arm::coefplot(glm.sanctions, main = "Sanctions",
              varnames = c("intercept", "GDPpc", "Polity", "Wordscore", "Gas", "NATO"), 
              col.pts="red", CI = 2)


#Alternative way to create coefficient plots
library(ggplot2)

# Create a model to plot
coefs <-  as.data.frame(summary(glm.sanctions)$coefficients[-1,1:2])
names(coefs)[2] <-  "se" 
coefs$vars <-  rownames(coefs)

ggplot(coefs, aes(vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="red", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="blue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") +
  theme_bw()



#### Ukraine Case: RandomForest analysis ####

library(randomForest)

rf.resolution <- randomForest(as.factor(resolutionalt) ~ log_newGDPpc + 
                        p_polity2 + 
                        ws2013cl_mvsmoothRUkr_tsmv +
                        rusgasdepend +
                        NATO, 
                      data=ukraine, na.action = na.omit, importance = TRUE)

varImpPlot(rf.resolution)

imp2 <- as.data.frame(importance(rf.resolution))

barplot(sort(imp2$MeanDecreaseGini), horiz = TRUE, 
        names.arg = c("NATO", "Gas dependency",  "GDPpc", "Polity", "EU/Rus Position"), cex.names=0.8,
        xlim = c(0,24), col = "red", main = "Resolution 68/262", xlab = "Variable Importance")



rf.sanctions <- randomForest(as.factor(sanctions) ~ log_newGDPpc + 
                       p_polity2 + 
                       ws2013cl_mvsmoothRUkr_tsmv +
                       rusgasdepend +
                       NATO, 
                     data=ukraine, na.action = na.omit, importance = TRUE)

varImpPlot(rf.sanctions)

imp1 <- as.data.frame(importance(rf.sanctions))

barplot(sort(imp1$MeanDecreaseGini), horiz = TRUE,
        names.arg = c("EU/Rus Position", "GDPpc", "Gas dependency", "Polity", "NATO"), 
        cex.names=0.8, xlim = c(0,20), col = "blue", main = "Sanctions", xlab = "Variable Importance")




rf.resolution2 <- randomForest(as.factor(resolutionalt2) ~ log_newGDPpc + 
                                p_polity2 + 
                                ws2013cl_mvsmoothRUkr_tsmv +
                                rusgasdepend +
                                NATO, 
                              data=ukraine, na.action = na.omit, importance = TRUE)

varImpPlot(rf.resolution2, main = "Resolution")

imp <- as.data.frame(importance(rf.resolution2))

barplot(sort(imp$MeanDecreaseGini), horiz = TRUE, 
        names.arg = c("NATO", "Gas dependency", "Polity", "GDPpc", "EU/Rus Position"), cex.names=0.8,
        xlim = c(0,5), col = c("darkred","red4", "red3", "red2", "red1", "red"), 
        main = "Resolution 68/262", xlab = "Variable Importance")



####  Heatmap   ####


ggplot(data=scores.results, aes(Country, Year)) +
 geom_tile(aes(fill = CA1standard), colour = "white") +
 scale_fill_gradient2(name="Value",low="red", high="steelblue", space = "Lab",na.value = "grey5", guide = "colourbar") +
  theme(axis.text.x = element_text(size = 2, angle = 90)) +
  theme(axis.text.y = element_text(size = 5)) +
  theme(axis.ticks=element_blank()) +
   ggtitle('CA 1 with time') +
 theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(panel.background = element_rect(fill = "gray60")) +
  scale_y_discrete(limits = seq(1970,2010, 10))
  

ggsave("CA1standardheatmap.pdf")


ggplot(data=scores.results, aes(Country, Year)) +
  geom_tile(aes(fill = CCA1), colour = "white") +
  scale_fill_gradient2(name="Value",low="red", high="steelblue", space = "Lab",na.value = "grey5", guide = "colourbar") +
  theme(axis.text.x = element_text(size = 2, angle = 90)) +
  theme(axis.text.y = element_text(size = 5)) +
  theme(axis.ticks=element_blank()) +
  ggtitle('CCA 1') +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(panel.background = element_rect(fill = "gray60")) +
  scale_y_discrete(limits = seq(1970,2010, 10))


ggsave("CCA1heatmap.pdf")



####  Summary Stats for Ukraine Analysis   ####

ukraine <- haven::read_dta("./New Analysis April 2015/Ukraine.dta")

smallukr <- dplyr::select(ukraine, ws2013cl_mvsmoothRUkr_tsmv, 
                          resolutionalt, resolutionalt2, sanctions, 
                          log_newGDPpc, p_polity2, 
                          rusgasdepend, NATO)

psych::df2latex(describe(smallukr, skew = FALSE))
