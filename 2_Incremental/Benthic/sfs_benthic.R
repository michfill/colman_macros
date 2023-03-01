############ SFS Meeting Data Analysis #########

##  For the SFS conference, Im going to start a brief analysis of benthic data from 2021. 
#   I will be plotting total density, %Chiro, and %EPT, then running an ANOVA and a 
#   Tukeys HSD.
##  
# ------------------------------------------------------------------------------

# Install new packages

install.packages("multcompView")
install.packages("ggthemes")

# Load Libraries 
library(tidyverse)
library (ggplot2)
library(agricolae)
library(ggthemes)
library(multcompView)
library(dplyr)

# read in data
benth <- read.csv("SFS_benthic_working.csv",head=T, sep=",")

# Box plots of EPT for BDA reaches vs reference reaches with no ANOVA or Post hoc

benth %>%
  group_by(treat) %>%
  filter(site=="lp") %>%
  ggplot(aes(x=treat, y=total)) +
  geom_boxplot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = 'Total Abundance', y = '') +
  ggtitle(expression(atop(bold("Lost Prairie Creek"), ""))) +
  theme(plot.title = element_text(hjust = 0.5, face='bold')) +
  geom_boxplot(fill = 'darkslategray1')


benth %>% 
  group_by(treat) %>%
  filter(site=="fish") %>%
  ggplot(aes(x=treat, y=total)) +
  geom_boxplot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = 'Total Abundance', y = '') +
  ggtitle(expression(atop(bold("Fish Creek"), ""))) +
  ylim(0,750) +
  theme(plot.title = element_text(hjust = 0.5, face='bold')) +
  geom_boxplot(fill = 'darkslategray1')


benth %>% 
  group_by(treat) %>%
  filter(site=="tp") %>%
  ggplot(aes(x=treat, y=total)) +
  geom_boxplot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = 'Total Abundance', y = '') +
  ggtitle(expression(atop(bold("Fish Creek"), ""))) +
  ylim(0,750) +
  theme(plot.title = element_text(hjust = 0.5, face='bold')) +
  geom_boxplot(fill = 'darkslategray1')


### By ggplot2 looking at Total Abundance 
### Tukey-HSD

benth %>%
 group_by(site,treat) %>%

summarize(lpmax_value = max(total))

lphsd=HSD.test(aov(total~site, treat, data=benth), trt = "treat", group = T)

lpsig.letters <-lphsd$groups[order(row.names(lphsd$groups)), ]

lpbenth %>%
  ggplot(aes(x = treat, y = total))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_boxplot(fill= c("darkolivegreen", "darkolivegreen3"))+
  geom_text(data = lpvalue_max, aes(x=treat, y = 5 + lpmax_value, label = lpsig.letters$groups), vjust=2)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = 'Total Abundance') +
  ylim(0,1700) +
  theme(text = element_text(size = 20)) + 
  ggtitle(expression(atop(bold("Lost Prairie Creek"), ""))) 




############## New Box Plot ################################

# put in a theme
# things that you want to put in a presentation, name the figuer
# cowplot, save_plot function and tell the directory you want it to geo in

bogus <- data.frame(site = c("fish", "lp", "tp"),
                    sig = c("", "*", ""))


ggplot() +  # simple plot with all the streams and treats
  geom_boxplot(data = benth, aes(x=site, y=total, fill=treat)) +
  geom_text(data = bogus, aes(x = site, y = 1000, label = sig), size = 10) +
  scale_fill_manual(breaks = c("bda", "ref"), 
                    values=c("darkolivegreen", "darkolivegreen3")) +
  theme_classic()
  

bogus2 <- data.frame(site = c("fish", "lp", "tp"),
                    sig = c("*", "*", "*"))
ggplot() +  # simple plot with all the streams and treats
  geom_boxplot(data = benth, aes(x=site, y=perchiro, fill=treat)) +
  geom_text(data = bogus2, aes(x = site, y = 100, label = sig), size = 10) +
  scale_fill_manual(breaks = c("bda", "ref"), 
                    values=c("darkslategray1", "darkslategray4")) +
  labs(x = 'Site', y = '% Chironomid') +
  theme_classic()

bogus3 <- data.frame(site = c("fish", "lp", "tp"),
                     sig = c("*", "", "*"))
ggplot() +  # simple plot with all the streams and treats
  geom_boxplot(data = benth, aes(x=site, y=perept, fill=treat)) +
  geom_text(data = bogus3, aes(x = site, y = 100, label = sig), size = 10) +
  scale_fill_manual(breaks = c("bda", "ref"), 
                    values=c("gold", "gold3")) +
  labs(x = 'Site', y = '% EPT') +
  theme_classic()
  
  


benth_mods <-   # coding club, not too sure what this is
  benth %>%
  group_by(site) %>%
  summarise(aov = list(aov(total~treat))) %>%
  rowwise() %>%
  mutate(groups = list(HSD.test(aov, trt = "treat", group = TRUE)$groups))

benth_new <-  #trying a new way to do it
  benth %>%
  group_by(site) # group by site
# anova
anova <- aov(total~ site/treat, data = benth_new) # how to treat a nested sample
# hsd
tukey <- TukeyHSD(anova)
# compact letter display
cld <- multcompLetters4(anova, tukey)
# table with factors and 3rd quantile
dt <- benth_new %>% group_by(site, treat) %>%
  summarise(w=mean(total), sd = sd(total), .groups = "keep") %>% 
  arrange(desc(w))
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$`site:treat`)
dt$cld <- cld$Letters

print(dt)

# plot
ggplot(dt, aes(site, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "Site", y = "Total") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
  ylim(0,1000) +
  theme_few()
############################################


# anova
anova <- aov(total~ site/treat, data = benth_new) # how to treat a nested sample
# hsd
tukey <- TukeyHSD(anova)
# compact letter display
cld <- multcompLetters4(anova, tukey)
# table with factors and 3rd quantile
dt <- benth_new %>% group_by(site, treat) %>%
  summarise(w=mean(total), sd = sd(total), .groups = "keep") %>% 
  arrange(desc(w))
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$`site:treat`)
dt$cld <- cld$Letters

print(dt)

# plot
ggplot(dt, aes(site, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "Site", y = "Total") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
  ylim(0,1000) +
  theme_few()

# Lost Prairie

lpbenth<-benth %>%
  filter(site=="lp")

lpvalue_max = lpbenth %>% group_by(treat) %>% summarize(lpmax_value = max(total))

lphsd=HSD.test(aov(total~treat, data=lpbenth), trt = "treat", group = T)

lpsig.letters <-lphsd$groups[order(row.names(lphsd$groups)), ]

lpbenth %>%
  ggplot(aes(x = treat, y = total))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_boxplot(fill= c("darkolivegreen", "darkolivegreen3"))+
  geom_text(data = lpvalue_max, aes(x=treat, y = 5 + lpmax_value, label = lpsig.letters$groups), vjust=2)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = 'Total Abundance') +
  ylim(0,1700) +
  theme(text = element_text(size = 20)) + 
  ggtitle(expression(atop(bold("Lost Prairie Creek"), ""))) 


# Fish

fbenth<-benth %>%
  filter(site=="fish")

fvalue_max = fbenth %>% group_by(treat) %>% summarize(fmax_value = max(total))

fhsd=HSD.test(aov(total~treat, data=fbenth), trt = "treat", group = T)

fsig.letters <-fhsd$groups[order(row.names(fhsd$groups)), ]

fbenth %>%
  ggplot(aes(x = treat, y = total))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_boxplot(fill= c("darkslategray1", "darkslategray4"))+
  geom_text(data = fvalue_max, aes(x=treat, y = -2 + fmax_value, label = fsig.letters$groups), vjust=2)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = 'Total Abundance') +
  ylim(0,750) +
  theme(text = element_text(size = 20)) + 
  ggtitle(expression(atop(bold("Fish Creek"), ""))) 


# Teepee

tpbenth<-benth %>%
  filter(site=="tp")

tpvalue_max = tpbenth %>% group_by(treat) %>% summarize(tpmax_value = max(total))

tphsd=HSD.test(aov(total~treat, data=tpbenth), trt = "treat", group = T)

tpsig.letters <-tphsd$groups[order(row.names(tphsd$groups)), ]

tpbenth %>%
  ggplot(aes(x = treat, y = total))+ 
  geom_boxplot(fill= c("gold3", "gold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = tpvalue_max, aes(x=treat, y = 0.1 + tpmax_value, label = tpsig.letters$groups), vjust=3)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = 'Total Abundance') +
  ylim(0,750) +
  theme(text = element_text(size = 20)) +  
  ggtitle(expression(atop(bold("Teepee Creek"), ""))) 



### By ggplot2 looking at percent chironomid 
### Tukey-HSD

# Lost Prairie

lpbenth<-benth %>%
  filter(site=="lp")

lpvalue_max = lpbenth %>% group_by(treat) %>% summarize(lpmax_value = max(perchiro))

lphsd=HSD.test(aov(perchiro~treat, data=lpbenth), trt = "treat", group = T)

lpsig.letters <-lphsd$groups[order(row.names(lphsd$groups)), ]

lpbenth %>%
  ggplot(aes(x = treat, y = perchiro))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_boxplot(fill= c("darkolivegreen", "darkolivegreen3"))+
  geom_text(data = lpvalue_max, aes(x=treat, y = 5 + lpmax_value, label = lpsig.letters$groups), vjust=3)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = '% Chironomid') +
  ylim(0,100) +
  theme(text = element_text(size = 20)) + 
  ggtitle(expression(atop(bold("Lost Prairie Creek"), ""))) 


# Fish

fbenth<-benth %>%
  filter(site=="fish")

fvalue_max = fbenth %>% group_by(treat) %>% summarize(fmax_value = max(perchiro))

fhsd=HSD.test(aov(perchiro~treat, data=fbenth), trt = "treat", group = T)

fsig.letters <-fhsd$groups[order(row.names(fhsd$groups)), ]

fbenth %>%
  ggplot(aes(x = treat, y = perchiro))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_boxplot(fill= c("darkslategray1", "darkslategray4"))+
  geom_text(data = fvalue_max, aes(x=treat, y = 100, label = fsig.letters$groups), vjust=15)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = '% Chironomid') +
  ylim(0,100) +
  theme(text = element_text(size = 20)) + 
  ggtitle(expression(atop(bold("Fish Creek"), ""))) 


# Teepee

tpbenth<-benth %>%
  filter(site=="tp")

tpvalue_max = tpbenth %>% group_by(treat) %>% summarize(tpmax_value = max(perchiro))

tphsd=HSD.test(aov(perchiro~treat, data=tpbenth), trt = "treat", group = T)

tpsig.letters <-tphsd$groups[order(row.names(tphsd$groups)), ]

tpbenth %>%
  ggplot(aes(x = treat, y = perchiro))+ 
  geom_boxplot(fill= c("gold3", "gold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = tpvalue_max, aes(x=treat, y = 0.1 + tpmax_value, label = tpsig.letters$groups), vjust=3)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = '% Chironomid') +
  ylim(0,100) +
  theme(text = element_text(size = 20)) +  
  ggtitle(expression(atop(bold("Teepee Creek"), ""))) 



## ggplot looking at % EPT
### Tukey-HSD

# Lost Prairie
lpbenth<-benth %>%
  filter(site=="lp")

lpvalue_max = lpbenth %>% group_by(treat) %>% summarize(lpmax_value = max(perept))

lphsd=HSD.test(aov(perept~treat, data=lpbenth), trt = "treat", group = T)

lpsig.letters <-lphsd$groups[order(row.names(lphsd$groups)), ]

lpbenth %>%
  ggplot(aes(x = treat, y = perept))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_boxplot(fill= c("darkolivegreen", "darkolivegreen3"))+
  geom_text(data = lpvalue_max, aes(x=treat, y = 5 + lpmax_value, label = lpsig.letters$groups), vjust=5)+
  stat_boxplot(geom = 'errorbar', width = 0.2)+
  labs(x = 'Treatment', y = '% EPT') +
  ylim(0,110) +
  theme(text = element_text(size = 20)) + 
  ggtitle(expression(atop(bold("Lost Prairie Creek"), ""))) 


# Fish

fbenth<-benth %>%
  filter(site=="fish")

fvalue_max = fbenth %>% group_by(treat) %>% summarize(fmax_value = max(perept))

fhsd=HSD.test(aov(perept~treat, data=fbenth), trt = "treat", group = T)

fsig.letters <-fhsd$groups[order(row.names(fhsd$groups)), ]

fbenth %>%
  ggplot(aes(x = treat, y = perept))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_boxplot(fill= c("darkslategray1", "darkslategray4"))+
  geom_text(data = fvalue_max, aes(x=treat, y = -2 + fmax_value, label = fsig.letters$groups), vjust=15)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = '% EPT') +
  ylim(0,100) +
  theme(text = element_text(size = 20)) + 
  ggtitle(expression(atop(bold("Fish Creek"), ""))) 


# Teepee

tpbenth<-benth %>%
  filter(site=="tp")

tpvalue_max = tpbenth %>% group_by(treat) %>% summarize(tpmax_value = max(perept))

tphsd=HSD.test(aov(perept~treat, data=tpbenth), trt = "treat", group = T)

tpsig.letters <-tphsd$groups[order(row.names(tphsd$groups)), ]

tpbenth %>%
  ggplot(aes(x = treat, y = perept))+ 
  geom_boxplot(fill= c("gold3", "gold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = tpvalue_max, aes(x=treat, y = 0.1 + tpmax_value, label = tpsig.letters$groups), vjust=15)+
  stat_boxplot(geom = 'errorbar', width = 0.1)+
  labs(x = 'Treatment', y = '% EPT') +
  ylim(0,100) +
  theme(text = element_text(size = 20)) +  
  ggtitle(expression(atop(bold("Teepee Creek"), ""))) 



## Other stuff you might need later
emerg %>% 
  group_by(treat) %>%
  filter(site=="lp") %>%
  ggplot(aes(x=treat, y=ept)) +
  geom_boxplot()+
  labs(x="", y="EPT", title = "Lost Prairie")

emerg %>% 
  group_by(treat) %>%
  filter(site=="fish") %>%
  ggplot(aes(x=treat, y=ept)) +
  geom_boxplot()+
  labs(x="", y="EPT", title = "Fish")

emerg %>% 
  group_by(treat) %>%
  filter(site=="tp") %>%
  ggplot(aes(x=treat, y=ept)) +
  geom_boxplot()+
  labs(x="", y="EPT", title = "Teepee")


