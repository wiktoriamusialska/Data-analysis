library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)

# Wczytanie danych ----
badana <- read_excel("badana.xlsx")
str(badana)
badana$CRP <- as.numeric(badana$CRP)
badana$WBC <- as.numeric(badana$WBC)
colnames(badana)

kontrola <- read_excel("kontrola.xlsx")
str(kontrola)
colnames(kontrola)
colnames(kontrola) <- colnames(badana)

badana <- badana %>% mutate(grupa="badana")
kontrola <- kontrola %>% mutate(grupa="kontrola")
dane <- rbind(badana, kontrola) %>% mutate(grupa = as.factor(grupa))


# Wiek ----
sum(is.na(badana$wiek))
summary(badana$wiek)

sum(is.na(kontrola$wiek))
summary(kontrola$wiek)


# Wzrost ----
sum(is.na(badana$wzrost))
sum(is.na(kontrola$wzrost))

summary(badana$wzrost)
badana$nr[which(badana$wzrost == 0)]
summary(badana$wzrost[-which(badana$wzrost == 0)])
sort(badana$wzrost)
summary(kontrola$wzrost)


# Waga ----
sum(is.na(badana$waga))
sum(is.na(kontrola$waga))

summary(badana$waga)
badana$nr[which(badana$waga == 0)]
summary(badana$waga[-which(badana$waga == 0)])
summary(kontrola$waga)

# BMI ----
badana1 <- badana[-which(is.na(badana$BMI)),]

for (i in 1:dim(badana1)[1]) {
  if (badana1[i, "BMI"] < 16) {
    badana1[i, "BMI_kat"] = "wygłodzenie"
  }
  else if (badana1[i, "BMI"] < 17) {
    badana1[i, "BMI_kat"] = "wychudzenie"
  }
  else if (badana1[i, "BMI"] < 18.5) {
    badana1[i, "BMI_kat"] = "niedowaga"
  }
  else if (badana1[i, "BMI"] < 25) {
    badana1[i, "BMI_kat"] = "waga prawidłowa"
  }
  else if (badana1[i, "BMI"] < 30) {
    badana1[i, "BMI_kat"] = "nadwaga"
  }
  else {
    badana1[i, "BMI_kat"] = "otyłość"
  }
}

table(badana1$BMI_kat)
badana1$BMI_kat <- factor(badana1$BMI_kat, levels=c("waga prawidłowa", "nadwaga", "otyłość")) 
table(badana1$BMI_kat)

p1 <- ggplot(badana1, aes(x=BMI_kat)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black", fill="plum2") + 
  theme_bw() + 
  labs(x="Kategoria BMI", y="Odsetek",
       title="Grupa badana") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(limits=c(0,1))


for (i in 1:dim(kontrola)[1]) {
  if (kontrola[i, "BMI"] < 16) {
    kontrola[i, "BMI_kat"] = "wygłodzenie"
  }
  else if (kontrola[i, "BMI"] < 17) {
    kontrola[i, "BMI_kat"] = "wychudzenie"
  }
  else if (kontrola[i, "BMI"] < 18.5) {
    kontrola[i, "BMI_kat"] = "niedowaga"
  }
  else if (kontrola[i, "BMI"] < 25) {
    kontrola[i, "BMI_kat"] = "waga prawidłowa"
  }
  else if (kontrola[i, "BMI"] < 30) {
    kontrola[i, "BMI_kat"] = "nadwaga"
  }
  else {
    kontrola[i, "BMI_kat"] = "otyłość"
  }
}

table(kontrola$BMI_kat)
kontrola$BMI_kat <- factor(kontrola$BMI_kat, levels=c("niedowaga", "waga prawidłowa", "nadwaga")) 
table(kontrola$BMI_kat)

p2 <- ggplot(kontrola, aes(x=BMI_kat)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black", fill="lightgreen") + 
  theme_bw() + 
  labs(x="Kategoria BMI", y="Odsetek",
       title="Grupa kontrolna") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt")) + 
  scale_y_continuous(limits=c(0,1))

grid.arrange(p1, p2, ncol=2)


# CKD-EPI ----
sum(is.na(badana$`CKD-EPI`))
summary(badana$`CKD-EPI`)

sum(is.na(kontrola$`CKD-EPI`))
summary(kontrola$`CKD-EPI`)

p1 <- ggplot(data=dane, aes(x=grupa, y=`CKD-EPI`, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot() + 
  theme_bw() +  
  labs(x="", y="CKD-EPI") + 
  scale_fill_manual(values = list("badana"="plum2", "kontrola"="lightgreen")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none")

p2 <- ggplot(data=dane, aes(x=`CKD-EPI`, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="CKD-EPI", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("plum2", "lightgreen")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2), 
             top=textGrob("Rozkład CKD-EPI w grupach",
                          gp=gpar(fontsize=26)))



# CKD stage ---- 
unique(badana$`CKD stage`)
library(RColorBrewer)
p1 <- ggplot(data=badana, aes(x=`CKD stage`, y=`CKD-EPI`, fill=`CKD stage`)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot() + 
  theme_bw() +  
  labs(x="", y="CKD-EPI",
       title="Grupa badana") +  
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_fill_brewer(palette = "Set2")

p2 <- ggplot(data=kontrola, aes(x=`CKD stage`, y=`CKD-EPI`, fill=`CKD stage`)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot() + 
  theme_bw() +  
  labs(x="", y="CKD-EPI",
       title="Grupa kontrolna") +  
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_fill_brewer(palette = "Set2")

grid.arrange(p1, p2, ncol=2)


for (i in 1:dim(dane)[1]) {
  if (dane[i, "CKD-EPI"] < 15) {
    dane[i, "CKD stage"] = "G5"
  }
  else if (dane[i, "CKD-EPI"] < 30) {
    dane[i, "CKD stage"] = "G4"
  }
  else if (dane[i, "CKD-EPI"] < 45) {
    dane[i, "CKD stage"] = "G3B"
  }
  else if (dane[i, "CKD-EPI"] < 60) {
    dane[i, "CKD stage"] = "G3A"
  }
  else if (dane[i, "CKD-EPI"] < 90) {
    dane[i, "CKD stage"] = "G2"
  }
  else {
    dane[i, "CKD stage"] = "G1"
  }
}

badana <- dane[dane$grupa == "badana",]
kontrola <- dane[dane$grupa == "kontrola",]

p1 <- ggplot(data=badana, aes(x=`CKD stage`, y=`CKD-EPI`, fill=`CKD stage`)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot() + 
  theme_bw() +  
  labs(x="", y="CKD-EPI",
       title="Grupa badana") +  
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_fill_brewer(palette = "Set2")

p2 <- ggplot(data=kontrola, aes(x=`CKD stage`, y=`CKD-EPI`, fill=`CKD stage`)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot() + 
  theme_bw() +  
  labs(x="", y="CKD-EPI",
       title="Grupa kontrolna") +  
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_fill_brewer(palette = "Set2")

grid.arrange(p1, p2, ncol=2)


table(badana$`CKD stage`)
table(kontrola$`CKD stage`)

p1 <- ggplot(badana, aes(x=`CKD stage`, fill=`CKD stage`)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black") +
  theme_bw() + 
  labs(x="CKD stage", y="Odsetek",
       title="Grupa badana") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt")) + 
  scale_y_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette = "Set2")

p2 <- ggplot(kontrola, aes(x=`CKD stage`, fill=`CKD stage`)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black") +
  theme_bw() + 
  labs(x="CKD stage", y="Odsetek",
       title="Grupa badana") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt")) + 
  scale_y_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette = "Set2")

grid.arrange(p1, p2, ncol=2)

