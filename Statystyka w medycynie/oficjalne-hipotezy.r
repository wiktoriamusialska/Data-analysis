library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(moments)
library(GGally)

# Wczytanie danych ----
badana <- read_excel("badana.xlsx")
str(badana)
badana$CRP <- as.numeric(badana$CRP)
badana$WBC <- as.numeric(badana$WBC)
colnames(badana)

kontrola <- read_excel("kontrola.xlsx")
str(kontrola)
kontrola$WBC <- as.numeric(kontrola$WBC)
colnames(kontrola)
colnames(kontrola) <- colnames(badana)

badana <- badana %>% mutate(grupa="badana")
kontrola <- kontrola %>% mutate(grupa="kontrola")
dane <- rbind(badana, kontrola) %>% mutate(grupa = as.factor(grupa))

# CKD-EPI ----
summary(badana$`CKD-EPI`)
summary(kontrola$`CKD-EPI`)

p1 <- ggplot(data=dane, aes(x=grupa, y=`CKD-EPI`, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE) + 
  theme_bw() +  
  labs(x="", y="CKD-EPI") + 
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=dane, aes(x=`CKD-EPI`, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="CKD-EPI", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2))

# Wartości odstające
iqr <- IQR(kontrola$`CKD-EPI`)
q1 <- quantile(kontrola$`CKD-EPI`, probs=0.25)
q3 <- quantile(kontrola$`CKD-EPI`, probs=0.75)
ids <- which(kontrola$`CKD-EPI` < q1 - 1.5*iqr | kontrola$`CKD-EPI` > q3 + 1.5*iqr)
ids
kontrola[ids, c("nr", "CKD-EPI")]

# Badanie skośności
agostino.test(badana$`CKD-EPI`)
agostino.test(kontrola$`CKD-EPI`)

# Badanie normalności
shapiro.test(badana$`CKD-EPI`)
shapiro.test(kontrola$`CKD-EPI`)

# Test t-Studneta
t.test(badana$`CKD-EPI`, kontrola$`CKD-EPI`, paired=FALSE,
       correct=TRUE, alternative="two.sided")
t.test(badana$`CKD-EPI`, kontrola$`CKD-EPI`, paired=FALSE,
            correct=TRUE, alternative="less")


# CRP ----
summary(badana$CRP)
summary(kontrola$CRP)

p1 <- ggplot(data=dane, aes(x=grupa, y=CRP, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE) + 
  theme_bw() +  
  labs(x="", y="CRP") + 
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=dane, aes(x=CRP, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="CRP", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2))

# Wartości odstające
iqr <- IQR(badana$CRP)
q3 <- quantile(badana$CRP, probs=0.75)
ids <- which(badana$CRP > q3 + 1.5*iqr)
badana[ids, c("nr", "CRP")]

# Badanie skośności
agostino.test(badana$CRP)
agostino.test(kontrola$CRP)

# Sprawdzenie normalności
shapiro.test(badana$CRP)
shapiro.test(kontrola$CRP)

# Test U Manna Whitneya
wilcox.test(badana$CRP, kontrola$CRP, paired=FALSE,
            correct=TRUE, alternative="two.sided", exact=FALSE) 
wilcox.test(badana$CRP, kontrola$CRP, paired=FALSE,
            correct=TRUE, alternative="greater", exact=FALSE) # warning

# fibrynogen ----
summary(badana$fibrynogen)
summary(kontrola$fibrynogen)

p1 <- ggplot(data=dane, aes(x=grupa, y=fibrynogen, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE) + 
  theme_bw() +  
  labs(x="", y="fibrynogen") + 
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=dane, aes(x=fibrynogen, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="fibrynogen", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2))

# Wartości odstające
iqr <- IQR(badana$fibrynogen)
q1 <- quantile(badana$fibrynogen, probs=0.25)
ids <- which(badana$fibrynogen < q1 - 1.5*iqr)
badana[ids, c("nr", "fibrynogen")]

# Badanie skośności
agostino.test(badana$fibrynogen)
agostino.test(kontrola$fibrynogen)

# Sprawdzenie normalności
shapiro.test(badana$fibrynogen)
shapiro.test(kontrola$fibrynogen)

# Test t-Studenta
t.test(badana$fibrynogen, kontrola$fibrynogen, paired=FALSE,
       correct=TRUE, alternative="two.sided")
t.test(badana$fibrynogen, kontrola$fibrynogen, paired=FALSE,
            correct=TRUE, alternative="greater")


# PLT ----
summary(badana$PLT)
summary(kontrola$PLT)

p1 <- ggplot(data=dane, aes(x=grupa, y=PLT, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE) + 
  theme_bw() +  
  labs(x="", y="PLT") + 
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=dane, aes(x=PLT, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="PLT", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2))

# Wartości odstające
iqr <- IQR(kontrola$PLT)
q3 <- quantile(kontrola$PLT, probs=0.75)
ids <- which(kontrola$PLT > q3 + 1.5*iqr)
kontrola[ids, c("nr", "PLT")]

# Badanie skośności
agostino.test(badana$PLT)
agostino.test(kontrola$PLT)

# Sprawdzenie normalności
shapiro.test(badana$PLT)
shapiro.test(kontrola$PLT)

# Test t-Studenta
t.test(badana$PLT, kontrola$PLT, paired=FALSE,
       correct=TRUE, alternative="two.sided")
t.test(badana$PLT, kontrola$PLT, paired=FALSE,
            correct=TRUE, alternative="greater")


# WBC ----
summary(badana$WBC)
summary(kontrola$WBC)

p1 <- ggplot(data=dane, aes(x=grupa, y=WBC, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE) + 
  theme_bw() +  
  labs(x="", y="WBC") + 
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=dane, aes(x=WBC, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="WBC", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2))

# Wartości odstające
iqr <- IQR(badana$WBC)
q3 <- quantile(badana$WBC, probs=0.75)
ids <- which(badana$WBC > q3 + 1.5*iqr)
badana[ids, c("nr", "WBC")]

# Badanie skośności
agostino.test(badana$WBC)
agostino.test(kontrola$WBC)

# Sprawdzanie normalności
shapiro.test(badana$WBC)
shapiro.test(kontrola$WBC)

# Test t-Studenta
t.test(badana$WBC, kontrola$WBC, paired=FALSE,
       correct=TRUE, alternative="two.sided")
t.test(badana$WBC, kontrola$WBC, paired=FALSE,
            correct=TRUE, alternative="greater")


# albumina ----
summary(badana$albumina)
badana[which(badana$albumina == 0),"nr"]
badana1 <- badana[-which(badana$albumina == 0), ]
summary(badana1$albumina)
summary(kontrola$albumina)

dane1 <- rbind(badana1, kontrola) %>% mutate(grupa = as.factor(grupa))


p1 <- ggplot(data=dane1, aes(x=grupa, y=albumina, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE) + 
  theme_bw() +  
  labs(x="", y="albumina") + 
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=dane1, aes(x=albumina, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="albumina", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2))

# Badanie skośności
agostino.test(badana1$albumina)
agostino.test(kontrola$albumina)

# Sprawdzenie normalności
shapiro.test(badana1$albumina)
shapiro.test(kontrola$albumina)

# Test t-Studenta
t.test(badana1$albumina, kontrola$albumina, paired=FALSE,
       correct=TRUE, alternative="two.sided")
t.test(badana1$albumina, kontrola$albumina, paired=FALSE,
       correct=TRUE, alternative="less")


# Kreatynina ----
summary(badana$kreatynina)
summary(kontrola$kreatynina)


p1 <- ggplot(data=dane, aes(x=grupa, y=kreatynina, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE) + 
  theme_bw() +  
  labs(x="", y="kreatynina") + 
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=dane, aes(x=kreatynina, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="kreatynina", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2)) 

# Wartości odstające
iqr <- IQR(badana$kreatynina)
q3 <- quantile(badana$kreatynina, probs=0.75)
ids <- which(badana$kreatynina > q3 + 1.5*iqr)
badana[ids, c("nr", "kreatynina")]

# Badanie skośności
agostino.test(badana$kreatynina)
agostino.test(kontrola$kreatynina)

# Sprawdzenie normalności
shapiro.test(badana$kreatynina)
shapiro.test(kontrola$kreatynina)

# Test U Manna Whitneya
wilcox.test(badana$kreatynina, kontrola$kreatynina, paired=FALSE,
            correct = TRUE, alternative="two.sided")
wilcox.test(badana$kreatynina, kontrola$kreatynina, paired=FALSE,
            correct = TRUE, alternative="greater")


# UA ----
summary(badana$UA)
badana[which(badana$UA == 0), "nr"]
badana1 <- badana[-which(badana$UA == 0),]
summary(badana1$UA)
summary(kontrola$UA)


dane1 <- rbind(badana1, kontrola) %>% mutate(grupa = as.factor(grupa))

p1 <- ggplot(data=dane1, aes(x=grupa, y=UA, fill=grupa)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE) + 
  theme_bw() +  
  labs(x="", y="UA") + 
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=dane1, aes(x=UA, fill=grupa)) + 
  geom_density(alpha=0.8) + 
  theme_bw() +  
  labs(x="UA", y="Gęstość") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
  guides(fill = guide_legend(title = "Grupa"))

grid.arrange(p1, p2, ncol=2, widths=c(1,2))

# Wartości odstające
iqr <- IQR(kontrola$UA)
q1 <- quantile(kontrola$UA, probs=0.25)
q3 <- quantile(kontrola$UA, probs=0.75)
kontrola[which(kontrola$UA < q1 - 1.5*iqr | kontrola$UA > q3 + 1.5*iqr), c("nr", "UA")]

# Badanie skośności
agostino.test(badana1$UA)
agostino.test(kontrola$UA)

# Sprawdzenie normalności
shapiro.test(badana1$UA)
shapiro.test(kontrola$UA)

# Test t-Studenta
t.test(badana1$UA, kontrola$UA, paired=FALSE,
       correct=TRUE, alternative="two.sided")
t.test(badana1$UA, kontrola$UA, paired=FALSE,
            correct=TRUE, alternative="greater")


# # Mocznik (?) ----
# summary(badana$mocznik)
# badana$nr[which(badana$mocznik == 0)]
# badana1 <- badana[-which(badana$mocznik == 0),]
# summary(badana1$mocznik)
# summary(kontrola$mocznik)
# 
# 
# dane1 <- rbind(badana1, kontrola) %>% mutate(grupa = as.factor(grupa))
# 
# 
# p1 <- ggplot(data=dane1, aes(x=grupa, y=mocznik, fill=grupa)) + 
#   stat_boxplot(geom = "errorbar",
#                width = 0.15) +
#   geom_boxplot(show.legend = FALSE) + 
#   theme_bw() +  
#   labs(x="", y="Mocznik") + 
#   scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
#   guides(fill = guide_legend(title = "Grupa")) + 
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 18),
#         axis.ticks.x=element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.x=element_blank())
# 
# p2 <- ggplot(data=dane1, aes(x=mocznik, fill=grupa)) + 
#   geom_density(alpha=0.8) + 
#   theme_bw() +  
#   labs(x="Mocznik", y="Gęstość") + 
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 18),
#         plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"),
#         legend.title = element_text(size=16),
#         legend.text = element_text(size=14)) +
#   scale_fill_manual(values = c("#F76D5E", "#72D8FF")) + 
#   guides(fill = guide_legend(title = "Grupa"))
# 
# grid.arrange(p1, p2, ncol=2, widths=c(1,2))
# 
# 
# iqr <- IQR(badana1$mocznik)
# q3 <- quantile(badana1$mocznik, probs=0.75)
# badana1$nr[which(badana1$mocznik > q3 + 1.5*iqr)]
# badana1$mocznik[which(badana1$mocznik > q3 + 1.5*iqr)]
# 
# wilcox.test(badana1$mocznik[-which(badana1$mocznik > q3 + 1.5*iqr)], kontrola$mocznik,
#             paired=FALSE, correct=TRUE)

# Zależności ----
kolumny <- c('nr', 'CKD-EPI', 'CRP', 'fibrynogen', 'PLT', 'WBC', 'albumina', 'kreatynina', 'UA')

badana_trimmed <- badana[,kolumny]
row_sub <- apply(badana_trimmed, 1, function(row) all(row != 0 ))
remove_rows <- apply(badana_trimmed, 1, function(row) any(row == 0 ))
row_sub
sum(remove_rows)
which(row_sub)
which(remove_rows)
badana$nr[which(remove_rows)]
badana_new <- badana_trimmed[row_sub, ]
dim(badana_new)


p1 <- ggplot(data=badana_new, aes(y=`CKD-EPI`)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE, fill="#F76D5E") +
  theme_bw() +
  labs(x="", y="CKD-EPI") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"))

p2 <- ggplot(data=badana_new, aes(y=`CRP`)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE, fill="#F76D5E") +
  theme_bw() +
  labs(x="", y="CRP") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"))

p3 <- ggplot(data=badana_new, aes(y=fibrynogen)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE, fill="#F76D5E") +
  theme_bw() +
  labs(x="", y="fibrynogen") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"))

p4 <- ggplot(data=badana_new, aes(y=PLT)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE, fill="#F76D5E") +
  theme_bw() +
  labs(x="", y="PLT") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"))

p5 <- ggplot(data=badana_new, aes(y=WBC)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE, fill="#F76D5E") +
  theme_bw() +
  labs(x="", y="WBC") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"))

p6 <- ggplot(data=badana_new, aes(y=albumina)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE, fill="#F76D5E") +
  theme_bw() +
  labs(x="", y="albumina") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"))

p7 <- ggplot(data=badana_new, aes(y=kreatynina)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE, fill="#F76D5E") +
  theme_bw() +
  labs(x="", y="kreatynina") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"))

p8 <- ggplot(data=badana_new, aes(y=UA)) +
  stat_boxplot(geom = "errorbar",
               width = 0.15) +
  geom_boxplot(show.legend = FALSE, fill="#F76D5E") +
  theme_bw() +
  labs(x="", y="UA") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 20), "pt"))

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=4)


# korelacje

cor_fun <- function(data, mapping, method="pearson", ndp=4, sz=5, stars=TRUE, ...){
  
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor.test(x, y, method=method)
  est <- corr$estimate
  p <- round(corr$p.value, 4)
  
  if(stars){
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1.01))]
    lbl <- paste0("Rho: ", round(est, ndp), stars, "\np-value: ", p)
  }else{
    lbl <- paste0("Rho: ", round(est, ndp), "\np-value: ", p)
  }
  
  if(stars != "") {
    kolor <- "green"
    alfa <- 0.2
  }
  else {
    kolor <- "snow2"
    alfa <- 0.5
    }
  
  ggplot(data=data, mapping=mapping) + 
    annotate("text", x=mean(x, na.rm=TRUE), y=mean(y, na.rm=TRUE), label=lbl,...)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = alpha(kolor, alfa)))
}


my_fn <- function(data, mapping, method="lm", cor_method="pearson", ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor.test(x, y, method=cor_method)
  p <- round(corr$p.value, 4)
  
  if(p < 0.05) {
    kolor <- "green"
    alfa <- 0.2
  }
  else {
    kolor <- "snow2"
    alfa <- 0.5
  }
  
  ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...) + 
    theme(panel.background = element_rect(fill = alpha(kolor, alfa)))
}



ggpairs(data=badana_new, 
        columns=2:9,
        lower=list(continuous=wrap(my_fn, cor_method="spearman", fill="snow4")),
        diag=list(continuous=wrap("densityDiag", fill="#F76D5E")),
        upper=list(continuous=wrap(cor_fun, method="spearman"))) + 
  # theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        axis.ticks = element_blank())

