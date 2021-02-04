library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(readxl)
library(plotly)
library(see)
library(ggplot2)
library(ggridges)
Ultimate <- read_xlsx("~/Documents/R/BASEBALL/UCSB.xlsx")
Ultimate$VertApprAngle <- as.vector(Ultimate$VertApprAngle)
Ultimate$VertApprAngle<- as.numeric(Ultimate$VertApprAngle )
Ultimate$Extension <- as.vector(Ultimate$Extension)
Ultimate$Extension <- as.numeric(Ultimate$Extension)
Ultimate$RelHeight <- as.numeric(Ultimate$RelHeight )
Ultimate$RelSpeed <- as.numeric(Ultimate$RelSpeed)
Ultimate$VertRelAngle <- as.numeric(Ultimate$VertRelAngle)
Ultimate$HorzRelAngle <- as.numeric(Ultimate$HorzRelAngle)
Ultimate$PlateLocHeight<- as.numeric(Ultimate$PlateLocHeight)
Ultimate$PlateLocSide <- as.numeric(Ultimate$PlateLocSide)
Ultimate$PlateLocSide <- as.vector(Ultimate$PlateLocSide)
Ultimate$PLH <- as.numeric(Ultimate$PLH)
Ultimate$PLS <- as.numeric(Ultimate$PLS)
Ultimate$ZoneSpeed <- as.numeric(Ultimate$ZoneSpeed)
Ultimate$HorzApprAngle <- as.numeric(Ultimate$HorzApprAngle)
Ultimate$ZoneTime <- as.numeric(Ultimate$ZoneTime)
Ultimate$SpinRate <- as.numeric(Ultimate$SpinRate)
Ultimate$SpinAxis <- as.numeric(Ultimate$SpinAxis)
Ultimate$Tilt <- as.numeric(Ultimate$Tilt)
Ultimate$RelSide <- as.numeric(Ultimate$RelSide)
Ultimate$VertBreak <- as.numeric(Ultimate$VertBreak)
Ultimate$InducedVertBreak <- as.numeric(Ultimate$InducedVertBreak)
Ultimate$HorzBreak <- as.numeric(Ultimate$HorzBreak)
Ultimate$ExitSpeed <- as.numeric(Ultimate$ExitSpeed)

Fastballs <- subset(Ultimate, TaggedPitchType == "Fastball")
Ultimate3 <- Fastballs %>%
  summarise(Pitcher, RelSpeed)
Ultimate3 <- na.omit(Ultimate3)

Ultimate3 <- subset(Ultimate3, Pitcher == "Arellano Jr., Jorge"|Pitcher == "Beer, Trevor"|Pitcher == "McGreevy, Michael"|Pitcher == "Boone, Rodney"|Pitcher == "Torra, Zachary"|Pitcher == "Troye, Christopher"|
                  Pitcher == "Adamson, Charlie" | Pitcher == "Dand, Conner"|Pitcher == "Callahan, JD"|Pitcher == "Roberts, Conner"|Pitcher == "Harvey, Ryan"|Pitcher == "Whiting, Samuel"|Pitcher == "Schrier, Alex"
                |Pitcher == "Owen, Noah"|Pitcher == "Lyons, Kevin"|Pitcher == "Lewis, Cory"|Pitcher == "Benbrook, Carter")
Ultimate3 <- subset(Ultimate3, )



p <-  Ultimate3 %>%
  mutate(Pitcher = fct_reorder(Pitcher, RelSpeed)) %>%
  ggplot(aes(x = RelSpeed, y = Pitcher, fill = Pitcher)) +
  geom_density_ridges(show.legend = FALSE) +
  theme_ridges() + 
  ggtitle("Fastball Velocity Distribution") +theme_classic() + geom_text(aes(x=89.5, label="90 mph", y="Owen, Noah"), colour="red", angle=90, vjust = 0.5, text=element_text(size=20)) + 
  geom_vline(xintercept = 90, 
             color = "red", size=.5)









