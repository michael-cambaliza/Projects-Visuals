library(readxl)
install.packages("pitchRx")
library(pitchRx)
Ultimate <- read_xlsx("~/Documents/R/BASEBALL/ultimate2.xlsx")
Ultimate <- Ultimate[2:35585,]
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





table(d$Event)

d <- Ultimate %>%
  summarise(Batter,PlayResult,PlateLocSide,PlateLocHeight)
table(d$PlayResult)
d <- d %>% rename(Event=PlayResult,X=PlateLocSide,Y=PlateLocHeight)
d <- subset(d, Event =="Single"|Event =="Double"|Event =="Triple"|Event =="HomeRun"|Event == "Out"|Event == "Sacrifice"
            |Event == "Error"|Event == "FieldersChoice")

heat_plot <- function(player, d){
  # inputs
  # player - name of player
  # d - pitchRX data frame with variables Batter, Event, and X, Z (location of pitch)
  # will output a ggplot2 object
  # need to use print function to display the pot
  require(dplyr)
  require(ggplot2)
  require(mgcv)
  # define the strike zone
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  # only consider events that are official at-bats
  TT <- table(d$Event)
  AB <- names(TT)
  d_AB <- filter(d, Event %in% AB )
  # define the 1/0 response variable
  d_AB<- mutate(d_AB, Hit=ifelse(Event %in% c("Single", "Double", "Triple", "HomeRun"),1, 0))
  # implement the GAM fit (logistic link)
  pdata <- filter(d_AB, Batter==Batter)
  fit <- gam(Hit ~ s(X, Y), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  x <- seq(-1.5, 1.5, length.out=200)
  y <- seq(0.5, 5, length.out=200)
  data.predict <- data.frame(X = c(outer(x, y * 0 + 1)),
                             Y = c(outer(x * 0 + 1, y)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  

  # construct the plot
  
  ggplot(kZone, aes(x, y)) +
    geom_tile(data=data.predict, 
              aes(x=X, y=Y, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1, col="black") +
    coord_fixed() +
    ggtitle(paste(player)) + theme_bw()
}

x <- as.data.frame(subset(d, Batter == "Willow, Jason"))
heat_plot("Willow, Jason", x)

x <- as.data.frame(subset(d, Batter == "O'Connor, McClain"))
heat_plot("O'Connor, McClain", x)

x <- as.data.frame(subset(d, Batter == "Castanon, Marcos"))
heat_plot("Castanon, Marcos", x)

x <- as.data.frame(subset(d, Batter == "Bloom, Gianni"))
heat_plot("Bloom, Gianni", x)

x <- as.data.frame(subset(d, Batter == "Willow, Jason"))
heat_plot("Willow, Jason", x)

x <- as.data.frame(subset(d, Batter == "Marquez, Mason"))
heat_plot("Marquez, Mason", x)

x <- as.data.frame(subset(d, Batter == "Eng, Mason"))
heat_plot("Eng, Mason", x)

x <- as.data.frame(subset(d, Batter == "Kirtley, Christian"))
heat_plot("Kirtley, Christian", x)

x <- as.data.frame(subset(d, Batter == "Sprinkle, Jordan"))
heat_plot("Sprinkle, Jordan", x)

x <- as.data.frame(subset(d, Batter == "Cummings, Cole"))
heat_plot("Cummings, Cole", x)

x <- as.data.frame(subset(d, Batter == "Oakley, Nick"))
heat_plot("Oakley, Nick", x)

x <- as.data.frame(subset(d, Batter == "Stone, Damian"))
heat_plot("Stone, Damian", x)

x <- as.data.frame(subset(d, Batter == "Neil, Andrew"))
heat_plot("Neil, Andrew", x)

x <- as.data.frame(subset(d, Batter == "Newman Jr., John"))
heat_plot("Newman Jr., John", x)

x <- as.data.frame(subset(d, Batter == "Johnson, Kyle"))
heat_plot("Johnson, Kyle", x)

x <- as.data.frame(subset(d, Batter == "Ledford, Steele"))
heat_plot("Ledford, Steele", x)

x <- as.data.frame(subset(d, Batter == "Neil, Andrew"))
heat_plot("Neil, Andrew", x)

x <- as.data.frame(subset(d, Batter == "Mortensen, Broc"))
heat_plot("Mortensen, Broc", x)

x <- as.data.frame(subset(d, Batter == "Vogt, Nick"))
heat_plot("Vogt, Nick", x)

x <- as.data.frame(subset(d, Batter == "Mosby, Leo"))
heat_plot("Mosby, Leo", x)

x <- as.data.frame(subset(d, Batter == "Arbolida, Cary"))
heat_plot("Arbolida, Cary", x)

x <- as.data.frame(subset(d, Batter == "Barber, Jeffrey"))
heat_plot("Barber, Jeffrey", x)

x <- as.data.frame(subset(d, Batter == "Williams, Luke"))
heat_plot("Williams, Luke", x)

x <- as.data.frame(subset(d, Batter == "Marsh, Michael"))
heat_plot("Marsh, Michael", x)

x <- as.data.frame(subset(d, Batter == "Forshey, Theo"))
heat_plot("Forshey, Theo", x)
plot(x$X,x$Y)

x <- as.data.frame(subset(d, Batter == "Rodriguez, Zachary"))
heat_plot("Rodriguez, Zachary", x)

x <- as.data.frame(subset(d, Batter == "Jeon, Eugene"))
heat_plot("Jeon, Eugene", x)

x <- as.data.frame(subset(d, Batter == "Willits, Bryce"))
heat_plot("Willits, Bryce", x)
