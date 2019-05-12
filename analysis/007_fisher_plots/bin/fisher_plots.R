################################################
## Plot Fisher results
################################################
library("tidyverse")
library("cowplot")
################################################
##Borough level
################################################
borough <- read_csv(
  "../data/Borough.csv", 
  col_types = cols(Mean = col_number(), 
  SE = col_number())
)
names(borough)[1] <- "Borough"
borough$Borough <- factor(
  borough$Borough,
  levels = borough$Borough
)
head(borough)

pb <- ggplot(
  data = borough,
  aes(
    x = Borough,
    y = Mean,
    fill = Group
  )
) +
  geom_bar(stat="identity") + 
  geom_text(
    data = borough,
    aes(
      x = Borough,
      y = Mean + 1,
      label = Group
    ),
    size = 3
  )+
  geom_errorbar(
    data = borough,
    aes(
      x = Borough,
      ymin = Mean,
      ymax = Mean+SE
    )
  )+
  ylab("Mortality rate")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, h = 1),
    panel.grid = element_line("transparent")
  )+
  scale_fill_discrete(name = "Fisher LSD")+
  guides(fill = guide_legend(nrow = 2))
pb

BoroughType <- read_csv(
  "../data/BoroughType.csv", 
  col_types = cols(
    Mean = col_number(), 
    SE = col_number()
  )
)
names(BoroughType)[1] <- "Type"

BoroughType$Type <- factor(
  BoroughType$Type,
  levels = BoroughType$Type
)

pt <- ggplot(
  data = BoroughType,
  aes(
    x = Type,
    y = Mean,
    fill = Group
  )
) +
  geom_bar(stat="identity") + 
  geom_text(
    data = BoroughType,
    aes(
      x = Type,
      y = Mean + 3,
      label = Group
    ),
    size = 3
  )+
  geom_errorbar(
    data = BoroughType,
    aes(
      x = Type,
      ymin = Mean,
      ymax = Mean+SE
    )
  )+
  ylab("Mortality rate [x1000]")+
  xlab("Age-specific mortality")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, h = 1),
    panel.grid = element_line("transparent")
  )+
  scale_fill_discrete(name = "Fisher LSD")+
  guides(fill = guide_legend(nrow = 1))
pt


BoroughTypeReal <- read_csv(
  "../data/BoroughTypeReal.csv", 
  col_types = cols(
    Mean = col_number(),
    SE = col_number()
  )
)
names(BoroughTypeReal)[1:2] <- c("Borough", "Type")
BoroughTypeReal$BoroughType <- paste(BoroughTypeReal$Borough, BoroughTypeReal$Type, sep = "-")
BoroughTypeReal$BoroughType <- factor(
  BoroughTypeReal$BoroughType,
  levels  = BoroughTypeReal$BoroughType
)
BoroughTypeReal$Group <- factor(
  BoroughTypeReal$Group,
  levels = unique(BoroughTypeReal$Group)
)
as.character(levels(BoroughTypeReal$Group))
levels(BoroughTypeReal$Group)[56:58] <- "m"
levels(BoroughTypeReal$Group)[46:47] <- "h"
levels(BoroughTypeReal$Group)[35:36] <- "a"

pbt <- ggplot(
  data = BoroughTypeReal,
  aes(
    x = BoroughType,
    y = Mean,
    fill = Group
  )
) +
  geom_bar(stat="identity") + 
  geom_text(
    data = BoroughTypeReal,
    aes(
      x = BoroughType,
      y = Mean + 5,
      label = Group
    ),
    size = 2
  )+
  geom_errorbar(
    data = BoroughTypeReal,
    aes(
      x = BoroughType,
      ymin = Mean,
      ymax = Mean+SE
    )
  )+
  ylab("Mortality rate [x1000]")+
  xlab("Age-specific mortality")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 80, h = 1),
    panel.grid = element_line("transparent")
  )+
  scale_fill_discrete(name = "Fisher LSD")+
  guides(fill = guide_legend(nrow = 3))
pbt

pfull <- plot_grid(
  plotlist = list(
    plot_grid(
      plotlist = list(
        pt, 
        pb
      ),
      nrow = 1,
      labels = c("A", "B")
    ),
    pbt
  ),
  ncol = 1,
  rel_heights = c(0.7, 1.3),
  labels = c("", "C")
)
pfull

ggsave(
  pfull,
  file = "../results/boroughpanel.pdf",
  width = 15,
  height = 10,
  device = cairo_pdf
)
################################################
##Neighbourhood level
################################################
nei_type <-read_csv("../data/cuauhtemoc_type.csv")
nei_type$Type <- factor(
  nei_type$Type,
  levels = nei_type$Type
)

pt <- ggplot(
  data = nei_type,
  aes(
    x = Type,
    y = Mean,
    fill = Group
  )
) +
  geom_bar(stat="identity") + 
  geom_text(
    data = nei_type,
    aes(
      x = Type,
      y = Mean + 0.5,
      label = Group
    ),
    size = 3
  )+
  geom_errorbar(
    data = nei_type,
    aes(
      x = Type,
      ymin = Mean,
      ymax = Mean+SE
    )
  )+
  ylab("Mortality rate [x1000]")+
  xlab("Age-specific mortality")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, h = 1),
    panel.grid = element_line("transparent")
  )+
  scale_fill_discrete(name = "Fisher LSD")+
  guides(fill = guide_legend(nrow = 2))
pt

neig <- read.csv(file = "../data/neig_borough.csv")
neig$Neighbourhood <- factor(
  as.character(neig$Neighbourhood),
  levels = as.character(neig$Neighbourhood)
)
pn <- ggplot(
  data = neig,
  aes(
    x = Neighbourhood,
    y = Mean,
    fill = Group
  )
) +
  geom_bar(stat="identity") + 
  geom_text(
    data = neig,
    aes(
      x = Neighbourhood,
      y = 7.8,
      label = Group
    ),
    size = 3,
    angle = 90,
    hjust = 0
  )+
  geom_errorbar(
    data = neig,
    aes(
      x = Neighbourhood,
      ymin = Mean,
      ymax = Mean+SE
    )
  )+
  ylab("Mortality rate [x1000]")+
  xlab("Age-specific mortality")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, h = 1),
    panel.grid = element_line("transparent")
  )+
  scale_fill_discrete(name = "Fisher LSD")+
  guides(fill = guide_legend(nrow = 1))+
  ylim(0, 8.5)
pn

#p_cuauhtemoc
mascara <- neig$Group
names(mascara) <- toupper(as.character(neig$Neighbourhood))

aux <- cbind(
  unique(as.character(cuauhtemoc$SETT_NAME)[order(as.character(cuauhtemoc$SETT_NAME))]),
  names(mascara)[order(names(mascara))]
)
table(aux[,1] != aux[,2])
aux[aux[,1] != aux[,2]]

names(mascara)[20] <- "ALGARIN"
names(mascara)[25] <- "SANTA MARIA INSURGENTES"
names(mascara)[28] <- "AMPL ASTURIAS"
names(mascara)[3]  <- "JUAREZ"  
names(mascara)[33] <- "VALLE GOMEZ"
names(mascara)[12] <- "TRANSITO"
names(mascara)[10] <- "CUAUHTEMOC"
names(mascara)[11] <- "SANTA MARIA LA RIBERA"
names(mascara)[24] <- "SAN SIMON TOLNAHUAC"
names(mascara)[26] <- "HIPODROMO"
names(mascara)[15] <- "UNIDAD HAB NONOALCO TLATELOLCO"
names(mascara)[34] <- "HIPODROMO DE LA CONDESA"
names(mascara)[23] <- "EX HIPODROMO DE PERALVILLO"

cuauhtemoc$fill <- mascara[as.character(cuauhtemoc$SETT_NAME)]

p_cuauhtemoc_fill <- ggplot() +
  geom_polygon(
    data = cuauhtemoc,
    aes(
      x = long,
      y = lat,
      group = id,
      fill = fill
    )
  ) +
  geom_path(
    data = cuauhtemoc,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    color = "black"
  )+
  coord_equal() +
  xlab("Longitude")+
  ylab("Latitud")+
  theme_bw()+
  annotation_scale()+
  coord_sf(crs = 4326)+
  theme(
    panel.grid = element_line(colour = "transparent")
  )+
  scale_fill_discrete(name="Fisher LSD")
p_cuauhtemoc_fill

pfull <- plot_grid(
    plotlist = list(
      plot_grid(
        plotlist = list(
          p_cuauhtemoc,
          p_cuauhtemoc_fill
        ),
        ncol = 2,
        labels = c("A", "B")
      ),
      plot_grid(
        plotlist = list(
          pt, pn
        ),
        ncol = 2,
        rel_widths = c(0.3, 0.7),
        labels = c("C", "D")
    )
  ),
  ncol = 1
)
pfull

ggsave(
  pfull,
  file = "../results/cuauhtemoc.pdf",
  width = 15,
  height = 10,
  device = cairo_pdf
)
