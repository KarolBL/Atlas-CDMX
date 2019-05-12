library("lme4")
library("nlme")
m_neigh <- read.csv("../results/ Neighbourhood.csv")
m_neigh$colonia <- paste(m_neigh$SETT_NAME, m_neigh$MUN_NAME, sep = "_")
m_neigh$days2 <- m_neigh$days^2
m_neigh$days3 <- m_neigh$days^3

model <- lmer(
  value ~ Type + days + days2 | SETT_NAME,
  data = m_neigh
)

model <- gls(
    value~1+Type+days+days2+days3+colonia,
    correlation=corAR1(form=~as.integer(as.character(days))|colonia),
    method = "REML", 
    na.action = na.omit,
    data = m_neigh
)

model <- lme(
    value~1+Type+days+days2+days3+colonia,
    random = list(
        MUN_NAME=pdIdent(~1),
        SETT_NAME=pdIdent(~1)
        #colonia = pdIdent(~1)
    ),
    method = "REML",
    control = lmeControl(
        niterEM = 150,
        msMaxIter = 200,
        returnObject = TRUE
    ),
    na.action = na.omit,
    data = m_neigh
)

par(mfrow=c(2,2))
plot(model)

