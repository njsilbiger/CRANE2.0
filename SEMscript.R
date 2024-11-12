#### SEM calculating direct and indirect effects of Nutrients on NEC ###


library(tidyverse)
library(here)
library(piecewiseSEM)
library(semEff)

### read in the data #####

AllData<-read_csv(here("QAQC_Data/AllQCData.csv"))

CoralOnly <- AllData %>%
  filter(Substrate == "Coral") %>%
  mutate(NutLevel = factor(NutLevel, levels = c("Ambient","Med","High")))


CoralOnly %>%
  ggplot(aes(x = TankN, y = NCP.AFDW))+
  geom_point(aes(color = DayNight))+
  geom_smooth(method = "lm", 
              formula = "y~poly(x,2)")

CoralOnly %>%
  ggplot(aes(y = TankpH, x = NCP.AFDW))+
  geom_point(aes(color = DayNight))+
  geom_smooth(method = "lm")

CoralOnly %>%
  ggplot(aes(x = TankpH, y = NEC.AFDW))+
  geom_point(aes(color = DayNight))+
  geom_smooth(method = "lm")

mod_coral<-psem(
  lm(NEC.AFDW ~ TankN +TankpH, data = CoralOnly),
  lm(NCP.AFDW ~ TankN, data = CoralOnly),
  lm(TankpH ~ NCP.AFDW, data = CoralOnly),
  data = CoralOnly
)


piecewiseSEM:::plot.psem(
  mod_coral,
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

mod_coral.boot <- bootEff(mod_coral, R = 300, seed = 13, parallel = "no")

coral_eff<-semEff(mod_coral.boot)
summary(coral_eff, response = "NEC.AFDW")

## Direct effect of N on NEC in a coral was 0.035, indirect effect was 0.210


### Header
mod_coral_Header<-psem(
  lm(NEC.AFDW ~ HeaderN +TankpH, data = CoralOnly),
  lm(NCP.AFDW ~ HeaderN, data = CoralOnly),
  lm(TankpH ~ NCP.AFDW, data = CoralOnly),
  data = CoralOnly
)

piecewiseSEM:::plot.psem(
  mod_coral_Header,
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

mod_coral.boot_Header <- bootEff(mod_coral_Header, R = 300, seed = 13, parallel = "no")

coral_eff_Header<-semEff(mod_coral.boot_Header )
summary(coral_eff_Header, response = "NEC.AFDW")

Effects<-tibble(Direct = c(0.035, -0.431), Indirect = c(0.210,0.040), type = c("Tank", "Header"))

Effects %>% 
  pivot_longer(cols = c(Direct, Indirect))%>%
  ggplot(aes(x = name, y = value, fill = type))+
    geom_col(position = "dodge2")+
  scale_fill_manual(values = c("#A0524E","#204652"))+
  labs(y = "Standardized effect of nitrate on net calcification",
       x = "",
       fill = "")+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "none")
ggsave(here("plots","standardEffect.pdf"), width = 6, height = 6)


# plot N uptake or loss

AllData %>%
  filter(Substrate == "Coral")%>%
  mutate(deltaN =  TankN-HeaderN) %>%
  select(NutLevel, `Treatment Tank \n(Community modified)`=TankN, `Header Tank \n(Nutrient Addition)` = HeaderN, Substrate) %>%
  pivot_longer(cols = c(`Treatment Tank \n(Community modified)`, `Header Tank \n(Nutrient Addition)`))%>% 
  group_by(NutLevel, name)%>%
  summarise(mean_N = mean(value, na.rm = TRUE),
            se_N = sd(value,na.rm = TRUE)/sqrt(n())) %>%
  mutate(NutLevel = ifelse(NutLevel == "Med","Medium",NutLevel))%>%
  mutate(NutLevel = factor(NutLevel, levels = c("Ambient","Medium" ,"High")))%>%
  ggplot(aes(x = NutLevel, y = mean_N, color = name ))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = mean_N - se_N, ymax = mean_N+se_N), width = 0.1)+
  geom_hline(yintercept = 0, lty = 2)+
  scale_color_manual(values = c("#A0524E","#204652"))+
  labs(x = "Treatment condition",
       y = expression(paste("Average Nitrate (", mu,"mol L"^-1,")")),
       color = "")+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "top")
ggsave(here("plots","NutrientHeaderTank.pdf"), width = 6, height = 6)
