# To inspect Data from Nguom rockshelter
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
str(rtf)
View(rtf)
ggplot(rtf, aes(x=Material, y=Context)) +
  geom_bar(stat="identity",colour = "black") + 
  facet_grid(~ Context) +
  theme_bw() +
  theme(legend.text=element_text(size=12)) +
  theme(text = element_text(size=12),
        axis.text.y = element_text(size =18, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 8, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("") +
  coord_flip() +
  ggtitle("Công cụ mảnh tước tu chỉnh ô B mái đá Ngườm 2017")


# Tom tat cac chi so
summary(rtf)

## HE SO TUONG QUAN-------------------------------------------------------------
# Mass/Max_dimension - rat OK
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
str(rtf)
attach(rtf)
View(rtf)
rtf$Mass <- as.numeric(rtf$Mass)
rtf$Length <- as.numeric(rtf$Length)
ggscatter(rtf , x = "Mass", y = "State",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence intehttp://127.0.0.1:22589/graphics/plot_zoom_png?width=2034&height=1046rval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 2, label.y = 200, size = 10) +
  geom_point(alpha = 0.05, size = 5, fill = "black" , color = "black") +
  facet_wrap( ~ Unit) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều dài (mm)") +
  ylab("Trọng lượng (g)")


# He so tuong quan chieu Dai va Rong - rat OK
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
str(rtf)
attach(rtf)
rtf$Width <- as.numeric(rtf$Width)
ggscatter(rtf , x = "Width", y = "Length",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 2, label.y = 200, size = 10) +
  geom_point(alpha = 0.05, size = 5, fill = "black" , color = "black") +
  facet_wrap( ~ Unit) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều rộng (mm)") +
  ylab("Chiều dài (mm)")


# He so tuong quan RONG DIEN GHE va DAY DIEN GHE - rat OK
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
str(rtf)
attach(rtf)
rtf$Platorm_width <- as.numeric(rtf$Platform_width)
ggscatter(rtf , x = "Platform_width", y = "Platform_thickness",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 2, label.y = 50, size = 10) +
  geom_point(alpha = 0.05, size = 5, fill = "black" , color = "black") +
  facet_wrap( ~ Units) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều rộng (mm)") +
  ylab("Chiều dài (mm)")


# He so tuong quan RONG DIEN GHE va DAY DIEN GHE - rat OK
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
str(rtf)
attach(rtf)
rtf$Platorm_width <- as.numeric(rtf$Platform_width)
ggscatter(rtf , x = "Platform_width", y = "Platform_thickness",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 2, label.y = 50, size = 10) +
  geom_point(alpha = 0.05, size = 5, fill = "black" , color = "black") +
  facet_wrap( ~ Units) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều rộng (mm)") +
  ylab("Chiều dài (mm)")


# He so tuong quan RONG DIEN GHE va CHIEU RONG - rat OK
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
str(rtf)
attach(rtf)
rtf$Platorm_width <- as.numeric(rtf$Platform_width)
ggscatter(rtf , x = "Platform_width", y = "Width",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 2, label.y = 80, size = 10) +
  geom_point(alpha = 0.05, size = 5, fill = "black" , color = "black") +
  facet_wrap( ~ Units) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều rộng (mm)") +
  ylab("Chiều dài (mm)")


# He so tuong quan DAY DIEN GHE va DAY
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
str(rtf)
attach(rtf)
rtf$Thickness <- as.numeric(rtf$Thickness)
ggscatter(rtf , x = "Platform_thickness", y = "Thickness",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 2, label.y = 80, size = 10) +
  geom_point(alpha = 0.05, size = 5, fill = "black" , color = "black") +
  facet_wrap( ~ Units) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều rộng (mm)") +
  ylab("Chiều dài (mm)")


# He so tuong quan GOC GHE NGOAI va GOC GHE TRONG - rat OK
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
str(rtf)
attach(rtf)
rtf$Internal_platform_angle <- as.numeric(rtf$Internal_platform_angle)
rtf$External_platform_angle <- as.numeric(rtf$External_platform_angle)
ggscatter(rtf , x = "Internal_platform_angle", y = "External_platform_angle",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 2, label.y = 200, size = 10) +
  geom_point(alpha = 0.05, size = 5, fill = "black" , color = "black") +
  facet_wrap( ~ Units) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 20) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Hệ số tương quan góc ghè ngoài và trong") +
  xlab("Góc ghè trong (o)") +
  ylab("Góc ghè ngoài (o)")



# LOAI HINH DIEN GHE------------------------------------------------
# Types of Striking platforms ngay 3- 7 - 2024
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(rtf)
rtf %>% 
  group_by(`Platform_types`) %>%
  tally()
rtf_Platform_types_tally <- rtf %>% 
  group_by(`Platform_types`) %>% 
  tally() %>% 
  filter(`Platform_types` != 'NA') %>%  # REMOVE NAs 
  arrange(desc(n))
ggplot(rtf_Platform_types_tally,
       aes(x = reorder(`Platform_types`, n), y = n)) +
  geom_bar(stat="identity", size = 1, 
           fill = "white", color = "black", 
           width = 0.8) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  xlab("") +
  ylab("Tần suất") +
  coord_flip()


# Distribution of Platform Width/Types of Striking Platforms - ca hai tang van hoa I va II
# Chua on
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
str(lop2)
lop2
rtf$Platform_width <- as.numeric(rtf$Platform_width)
rtf$Mass <- as.numeric(rtf$Mass)
ggplot(lop2 , aes(x = reorder(Mass, Platform_width),
                  y = Platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ Material) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 15, angle= 45, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width(mm)") +
  ggtitle("Tầng văn hóa I và II") +
  labs(fill = "Flakes") +
  coord_flip()


# Anova of States with Thickness - Unit 1
# OK
str(lop2)
attach(lop2)
lop2$Platform_width <- as.numeric(lop2$Platform_width)
av = aov(Platform_width ~ Platform_types)
summary(av)
av
TukeyHSD(av)
l2 = TukeyHSD(av)
plot(l2)


# Loai hinh dien ghe va chieu day tang van hoa som  - 2
# Chua Ok
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(ggpubr)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
str(lop2)
lop2
ggplot(lop2 , aes(x = reorder(Platform_thickness, Platform_types),
                  y = Platform_types)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ Context) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 15, angle= 45, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width(mm)") +
  ggtitle("Tầng văn hóa I và II") +
  labs(fill = "Flakes") +
  coord_flip()
# Anova of States with Thickness - Unit 1
str(lop2)
attach(lop2)
lop2$Platform_thickness <- as.numeric(lop1$Platform_thickness)
av = aov(Platform_thickness ~ Platform_types)
summary(av)
av
TukeyHSD(av)
lop21 = TukeyHSD(av)
plot(lop21)



# Distribution of Platform Thickness/Types of Striking Platforms
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(rtf)
str(rtf)
rtf
ggplot(rtf , aes(x = reorder(Platform_types, Platform_thickness),
                  y = Platform_thickness)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.2, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ Units) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thikcness (mm)") +
  ylim(0,50) +
  ggtitle("Unit 1 and Unit 2") +
  labs(fill = "Flakes") +
  coord_flip()
  
# Anova of States with Thickness - Unit 1
str(rtf)
attach(rtf)
rtf$Platform_thickness <- as.numeric(rtf$Platform_thickness)
av = aov(Platform_thickness ~ Platform_types)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



#### Dimensions-----------------------------------------------------
#### Unit_2
## MASS - RAW MATERIALS - ok
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$mass <- as.numeric(flake_u2$mass)
ggplot(flake_u2, aes(x = reorder(state_of_preservation, mass),
                     y = mass, fill = total_material)) + 
  geom_boxplot(width = 0.8, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.09) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 16, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 16, angle = 0, hjust=.5)) +
  scale_y_log10() +
  ylab("Mass (g)") + 
  xlab("") +
  ggtitle("Unit 2") +
  labs(fill = "") +
  coord_flip()



# Ngay 22-11-2019
# DIMENSIONS ---------------------------------------------------------
# Unit 2 - Mass
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
library(tidyr)
library(microbenchmark)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u1)
flake_u2$mass <- as.numeric(flake_u2$mass)
# Mass and Raw Material ------------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
flake_u2$mass <- as.numeric(flake_u2$mass)
flake_u2 %>% na.omit()
# inspect data
str(flake_u2)
names(flake_u21) <- make.names(names(flake_u2))
# Compute ANOVA
mass_total_material_aov <- aov(mass ~ total_material, data = flake_u2)
mass_total_material_aov_tidy <- tidy(mass_total_material_aov)
# Compute post-hoc test
mass_total_material_aov_posthoc <- tidy(TukeyHSD(mass_total_material_aov))
# Plot
mass_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Mass (g)") +
  ylab("Estimate") +
  ggtitle("Unit 2") +
  labs(colour = "Significant difference") 
### P_value of Mass
mass_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(mass ~ total_material, data = flake_u2)
summary(results)
## Anova to test there was a different or not between three main types of raw materials and mass in Unit 1
attach(flake_u2)
str(flake_u2)
av = aov(mass ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# Boxploting for Mass / Raw Materials
flake_u2$mass <- as.numeric(flake_u2$mass)
ggplot(flake_u2, aes(x = reorder(total_material, mass),
                     y = max_dimension)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 1) +
  geom_jitter(alpha = 0.12) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Mass (g)") + 
  xlab("") +
  ggtitle("Unit 2") +
  scale_y_log10()
## Chi.square test for Mass/State of preservation
attach(flake_u2)
View(flake_u2)
tabl1 = table(flake_u2$mass, flake_u2$total_material)
tabl1 
cor(tabl1)


#===============================================================================
# Anova for differences of Platform Width/Types of Platforms in Unit 1
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(rtf)
str(rtf)
ggplot(rtf , aes(x = reorder(Platform_types, Platform_width),
                    y = Platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ Units) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width (mm)") +
  ylim(0,30) +
  ggtitle("Unit 1") +
  labs(fill = "Flakes") +
  # Anova of States with Thickness - Unit 1
  str(flake1)
attach(flake1)
flake1$platform_thickness <- as.numeric(flake1$platform_thickness)
av = aov(platform_width ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# Anova for differences of Platform Thickness/Types of Platforms in Unit 1
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake1 <- read_excel("flake.xlsx", sheet = 'flake1')
View(flake1)
str(flake1)
ggplot(flake1 , aes(x = reorder(type_of_striking_platforms, platform_thickness),
                    y = platform_thickness)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thikcness (mm)") +
  ylim(0,30) +
  ggtitle("Unit 1") +
  labs(fill = "Flakes") +
  # Anova of States with Thickness - Unit 1
  str(flake1)
attach(flake1)
flake1$platform_thickness <- as.numeric(flake1$platform_thickness)
av = aov(platform_thickness ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


#===============================================================================
# Anova for differences of Platform Width/Types of Platforms in Unit 2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake2 <- read_excel("flake.xlsx", sheet = 'flake2')
str(flake2)
ggplot(flake2 , aes(x = reorder(type_of_striking_platforms, platform_width),
                    y = platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width (mm)") +
  ylim(0,30) +
  ggtitle("Unit 2") +
  labs(fill = "Flakes") +
  # Anova of States with Thickness - Unit 2
  str(flake2)
attach(flake2)
flake1$platform_width <- as.numeric(flake1$platform_width)
av = aov(platform_width ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)

# Anova for differences of Platform Width/Types of Platforms in Unit 2
ggplot(flake2 , aes(x = reorder(type_of_striking_platforms, platform_thickness),
                    y = platform_thickness)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thickness (mm)") +
  ylim(0,25) +
  ggtitle("Unit 2") +
  labs(fill = "Flakes") +
  # Anova of States with Thickness - Unit 2
  str(flake2)
attach(flake2)
flake1$platform_thickness <- as.numeric(flake1$platform_thickness)
av = aov(platform_thickness ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



# Plotting Geom_Point for Platform Width/Platform Thikcness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_u12 <- read_excel("flake.xlsx", sheet = 'flake1+2')
attach(flake_u12)
str(flake_u12)
ggplot(flake_u12, aes(x = platform_thickness, y = platform_width,
                      fill = type_of_striking_platforms)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(size = 1, method = "lm", formula = y ~ x+I(x^2)) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  theme(legend.position="right") +
  xlim(0, 30) +
  ylab("Platform Width (mm)") +
  xlab("Platform Thickness (mm)") +
  labs(fill = "Platforms")
## Chi.square test for Mass/State of preservation
attach(flake_u12)
cor.test(platform_thickness, platform_width, method = "pearson")




#====================================================================
# Comparing Platform Width between Flakes/Split River Cobbles Unit 1+2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
attach(flake_src)
str(flake_src)
ggplot(flake_src , aes(x = reorder(type_of_striking_platforms, platform_width),
                       y = platform_width, fill = flake_type)) + 
  geom_boxplot(outlier.shape = NA, width = 0.6, faltten = 0.7, size = 0.6) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width (mm)") +
  ylim(0,110) +
  ggtitle("") +
  labs(fill = "") 
# Anova of Platform Width/Flake/Split River cobbles - Unit 1+2
str(flake_src)
attach(flake_src)
flake_src$platform_width <- as.numeric(flake_src$platform_width)
av = aov(platform_width ~ flake_type)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


# Comparing Platform Thickness between Flakes/Split River Cobbles Unit 1+2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
View(flake_src)
str(flake_src)
ggplot(flake_src , aes(x = reorder(type_of_striking_platforms, platform_thickness),
                       y = platform_thickness, fill = flake_type)) + 
  geom_boxplot(outlier.shape = NA, width = 0.6, faltten = 0.7, size = 0.6) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thikcness (mm)") +
  ylim(0,35) +
  ggtitle("") +
  labs(fill = "") 
# Anova of States with Thickness - Unit 1
str(flake_src)
attach(flake_src)
flake_src$platform_thickness <- as.numeric(flake_src$platform_thickness)
av = aov(platform_thickness ~ flake_type)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



# Platform Angles ==========================================================
## Distribution of "Internal Platform Angle"
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
View(flake_src)
str(flake_src)
ggplot(flake_src, aes(x = reorder(type_of_striking_platforms, internal_platform_angle),
                      y = internal_platform_angle, fill = unit)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("Internal Platform Angle") +
  xlab("") +
  ylab("Degree") +
  ylim(0, 150) +
  labs(fill = "") 



# Correlations between Internal/External Platform Angles
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
View(flake_src)
str(flake_src)
plot(internal_platform_angle, external_platform_angle, las = 2, main = "Scatter plot")
cor.test(internal_platform_angle, external_platform_angle, method = "pearson")
pairs(flake_u2[, 15:17])
cor(flake_u2[, 15:17], method = "spearman")


# Correlation test for Internal Platform Angles
View(flake_src)
attach(flake_src)
tabl1 = table(flake_u12$internal_platform_angle, flake_u12$internal_platform_angle)
tabl1 
cor.test(internal_platform_angle, external_platform_angle, method = "pearson")
#---------------------------------------------------------------------------

# Max and Raw Material ------------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
flake_u2 %>% na.omit()
# inspect data
str(flake_u2)
names(flake_u2) <- make.names(names(flake_u2))
# Compute ANOVA
max_dimension_total_material_aov <- aov(max_dimension ~ total_material, data = flake_u2)
max_dimension_total_material_aov_tidy <- tidy(max_dimension_total_material_aov)
# Compute post-hoc test
max_dimension_total_material_aov_posthoc <- tidy(TukeyHSD(max_dimension_total_material_aov))
# Plot
max_dimension_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Max Dimension (mm)") +
  ylab("Estimate") +
  ggtitle("Unit 2") +
  labs(colour = "Significant difference") 
### P_value of Mass
max_dimension_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(max_dimension ~ total_material, data = flake_u2)
summary(results)
## Chi.square test for Mass/State of preservation
attach(flake_u2)
View(flake_u2)
tabl1 = table(flake_u2$max_dimension, flake_u2$total_material)
tabl1 
cor(tabl1)


# Ploting Boxplot Max Dimension/Raw Materials
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
ggplot(flake_u2, aes(x = reorder(total_material, max_dimension),
                     y = max_dimension)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Max Dimension (mm)") + 
  xlab("") +
  ggtitle("Unit 2") +
  scale_y_log10()
## Anova for Lenght/Raw Material types
attach(flake_u2)
str(flake_u2)
av = aov(max_dimension ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# Length and Raw Material ----------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$length <- as.numeric(flake_u2$length)
flake_u2 %>% na.omit()
# inspect data
str(flake_u2)
names(flake_u2) <- make.names(names(flake_u2))
# Compute ANOVA
length_total_material_aov <- aov(length ~ total_material, data = flake_u2)
length_total_material_aov_tidy <- tidy(length_total_material_aov)
# Compute post-hoc test
length_total_material_aov_posthoc <- tidy(TukeyHSD(length_total_material_aov))
# Plot
length_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Length (mm)") +
  ylab("Estimate") +
  ggtitle("Unit 2") +
  labs(colour = "Significant difference") 
### P_value of Mass
length_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(length ~ total_material, data = flake_u2)
summary(results)
## Anova for Lenght / Raw Material types
attach(flake_u2)
str(flake_u2)
av = aov(length ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)

## Boxplot of Length and Raw Materials
library(ggplot2)
attach(flake_u2)
str(flake_u2)
flake_u2$length <- as.numeric(flake_u2$length)
ggplot(flake_u2, aes(x = reorder(total_material, length),
                     y = length)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.1) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Legnth (mm)") + 
  xlab("") +
  ggtitle("Unit 2") +
  scale_y_log10()

# Chi-square Length/Raw Material
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$length <- as.numeric(flake_u2$length)
## Chi.square test for Mass/State of preservation
attach(flake_u2)
tabl1 = table(flake_u2$length, flake_u2$total_material)
tabl1 
cor(tabl1)



###------------------------
## To know all numbers of flakes from square B-EDEF
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
colnames(rtf)
rtf %>% 
  group_by(`Context`) %>%
  tally()
rtf_Context_tally <- rtf %>% 
  group_by(`Context`) %>% 
  tally() %>% 
  filter(`Context` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_Context_tally,
       aes(x = reorder(`Context`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 1, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Frequency") +
  coord_flip()


## Xem phan bo CONG CU MANH THEO TY LE
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
rtf <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
rtf
rtf %>% 
  group_by(`Types`) %>%
  tally()
rtf_Types_tally <- rtf %>% 
  group_by(`Types`) %>% 
  tally() %>% 
  filter(`Types` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_Types_tally,
       aes(x = reorder(`Types`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 1, color="black", fill="white", width = 0.3) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 15, colour = "black", angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, colour = "black", angle= 0, hjust=.5)) +
  xlab("") +
  ylab("Frequency") +
ggtitle(" Số lượng các công cụ mảnh tu chỉnh từ mảnh tước và mảnh tách ô B")


# Su phan bo theo TINH TRANG CONG CU MANH
rtf %>% 
  group_by(`State`) %>%
  tally()
rtf_State_tally <- rtf %>% 
  group_by(`State`) %>% 
  tally() %>% 
  filter(`State` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_State_tally,
       aes(x = reorder(`State`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 1, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size = 30,color="black")) +
  xlab("Tình trạng") +
  ylab("Tần suất") +
  coord_flip()



##----------------
# To know Major method of flaking of Retouched flakes at Nguom 2017
rtf %>% 
  group_by(`Method_of_flaking`) %>%
  tally()

rtf_Method_of_flaking_tally <- rtf %>% 
  group_by(`Method_of_flaking`) %>% 
  tally() %>% 
  filter(`Method_of_flaking` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_Method_of_flaking_tally,
       aes(x = reorder(`Method_of_flaking`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 0.9, color="black", fill="white", width = 0.4) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  ggtitle("Flaking methods") +
  xlab("") +
  ylab("Frequency") +
  scale_y_log10()


## De biet duoc su da dang DIEN GHE
rtf %>% 
  group_by(`Platform_types`) %>%
  tally()
rtf_Platform_types_tally <- rtf %>% 
  group_by(`Platform_types`) %>% 
  tally() %>% 
  filter(`Platform_types` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_Platform_types_tally,
       aes(x = reorder(`Platform_types`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 0.5, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle= 0, hjust=.5)) +
  xlab("") +
  ylab("Frequency") +
  coord_flip() +
  ggtitle("Types of striking platform in square B")


# Huong cua nhom cong cu manh tren MAT LUNG
rtf %>% 
  group_by(`Orientation_of_the_dorsal_flake_scars`) %>%
  tally()
rtf_Orientation_of_the_dorsal_flake_scars_tally <- rtf %>% 
  group_by(`Orientation_of_the_dorsal_flake_scars`) %>% 
  tally() %>% 
  filter(`Orientation_of_the_dorsal_flake_scars` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_Orientation_of_the_dorsal_flake_scars_tally,
       aes(x = reorder(`Orientation_of_the_dorsal_flake_scars`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 0.9, color="black", fill="white", width = 0.5) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=.5)) +
  ggtitle("Orientation of retocuhed scars on the dorsal side") +
  xlab("") +
  ylab("Frequency")


# Huong cua nhom cong cu manh tren MAT BUNG
rtf %>% 
  group_by(`Orientation_of_the_ventral_flake_scars`) %>%
  tally()
rtf_Orientation_of_the_ventral_flake_scars_tally <- rtf %>% 
  group_by(`Orientation_of_the_ventral_flake_scars`) %>% 
  tally() %>% 
  filter(`Orientation_of_the_ventral_flake_scars` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_Orientation_of_the_ventral_flake_scars_tally,
       aes(x = reorder(`Orientation_of_the_ventral_flake_scars`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 0.9, color="black", fill="white", width = 0.5) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=.5)) +
  ggtitle("Orientation of retocuhed scars on the ventral side") +
  xlab("") +
  ylab("Frequency") 



# Vi tri tu chinh tren MAT LUNG
rtf %>% 
  group_by(`Localisation_of_retouch_on_dorsal_sides`) %>%
  tally()
rtf_Localisation_of_retouch_on_dorsal_sides_tally <- rtf %>% 
  group_by(`Localisation_of_retouch_on_dorsal_sides`) %>% 
  tally() %>% 
  filter(`Localisation_of_retouch_on_dorsal_sides` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_Localisation_of_retouch_on_dorsal_sides_tally,
       aes(x = reorder(`Localisation_of_retouch_on_dorsal_sides`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 0.5, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=.5)) +
  ggtitle("Retouched localizations on dorsal side") +
  xlab("") +
  ylab("Frequency") +
  coord_flip()


# Vi tri tu chinh tren MAT BUNG
rtf %>% 
  group_by(`Localisation_of_retouch_on_ventral_sides`) %>%
  tally()
rtf_Localisation_of_retouch_on_ventral_sides_tally <- rtf %>% 
  group_by(`Localisation_of_retouch_on_ventral_sides`) %>% 
  tally() %>% 
  filter(`Localisation_of_retouch_on_ventral_sides` != 'NA') %>% 
  arrange(desc(n))
ggplot(rtf_Localisation_of_retouch_on_ventral_sides_tally,
       aes(x = reorder(`Localisation_of_retouch_on_ventral_sides`, n), 
           y = n, lable = n)) +
  geom_bar(stat="identity", size = 0.5, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=.5)) +
  ggtitle("Retouched localizations on ventral side") +
  xlab("") +
  ylab("Frequency") +
  coord_flip()



###------------ CÁC CHI SO VE QUY MO
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
rt2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
rt2
str(rt2)
summary(rt2)
median(Mass$rt2)
# To see mean of metric measurements of Length/Width/Thickness/Platformwidht-Thickness
summary(rt)
ggplot(rt, aes(x=Length, 
                y = Width, 
                fill = State_of_preservation)) +
  geom_boxplot(width = 0.3) +
  theme_bw(base_size = 30) +
  xlab("Length(mm)") +
  ylab("Width (mm)") +
  labs(fill = "States")


### RETOUCHED FLAKES------------------------------RETOUCHED FLAKES
### To summarise all technogological attributes of RETOUCHED FLAKES at Nguom_ Square B
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
rtf <- read_excel("Retouchedflakes.xlsx", sheet = 'Sheet 1')
rtf
colnames(rtf)
ggplot(rtf, aes(Length, Width, fill = State_of_preservation)) +
  geom_point(size = 4, alpha = 0.1) +
  facet_wrap(~ Flake_types) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=.5)) +
  xlab("Length (mm)") +
  ylab("Width (mm)") +
  labs(fill = "States") # Thay (Shift_) trong excel


# To compare mean of Plain flakes from ALL contexts
rtf <- read_excel("Retouchedflakes.xlsx", sheet = 'Sheet 1')
rtf %>%
  filter(is.na(Context) == FALSE) %>% 
  ggplot(aes(x = reorder(Context, 
                         desc(Context)),
             y = Mass)) +
  geom_boxplot(outlier.shape = NA, size = 0.9) +
  geom_sina(alpha = 0.1) +
  geom_jitter(position=position_jitter(width = 0, height = 9)) +
  theme_bw() +
  theme(text = element_text(size=20)) +
  xlab("Excavation contexts") +
  ylab("Mass (g)") +
  ylim(0,100)


## Tinh trang va Lop khai quat
library(ggplot2)
rtf
rtf$External_platform_angle <- as.numeric(rtf$External_platform_angle)
rtf$Internal_platform_angle <- as.numeric(rtf$Internal_platform_angle)
ggplot(rtf, aes(x=State_of_preservation, y=Context)) +
  geom_bar(stat="identity", position = "fill", size = 10) + 
  facet_grid(~Context) + 
  theme(text = element_text(size = 25)) +
  theme_bw() +
  ggtitle("C?c l???p khai qu???t t???i ? B, m?i d? Ngu???m 2017") +
  xlab("") +
  ylab("T???n su???t") +
  coord_flip() +
  labs(fill = "Lo???i di???n gh?")


## Tim hieu ty le vo cuoi va So luong vet am ban tren mat lung
library(ggplot2)
ggplot(pfNG, aes(x=reorder(Dorsal_cortex_percentage, Mass), y=Mass)) +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  xlab("") +
  ylab("Frequency") +
  ylim("0, 1000") +
  scale_y_log10()


# To see Flake initiation types corresponding to Flake termination type of PLAIN FLAKES
library(tidyverse) 
pfNG %>%
  filter(!is.na(Flake_termination_type)) %>% 
  filter(!is.na(Flake_inititation_type)) %>% 
  ggplot(aes(x=Flake_inititation_type, fill=Flake_termination_type)) + 
  geom_bar(width = 0.6) +
  theme_bw(base_size = 30) +
  labs(fill = "Flake termination") +
  ylab("Frequency") +
  xlab("Flake initiation") +
  ggtitle("")


#---------------------------------------------------------------- LUU Y
### Tinh trung goc ghe ngoai va goc ghe trong cua manh tuoc
rtf
View(rtf)
rtf$External_platform_angle <- as.numeric(rtf$External_platform_angle)
rtf$Internal_platform_angle <- as.numeric(rtf$Internal_platform_angle)
rtf[is.na(rtf)] <- 0
names(rtf)
summary(rtf)
attach(rtf)
mean(External_platform_angle)
sd(External_platform_angle)
mean(Internal_platform_angle)
sd(Internal_platform_angle)

# Mean of Internal/External platform angles
# Boxplotting for External and Internal platform angles
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
rtf <- read_excel("Retouchedflakes.xlsx", sheet = 'Sheet 1')
rtf <- read_excel("Plainflakes.xlsx", sheet = 'Sheet1')
rtf$Internal_platform_angle <- as.numeric(rtf$Internal_platform_angle)
rtf$External_platform_angle <- as.numeric(rtf$External_platform_angle)
View(rtf)
colnames(rtf)
cum1 = c(External_platform_angle, Internal_platform_angle)
View(cum1)
metric1 <- c(rep("External_platform_angle", 803), rep("Internal_platform_angle", 803)) 
dat = data.frame(metric1, cum1)
dat
ggplot(dat, aes(x =reorder(metric1, cum1, fill = metric1, position=position_dodge(2)),
                y = cum1)) +
  geom_boxplot(size =.6, width = 0.3, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 20, angle = 0, hjust= 0),
        axis.text.x = element_text(size = 20, angle= 0, hjust= 0)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  scale_y_log10() +
  ggtitle("The mean of platform angle of retouched flakes") +
  xlab("") +
  ylab("Degree") +
  ylim(0, 120) # the differences can be seen from the Internal/External platform angles

#
av = aov(Internal_platform_angle ~ External_platform_angle)
summary(av) # There was not significant difference
av
TukeyHSD(av)
tk_u2 = TukeyHSD(av)
plot(tk_u2)


### Tim hieu kich thuoc cua nhom manh tuoc nay: Dai, Rong, Day, Day, Max-dimension
# Dai va Rong
library(devtools)
library(tidyverse)
library(ggplot2)
library(ggpubr)
ggplot(rtf, aes(Length, Width)) +
  geom_point(alpha = 1/5, size = 5) +
  geom_smooth(method ="lm") +
  stat_compare_means(label = "p.format") +
  scale_y_log10()


####--------------------------------------------------------------------
### TINH SU KHAC BIET CUA HAI NHOM CONG CU MANH TU MANH TUOC VA MANH VO
## Chieu DAI VA KICH THUOC LON NHAT
ggscatter(rtf, x = "Length", y = "Max_dimension",
          add = "reg.line",                                 # Add regression line
          color = "Flake_types", palette = "Flake_types",   # Color by groups "cyl"
          shape = "Flake_types",                            # Change point shape by groups "cyl"
          fullrange = TRUE,                                 # Extending the regression line
          rug = TRUE                                        # Add marginal rug
)+
  stat_cor(aes(color = Flake_types), label.x = 3)  +
  geom_point(alpha = 1/10, size = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  xlab("Chi???u d?i (mm)") +
  ylab("K?ch thu???c l???n nh???t (mm)")

##  Chieu DAI va RONG
ggscatter(rtf, x = "Length", y = "Width",
          add = "reg.line",                                 # Add regression line
          color = "Flake_types", palette = "Flake_types",   # Color by groups "cyl"
          shape = "Flake_types",                            # Change point shape by groups "cyl"
          fullrange = TRUE,                                 # Extending the regression line
          rug = TRUE)+                                       # Add marginal rug
  stat_cor(aes(color = Flake_types), label.x = 3, size = 14)  +
  geom_point(alpha = 1/10, size = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 20) +
  xlab("Length (mm)") +
  ylab("Width (mm)")


## Chieu DAI VA DAY
ggscatter(rtf, x = "Length", y = "Width",
          add = "reg.line",                                 # Add regression line
          color = "Flake_types", palette = "Flake_types",   # Color by groups "cyl"
          shape = "Flake_types",                            # Change point shape by groups "cyl"
          fullrange = TRUE,                                 # Extending the regression line
          rug = TRUE                                        # Add marginal rug
)+
  stat_cor(aes(color = Flake_types), label.x = 3)  +
  geom_point(alpha = 1/10, size = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 20) +
  xlab("Chi???u d?i (mm)") +
  ylab("Chi???u d?y (mm)")


### Chieu DAI VA MASS
ggscatter(rtf, x = "Length", y = "Mass",
          add = "reg.line",                                 # Add regression line
          color = "Flake_types", palette = "Flake_types",   # Color by groups "cyl"
          shape = "Flake_types",                            # Change point shape by groups "cyl"
          fullrange = TRUE,                                 # Extending the regression line
          rug = TRUE                                        # Add marginal rug
)+
  stat_cor(aes(color = Flake_types), label.x = 3)  +
  geom_point(alpha = 1/10, size = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  xlab("Chi???u d?i (mm)") +
  ylab("Tr???ng lu???ng (gram)") +
  scale_y_log10()


## SO SANH TUONG QUAN CUA GOC GHE NGOAI VA TRONG VOI HINH DAN DUOI MANH TUOC
rtf
View(rtf)
rtf$External_platform_angle <- as.numeric(rtf$External_platform_angle)
rtf$Internal_platform_angle <- as.numeric(rtf$Internal_platform_angle)
rtf[is.na(rtf)] <- 0
ggscatter(rtf, x = "External_platform_angle", y = "Internal_platform_angle",
          add = "reg.line",                                                     # Add regression line
          color = "Flake_termination_type", palette = "Flake_termination_type",   # Color by groups "cyl"
          shape = "",                                      # Change point shape by groups "cyl"
          fullrange = TRUE,                                                     # Extending the regression line
          rug = TRUE                                                            # Add marginal rug
)+
  stat_cor(aes(color = Flake_types), label.x = 3)  +
  geom_point(alpha = 1/10, size = 3) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 20) +
  xlab("G?c gh? ngo?i(degree)") +
  ylab("G?c gh? trong (degree)") +
  scale_y_log10()



## SO SANH TUONG QUAN CUA GOC GHE NGOAI VA TRONG VOI HINH DANG DAU MANH TUOC
rtf
View(rtf)
rtf$External_platform_angle <- as.numeric(rtf$External_platform_angle)
rtf$Internal_platform_angle <- as.numeric(rtf$Internal_platform_angle)
rtf[is.na(rtf)] <- 0
ggscatter(rtf, x = "External_platform_angle", y = "Internal_platform_angle",
          add = "reg.line",                                                     # Add regression line
          color = "Flake_initiation_type", palette = "Flake_initiation_type",   # Color by groups "cyl"
          shape = "Flake_initiation_type",                                      # Change point shape by groups "cyl"
          fullrange = TRUE,                                                     # Extending the regression line
          rug = TRUE                                                            # Add marginal rug
)+
  stat_cor(aes(color = Flake_types), label.x = 3)  +
  geom_point(alpha = 1/10, size = 5) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 20) +
  ylab("G?c gh? trong (degree)") +
  xlab("G?c gh? ngo?i (degree)") +
  scale_y_log10()


###---------------------------------------------------------
## -Cai nay khong can dung
ggscatter(rtf, x = "Dorsal_cortex_percentage", y = "Localization_of_dosral_cortex",
          add = "reg.line",                                 # Add regression line
          color = "Orientation_of_the_dorsal_flake_scars", palette = "Orientation_of_the_dorsal_flake_scars",   # Color by groups "cyl"
          shape = "Orientation_of_the_dorsal_flake_scars",                            # Change point shape by groups "cyl"
          fullrange = TRUE,                                 # Extending the regression line
          rug = TRUE                                        # Add marginal rug
)+
  stat_cor(aes(color = Flake_types), label.x = 3)  +
  geom_point(alpha = 1/10, size = 2) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 10) +
  xlab("Chi???u d?i (mm)") +
  ylab("Chi???u r???ng (mm)")

## Tinh he so tuong quan CHIEU DAI-CHIEU TOI DA
ggscatter(rtf, x = "Length", y = "Max_dimension",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 1, label.y = 150) +
  geom_point(alpha = 1/10, size = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  xlab("Chi???u d?i (mm)") +
  ylab("K?ch thu???c l???n nh???t (mm)")

## Tinh he so tuong quan CHIEU DAI-CHIEU RONG
ggscatter(rtf, x = "Length", y = "Width",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 1, label.y = 75) +
  geom_point(alpha = 1/10, size = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  xlab("Legnth (mm)") +
  ylab("Width (mm)")


## Tinh he so tuong quan CHIEU DAI-CHIEU DAY
ggscatter(rtf, x = "Length", y = "Thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 1, label.y = 80) +
  geom_point(alpha = 1/10, size = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  xlab("Chi???u d?i (mm)") +
  ylab("Chi???u d?y (mm)")


## Tinh he so tuong quan RONG DIEN GHE VA DAY DIEN GHE
ggscatter(rtf, x = "Platform_width", y = "Platform_thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 1, label.y = 30) + # Luu y co the dich chuyen R/P trong bang thong ke
  geom_point(alpha = 1/10, size = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  xlab("Chi???u d?i (mm)") +
  ylab("Chi???u d?y (mm)")


# So sanh goc ghe ngoai va goc ghe trong CONG CU MANH - DAU MANH TUOC
ggscatter(rtf, x = "External_platform_angle", y = "Internal_platform_angle",
          color = "Flake_initiation_type", palette = "External_platform_angle",
          shape = "Flake_initiation_type",
          ellipse = TRUE, ellipse.type = "convex") +
  stat_cor(method = "pearson", label.x = 0, label.y = 1150, size = 10) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 20, angle = 0, hjust= 1),
        axis.text.x = element_text(size = 20, angle= 0, hjust=0.5)) +
  xlab("Platform width (mm)") +
  ylab("Platform thickness (mm)")


# So sanh goc ghe ngoai va goc ghe trong CONG CU MANH - DUOI MANH TUOC
ggscatter(rtf, x = "External_platform_angle", y = "Internal_platform_angle",
          color = "Flake_termination_type", palette = "External_platform_angle",
          shape = "Flake_termination_type",
          ellipse = TRUE, ellipse.type = "convex") +
  stat_cor(method = "pearson", label.x = 550, label.y = 1150) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 1),
        axis.text.x = element_text(size = 30, angle= 0, hjust=0.5)) +
  xlab("G?c gh? ngo?i(degree)") +
  ylab("G?c gh? trong (degree)") +
  scale_alpha_continuous()


# So sanh CHIEU RONG DIEN GHE VA CHIEU DAY DIEN GHE
ggscatter(rtf, x = "Platform_width", y = "Platform_thickness",
          color = "Flake_initiation_type", palette = "Platform_width",
          shape = "Flake_initiation_type",
          ellipse = TRUE, ellipse.type = "convex") +
  stat_cor(method = "pearson", label.x = 1, label.y = 30, size = 10) +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 1),
        axis.text.x = element_text(size = 30, angle= 0, hjust=0.5)) +
  xlab("Platform width (mm)") +
  ylab("Platform thikcness (mm)") +
  scale_alpha_continuous() +
  labs(fill = "Flake initiation")
# 

