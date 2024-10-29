# Tong so hien vat 02 lop van hoa II va I_ mai da Nguom 2014
# Hinh thau DAU manh tuoc - rat OK
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
attach(Tongso)
Tongso %>% 
  group_by(`Context`) %>%
  tally()
Tongso_Context_tally <- Tongso %>% 
  group_by(`Context`) %>% 
  tally() %>% 
  filter(`Context` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso_Context_tally,
       aes(`Context`, n), 
           y = n, fill = State) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 35, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 35, angle = 0, hjust = 0.5)) +
  ylab("Tần suất") +
  xlab("
       Lớp văn hóa I (L10-L4) và Lớp văn hóa II (L15-L11)")




#-------------------------QUY MO LOP VAN HOA 41.5-22.0.0KA----------------------
# For Mass va Hai lop van hoa 1 & 2
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
Tongso$Mass <- as.numeric(Tongso$Mass)
Tongso <- Tongso %>% drop_na(Mass)
Tongso <- Tongso %>% drop_na(Material)
ggplot(Tongso, aes(x=reorder(Context1, Mass),
                    y = Mass)) +
  geom_boxplot(outlier.shape = NA, width = 0.3, size = 0.8) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  theme_bw() +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 35, angle = 0, hjust = .5),
        axis.text.x = element_text(size = 35, angle = 0, hjust = .5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Trọng lượng (gram)") +
  scale_y_log10() +
  ggtitle("Lớp văn hóa II và I (41.5 - 22.5ka)")


# Anova of Lop van hoa and Mass
str(Tongso)
attach(Tongso)
av = aov(Mass ~ Context1)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# For Mass va Lop dao
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
Tongso$Mass <- as.numeric(Tongso$Mass)
Tongso <- Tongso %>% drop_na(Mass)
Tongso <- Tongso %>% drop_na(Material)
ggplot(Tongso, aes(Context, Mass),
       y = Mass) +
  geom_boxplot(size = 0.6, width = 0.8, fatten = 0.4) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 35, angle = 0, hjust = .5),
        axis.text.x = element_text(size = 35, angle = 0, hjust = .5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Trọng lượng (gram)") +
  ggtitle("Lớp văn hóa II và I (41.5 - 22.5ka)")  +
  scale_y_log10() +
  coord_flip()

# Anova of Lop dao and Mass
str(Tongso)
attach(Tongso)
av = aov(Mass ~ Context)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

#---------------------------------- DAI----------------------------------------
# Do voi Length
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$Length <- as.numeric(Tongso$Length)
Tongso <- Tongso %>% drop_na(Length)
Tongso <- Tongso %>% drop_na(Material)
ggplot(Tongso, aes(x=reorder(Context1, Length),
                    y = Length)) +
  geom_boxplot(size = 1, width = 0.3, fatten = 2) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 35, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Chiều dài (mm)") +
  scale_y_log10() +
  ggtitle("") +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)")

# Anova of States with Length- Lop van hoa II - voi State
str(Tongso)
attach(Tongso)
av = aov(Length ~ Context1)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# Do voi Length va Lop dao
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$Length <- as.numeric(Tongso$Length)
Tongso <- Tongso %>% drop_na(Length)
Tongso <- Tongso %>% drop_na(Material)
ggplot(Tongso, aes(Context, Length),
       y = Length) +
  geom_boxplot(size = 0.6, width = 0.8, fatten = 0.1) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.1),
        axis.text.x = element_text(size = 35, angle = 0, hjust=.1)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Chiều dài (mm)") +
  ggtitle("") +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)") +
  coord_flip()


# Anova of States with Length- Lop van hoa II - voi Material
str(Tongso)
attach(Tongso)
av = aov(Length ~ Context)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# ------------------------------------------RONG--------------------------------
# Doi voi Width - lOP VAN HOA
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$Thickness <- as.numeric(Tongso$Thickness)
Tongso$Width <- as.numeric(Tongso$Width)
Tongso <- Tongso %>% drop_na(State)
ggplot(Tongso, aes(x=reorder(Context1, Width),
                    y = Width)) +
  geom_boxplot(size = 1, width = 0.3, flatten = 0.4) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 35, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Chiều rộng (mm)") +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)")

# Anova of Lop van van and Width
str(Tongso)
attach(Tongso)
av = aov(Width ~ Context1)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# Doi voi Width - Lop dao L4-L15)
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
Tongso$Thickness <- as.numeric(Tongso$Thickness)
Tongso$Width <- as.numeric(Tongso$Width)
Tongso <- Tongso %>% drop_na(State)
ggplot(Tongso, aes(Context, Width),
                   y = Width) +
  geom_boxplot(size = 0.6, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Chiều rộng (mm)") +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)") +
  coord_flip()

# Anova of Lop van van and Width
str(Tongso)
attach(Tongso)
av = aov(Width ~ Context)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


#--------------------------------------- DAY------------------------------------
# Doi voi chieu day - Thickness
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
Tongso$Thickness <- as.numeric(Tongso$Thickness)
Tongso <- Tongso %>% drop_na(Thickness)
Tongso <- Tongso %>% drop_na(Context1)
ggplot(Tongso, aes(x=reorder(Context1, Thickness),
                    y = Thickness)) +
  geom_boxplot(size = 1, width = 0.3, fatten = 0.4) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Chiều dày (mm)") +
  ggtitle("") +
  scale_y_log10()

# Anova Day va Lop Van Hoa 1 va 2
str(Tongso)
attach(Tongso)
av = aov(Thickness ~ Context1)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# Anova Day va Lop Dao (L4-L15)
str(Tongso)
attach(Tongso)
av = aov(Thickness ~ Context)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

#------------------------------- HE SO TUONG QUAN ------------------------------
# RONG VA DAY DIEN GHE
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggpubr)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
Tongso$Platform_width <- as.numeric(Tongso$Platform_width )
Tongso <- data2 %>% drop_na(Platform_width)
Tongso$Platform_thickness <- as.numeric(Tongso$Platform_thickness )
Tongso <- Tongso %>% drop_na(Platform_thickness)
Tongso <- Tongso %>% drop_na(Initiation)
ggscatter(Tongso , x = "Platform_width", y = "Platform_thickness",
          color = "black", shape = 1, size = 4, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "", label.x = 30, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 0, label.y = 30, size = 10) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)") +
  xlab("Rộng diện ghè") +
  ylab("Dày diện ghè") +
  scale_x_log10()

# RONG VA CHIEU RONG
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggpubr)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
Tongso$Platform_width <- as.numeric(Tongso$Platform_width )
Tongso <- Tongso %>% drop_na(Platform_width)
Tongso$Width <- as.numeric(Tongso$Width )
Tongso <- Tongso %>% drop_na(Width)
Tongso <- Tongso %>% drop_na(Initiation)
ggscatter(Tongso , x = "Platform_width", y = "Width",
          color = "black", shape = 1, size = 4, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "", label.x = 30, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 0, label.y = 70, size = 6) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context) +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)") +
  xlab("Rộng diện ghè (mm)") +
  ylab("Chiều rộng (mm)") +
  scale_x_log10()


# DAY DIEN GHE VÀ DAY
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggpubr)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
Tongso$Platform_thickness <- as.numeric(Tongso$Platform_thickness )
Tongso <- Tongso %>% drop_na(Platform_thickness)
Tongso$Thickness <- as.numeric(Tongso$Thickness )
Tongso <- Tongso %>% drop_na(Width)
Tongso <- Tongso %>% drop_na(Initiation)
ggscatter(Tongso , x = "Thickness", y = "Platform_thickness",
          color = "black", shape = 1, size = 4, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "", label.x = 30, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 0, label.y = 25, size = 8) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)") +
  xlab("Chiều dày (mm)") +
  ylab("Dày diện ghè (mm)") +
  scale_x_log10()


#--------------------------------- DIEN GHE-------------------------------------
# For Platform_Width
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$Platform_width <- as.numeric(Tongso$Platform_width)
Tongso <- Tongso %>% drop_na(Context1)
Tongso <- Tongso %>% drop_na(Platform)
Tongso$Platform_width <- as.numeric(Tongso$Platform_width)
ggplot(Tongso, aes(x=reorder(Context1, Platform_width),
                    y = Platform_width)) +
  geom_boxplot(size = 1, width = 0.3, fatten = 0.4) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  facet_wrap( ~ Platform) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Rộng diện ghè (mm)") +
  ggtitle("")


# Anova of States with Rong Dien Ghe/Lop van hoa
str(Tongso)
attach(Tongso)
av = aov(Platform_width ~ Context)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# Anova of States with Rong Dien Ghe/Tinh trang manh tuoc
str(Tongso)
attach(Tongso)
av = aov(Platform_width ~ State)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


#-------------------------------- DAY DIEN GHE----------------------------------
# For Platform_Thickness
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
View(Tongso)
Tongso$Platform_thickness <- as.numeric(Tongso$Platform_thickness)
Tongso <- Tongso %>% drop_na(Platform)
ggplot(Tongso, aes(x=reorder(Context1, Platform_thickness),
                    y = Platform_thickness)) +
  geom_boxplot(size = 1, width = 0.3, fatten = 0.8) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  facet_wrap( ~ Platform) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust =.5),
        axis.text.x = element_text(size = 30, angle = 0, hjust =.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Dày diện ghè (mm)") +
  scale_y_log10() +
  ggtitle("")

# Anova of States with Day Dien Ghe/Lop van hoa
str(Tongso)
attach(Tongso)
av = aov(Platform_thickness ~ Context)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# Anova of States with Rong Dien Ghe/Tinh trang manh tuoc
str(Tongso)
attach(Tongso)
av = aov(Platform_thickness ~ Context1)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)



#------------------------------GOC GHE NGOAI/TRONG------------------------------
# For Goc ghe ngoai
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$External_platform_angle <- as.numeric(Tongso$External_platform_angle)
Tongso <- Tongso %>% drop_na(Context1)
Tongso <- Tongso %>% drop_na(Platform)
Tongso$Internal_platform_angle <- as.numeric(Tongso$Internal_platform_angle)
ggplot(Tongso, aes(x=reorder(Context1, External_platform_angle),
                   y = External_platform_angle)) +
  geom_boxplot(size = 1, width = 0.3, fatten = 0.8) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  facet_wrap( ~ Platform) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust =.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Góc ghè ngoài (o)") +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)")


# Goc ghe trong
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$External_platform_angle <- as.numeric(Tongso$External_platform_angle)
Tongso <- Tongso %>% drop_na(Context1)
Tongso <- Tongso %>% drop_na(Platform)
Tongso$Internal_platform_angle <- as.numeric(Tongso$Internal_platform_angle)
ggplot(Tongso, aes(x=reorder(Context1, Internal_platform_angle),
                   y = Internal_platform_angle)) +
  geom_boxplot(size = 1, width = 0.3, fatten = 0.8) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  facet_wrap( ~ Platform) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust =.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Góc ghè trong (o)") +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)")

# Anova GOC GHE TRONG/NGOAI
# Trong
str(Tongso)
attach(Tongso)
av = aov(Internal_platform_angle ~ Context1)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# NGOAI
str(Tongso)
attach(Tongso)
av = aov(External_platform_angle ~ Context1)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# MOI QUAN HE GOC GHE TRONG VA NGOAI
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggpubr)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$External_platform_angle <- as.numeric(Tongso$External_platform_angle)
Tongso <- Tongso %>% drop_na(Context1)
Tongso <- Tongso %>% drop_na(Platform)
Tongso$Internal_platform_angle <- as.numeric(Tongso$Internal_platform_angle)
Tongso <- Tongso %>% drop_na(Initiation)
ggscatter(Tongso , x = "Internal_platform_angle", y = "External_platform_angle",
          color = "black", shape = 1, size = 4, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "", label.x = 30, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 0, label.y = 25, size = 8) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Lớp văn hóa II và I (41.5-22.5ka)") +
  xlab("Góc ghè trong (o)") +
  ylab("Góc ghè ngoài (o)") +
  scale_x_log10()


#----------------------------------TY LE VO CUOI--------------------------------
# Mat Lung - Lop Van hoa II
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso1)
Tongso1 %>% 
  group_by(`Tylevocuoi`) %>%
  tally()
Tongso1_Tylevocuoi_tally <- Tongso1 %>% 
  group_by(`Tylevocuoi`) %>% 
  tally() %>% 
  filter(`Tylevocuoi` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso1_Tylevocuoi_tally,
       aes(x = reorder(`Tylevocuoi`, n), 
           y = n, fill = Context)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("
       (%)") +
  ylab("Tần suất") +
  ggtitle("Tỷ lệ vỏ cuội công cụ mảnh Tvh - II")


# Mat Lung - Lop Van hoa I
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
str(Tongso2)
Tongso2 %>% 
  group_by(`Tylevocuoi`) %>%
  tally()
Tongso2_Tylevocuoi_tally <- Tongso2 %>% 
  group_by(`Tylevocuoi`) %>% 
  tally() %>% 
  filter(`Tylevocuoi` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso2_Tylevocuoi_tally,
       aes(x = reorder(`Tylevocuoi`, n), 
           y = n, fill = Context)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("
       (%)") +
  ylab("Tần suất") +
  ggtitle("Tỷ lệ vỏ cuội công cụ mảnh Tvh - I")



# --------------------------------- DAU/DUOI MANH TUOC--------------------------------
# HINH THAI DAU MANH TUOC LOP VA HOA 2
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso1)
Tongso1 %>% 
  group_by(`Initiation`) %>%
  tally()
Tongso1_Initiation_tally <- Tongso1 %>% 
  group_by(`Initiation`) %>% 
  tally() %>% 
  filter(`Initiation` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso1_Initiation_tally,
       aes(x = reorder(`Initiation`, n), 
           y = n, fill = Context1)) +
  geom_bar(stat="identity", width = 0.35, size = 0.9, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size = 25),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab(" ") +
  ylab("Tần suất") +
  ggtitle("Hình thái đầu mặt bụng gần diện ghè công cụ mảnh Tvh - II")


# HINH THAI DUOI MANH TUOC LOP VA HOA 2
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso1)
Tongso1 %>% 
  group_by(`Termination`) %>%
  tally()
Tongso1_Termination_tally <- Tongso1 %>% 
  group_by(`Termination`) %>% 
  tally() %>% 
  filter(`Termination` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso1_Termination_tally,
       aes(x = reorder(`Termination`, n), 
           y = n, fill = Context1)) +
  geom_bar(stat="identity", width = 0.5, size = 0.9, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab(" ") +
  ylab("Tần suất") +
  ggtitle("Đuôi công cụ mảnh Tvh - II")



# HINH THAI DUOI MANH TUOC LOP VA HOA 1
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
str(Tongso2)
Tongso2 %>% 
  group_by(`Initiation`) %>%
  tally()
Tongso2_Initiation_tally <- Tongso2 %>% 
  group_by(`Initiation`) %>% 
  tally() %>% 
  filter(`Initiation` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso2_Initiation_tally,
       aes(x = reorder(`Initiation`, n), 
           y = n, fill = Context1)) +
  geom_bar(stat="identity", width = 0.35, size = 0.9, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size = 25),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("Tần suất") +
  ggtitle("Hình thái đầu mặt bụng gần diện ghè công cụ mảnh Tvh - I")


# HINH THAI DUOI MANH TUOC LOP VA HOA 1
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
str(Tongso2)
Tongso2 %>% 
  group_by(`Termination`) %>%
  tally()
Tongso2_Termination_tally <- Tongso2 %>% 
  group_by(`Termination`) %>% 
  tally() %>% 
  filter(`Termination` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso2_Termination_tally,
       aes(x = reorder(`Termination`, n), 
           y = n, fill = Context1)) +
  geom_bar(stat="identity", width = 0.5, size = 0.9, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab(" ") +
  ylab("Tần suất") +
  ggtitle("Đuôi công cụ mảnh Tvh - I")


#----------------------------------VI TRI TU CHINH------------------------------
# MAT LUNG - LOP 41.5-22.5KA
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso1)
View(Tongso1)
Tongso1 %>% 
  group_by(`Tuchinhlung`) %>%
  tally()
Tongso1_Tuchinhlung_tally <- Tongso1 %>% 
  group_by(`Tuchinhlung`) %>% 
  tally() %>% 
  filter(`Tuchinhlung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso1_Tuchinhlung_tally,
       aes(x = reorder(`Tuchinhlung`, n), 
           y = n, fill = Context)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("
       Tần suất") +
  coord_flip() +
  ggtitle("Vị trí tu chỉnh mặt lưng Tvh-II")


# MAT BUNG - LOP 41.5-22.5KA
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso1)
View(Tongso1)
Tongso1 %>% 
  group_by(`Tuchinhbung`) %>%
  tally()
Tongso1_Tuchinhbung_tally <- Tongso1 %>% 
  group_by(`Tuchinhbung`) %>% 
  tally() %>% 
  filter(`Tuchinhbung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso1_Tuchinhbung_tally,
       aes(x = reorder(`Tuchinhbung`, n), 
           y = n, fill = Context)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("
       Tần suất") +
  coord_flip() +
  ggtitle("Vị trí tu chỉnh mặt bụng Tvh-II")


#---------------------------------- TU CHINH MOT MAT--------------------41-30ka
# MAT LUNG
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso13 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang4135Lung')
str(Tongso13)
Tongso13 %>% 
  group_by(`Tuchinhlung`) %>%
  tally()
Tongso13_Tuchinhlung_tally <- Tongso13 %>% 
  group_by(`Tuchinhlung`) %>% 
  tally() %>% 
  filter(`Tuchinhlung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso13_Tuchinhlung_tally,
       aes(x = reorder(`Tuchinhlung`, n), 
           y = n, fill = State)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("
       Tần suất") +
  coord_flip() +
  ggtitle("Công cụ tu chỉnh một mặt của mặt lưng Tvh-II")


# MAT BUNG
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso14 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35Bung')
str(Tongso14)
Tongso14 %>% 
  group_by(`Tuchinhbung`) %>%
  tally()
Tongso14_Tuchinhbung_tally <- Tongso14 %>% 
  group_by(`Tuchinhbung`) %>% 
  tally() %>% 
  filter(`Tuchinhbung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso14_Tuchinhbung_tally,
       aes(x = reorder(`Tuchinhbung`, n), 
           y = n, fill = State)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("
       Tần suất") +
  coord_flip() +
  ggtitle("Công cụ tu chỉnh một mặt của mặt bụng Tvh-II")

  
#----------------------------------VI TRI TU CHINH - 28-22ka--------------------
# MAT LUNG
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
str(Tongso2)
Tongso2 %>% 
  group_by(`Tuchinhlung`) %>%
  tally()
Tongso2_Tuchinhlung_tally <- Tongso2 %>% 
  group_by(`Tuchinhlung`) %>% 
  tally() %>% 
  filter(`Tuchinhlung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso2_Tuchinhlung_tally,
       aes(x = reorder(`Tuchinhlung`, n), 
           y = n, fill = State)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("
       Tần suất") +
  coord_flip() +
  ggtitle("Vị trí tu chỉnh mặt lưng Tvh-I")


# MAT BUNG
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
str(Tongso2)
Tongso2 %>% 
  group_by(`Tuchinhbung`) %>%
  tally()
Tongso2_Tuchinhbung_tally <- Tongso2 %>% 
  group_by(`Tuchinhbung`) %>% 
  tally() %>% 
  filter(`Tuchinhbung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso2_Tuchinhbung_tally,
       aes(x = reorder(`Tuchinhbung`, n), 
           y = n, fill = State)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("
       Tần suất") +
  coord_flip() +
  ggtitle("Vị trí tu chỉnh mặt bụng Tvh-I")


#---------------------------------- TU CHINH MOT MAT--------------------
# MAT LUNG
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso11 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang2822Lung')
str(Tongso11)
View(Tongso11)
Tongso11 %>% 
  group_by(`Tuchinhlung`) %>%
  tally()
Tongso11_Tuchinhlung_tally <- Tongso11 %>% 
  group_by(`Tuchinhlung`) %>% 
  tally() %>% 
  filter(`Tuchinhlung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso11_Tuchinhlung_tally,
       aes(x = reorder(`Tuchinhlung`, n), 
           y = n, fill = State)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("
       Tần suất") +
  coord_flip() +
  ggtitle("Công cụ tu chỉnh một mặt của mặt lưng Tvh-I")

# MAT LUNG
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso12 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang2822Bung')
str(Tongso12)
View(Tongso12)
Tongso12 %>% 
  group_by(`Tuchinhbung`) %>%
  tally()
Tongso12_Tuchinhbung_tally <- Tongso12 %>% 
  group_by(`Tuchinhbung`) %>% 
  tally() %>% 
  filter(`Tuchinhbung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso12_Tuchinhbung_tally,
       aes(x = reorder(`Tuchinhbung`, n), 
           y = n, fill = State)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("
       Tần suất") +
  coord_flip() +
  ggtitle("Công cụ tu chỉnh một mặt dưới bụng Tvh-I")


##------------------------------LPO 41.5KA - 22.0KA---------------------
#---------------------------HINH THAI DAU/DUOI/DIEN GHE/----------------
# VI TRI TU CHINH LUNG 41-22KA
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso <- Tongso %>% drop_na(Tuchinhbung)
ggplot(Tongso) +
  aes(x = Tylevocuoi, fill = Context1) +
  geom_bar(position = "fill", width= 3, size = 0.9) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', position=position_fill(vjust=0.3)) +
  facet_wrap( ~ Context1) +
  theme(aspect.ratio = 12/6) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.3),
        axis.text.x = element_text(size = 25, angle = 0, vjust = 0.5)) +
  theme(legend.position="top") +
  xlab(" Vị trí tu chỉnh mặt bụng ") +
  ylab("
       (%)") +
  ggtitle(" ") +
  labs(fill = "Lớp văn hóa") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous() +
  coord_flip()

# VI TRI TU CHINH LUNG - 41.5ka-22ka
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso <- Tongso %>% drop_na(Tuchinhlung)
ggplot(Tongso) +
  aes(x = Tuchinhlung, fill =Context1) +
  geom_bar(position = "fill", width=0.9, size = 0.9) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', position=position_fill(vjust=0.3)) +
  facet_wrap( ~ Context1) +
  theme(aspect.ratio = 12/6) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.3),
        axis.text.x = element_text(size = 25, angle = 0, vjust = 0.5)) +
  theme(legend.position="top") +
  xlab(" Vị trí tu chỉnh mặt lưng ") +
  ylab("
       (%)") +
  ggtitle(" ") +
  labs(fill = "Lớp văn hóa") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous() +
  coord_flip()


# HE SO TUONG QUAN VE QUY MO DIEN GHE
# Platform Thickness/Width
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$Platform_width <- as.numeric(Tongso$Platform_width)
Tongso$Platform_thickness <- as.numeric(Tongso$Platform_thickness)
ggscatter(Tongso , x = "Platform_width", y = "Platform_thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 30, size = 13) +
  geom_point(alpha = 0.1, size = 7, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context1) +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Hệ số tương quan - Lớp văn hóa II") +
  xlab("Chiều dày (mm)") +
  ylab("Chiều rộng (mm)")


# Internal/External Angle
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$External_platform_angle <- as.numeric(Tongso$External_platform_angle)
Tongso$Internal_platform_angle <- as.numeric(Tongso$Internal_platform_angle)
ggscatter(Tongso , x = "External_platform_angle", y = "Internal_platform_angle",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 40, size = 15) +
  geom_point(alpha = 0.2, size = 4, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context1) +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Hệ số tương quan") +
  xlab("Góc ghè ngoài (o)") +
  ylab("Góc ghè trong (o)")


# Mass/Length
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$Mass <- as.numeric(Tongso$Mass)
Tongso$Length <- as.numeric(Tongso$Length)
ggscatter(Tongso , x = "Length", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 400, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Hệ số tương quan") +
  xlab("Chiều dài (mm)") +
  ylab("Trọng lượng (g)")


# Mass/Width
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$Mass <- as.numeric(Tongso$Mass)
Tongso$Width <- as.numeric(Tongso$Width)
ggscatter(Tongso, x = "Width", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 400, size=13) +
  geom_point(alpha = 1, size = 4, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều rộng (mm)") +
  ylab("Trọng lượng (g)")


# Mass/Thickness
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
Tongso$Mass <- as.numeric(Tongso$Mass)
Tongso$Thickness <- as.numeric(Tongso$Thickness)
ggscatter(Tongso , x = "Thickness", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 400, size=13) +
  geom_point(alpha = 0.08, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Context1) +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Hệ số tương quan") +
  xlab("Chiều dày (mm)") +
  ylab("Trọng lượng (g)")


# Types of Striking platforms ngay 3- 7 - 2024
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
lop2 %>% 
  group_by(`Platform_types`) %>%
  tally()
lop2_Platform_types_tally <- lop2 %>% 
  group_by(`Platform_types`) %>% 
  tally() %>% 
  filter(`Platform_types` != 'NA') %>%  # REMOVE NAs 
  arrange(desc(n))
ggplot(lop2_Platform_types_tally,
       aes(x = reorder(`Platform_types`, n), y = n)) +
  geom_bar(stat="identity", size = 0.8, 
           fill = "white", color = "black", 
           width = 0.8) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Tần suất") +
  ggtitle("Diện ghè tầng văn hóa II") +
  coord_flip()


# Distribution of Platform Width/Types of Striking Platforms - tang van hoa II
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
ggplot(lop2 , aes(x = reorder(Platform, Platform_width),
                  y = Platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap(~ Context) +
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
lop2$Platform_width <- as.numeric(lop2$Platform_width)
av = aov(Platform_width ~ Platform_types)
summary(av)
av
TukeyHSD(av)
l2 = TukeyHSD(av)
plot(l2)



#--------------------------- VI TRI TU CHINH------------------------------------
# MAT LUNG - 41.5-22.5KA
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
Tongso %>% 
  group_by(`Tuchinhlung`) %>%
  tally()
Tongso_Tuchinhlung_tally <- Tongso %>% 
  group_by(`Tuchinhlung`) %>% 
  tally() %>% 
  filter(`Tuchinhlung` != 'NA') %>% 
  arrange(desc(n))
ggplot(Tongso_Tuchinhlung_tally,
       aes(x = reorder(`Tuchinhlung`, n), 
           y = n, fill = Context)) +
  geom_bar(stat="identity", width = 0.9, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size= 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("Tần suất") +
  coord_flip() +
  ggtitle("Vị trí tu chỉnh mặt lưng Tvh - II và I")


###------------------------- MAT LUNG--------------------------------
# Ty le vo cuoi
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
str(lop2)
lop2
lop2$Tylevocuoi <- as.numeric(lop2$Tylevocuoi)
lop2_Tylevocuoi_tally <- lop2 %>% 
  group_by(`Tylevocuoi`) %>% 
  tally() %>% 
  filter(`Tylevocuoi` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop2_Tylevocuoi_tally,
       aes(x = reorder(`Tylevocuoi`, n), 
           y = n, fill = unit)) +
  geom_bar(stat="identity", size = 0.5, linewidth = 0.8, color="black", fill="gray") +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) +
  xlab("
       (%)") +
  ylab("Tần suất") +
  ggtitle("Tỷ lệ vỏ cuội trên các công cụ mảnh Tvh-II")


# Vi tri vo cuoi
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
str(lop2)
lop2
lop2 %>% 
  group_by(`Vitrivocuoi`) %>%
  tally()
lop2_Vitrivocuoi_tally <- lop2 %>% 
  group_by(`Vitrivocuoi`) %>% 
  tally() %>% 
  filter(`Vitrivocuoi` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop2_Vitrivocuoi_tally,
       aes(x = reorder(`Vitrivocuoi`, n), 
           y = n, fill = unit)) +
  geom_bar(stat="identity", size = 0.8, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size =20, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("Tần suất") +
  coord_flip() +
  ggtitle("Vị trí vỏ cuội trên các công cụ mảnh Tvh-II")


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
ggplot(lop2 , aes(x = reorder(Platform, Platform_width),
                  y = Platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap(~ Context) +
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
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
str(lop2)
lop2
ggplot(lop2 , aes(x = reorder(Platform_types, Platform_thickness),
                  y = Platform_thickness)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.2, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ Unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thikcness (mm)") +
  ylim(0,50) +
  ggtitle("Unit 1 and Unit 2") +
  labs(fill = "Flakes") +
  
  # Anova of States with Thickness - Unit 1
  str(flake_u12)
attach(flake_u12)
flake_u12$platform_thickness <- as.numeric(flake_u12$platform_thickness)
av = aov(platform_thickness ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


## Dimensions-----------------------------------------------------
# Lop SOM
## Kieu Platoforms voi Quy mo
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
lop2$Mass <- as.numeric(lop2$Mass)
lop2 <- lop2 %>% drop_na(Mass)
lop2 <- lop2 %>% drop_na(State)
lop2 <- lop2 %>% drop_na(Platform)
ggplot(lop2, aes(x = reorder(State, Mass),
                 y = Mass, fill = Platform)) + 
  geom_boxplot(width = 0.8, size = 1, fatten = 0.6, outlier.shape = NA) +
  geom_point(alpha = 0.00) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  scale_y_log10() +
  ylab("Trọng lượng (g)") + 
  xlab("") +
  ggtitle("Lớp văn hóa II ") +
  labs(fill = "") +
  coord_flip()

# Anova
str(lop2)
attach(lop2)
lop2$Platform_width <- as.numeric(lop2$Platform_width)
av = aov(Platform_width ~ Platform)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


## Kieu Platoforms voi Chieu Rong Dien Ghe
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
lop2$Platform_width <- as.numeric(lop2$Platform_width)
lop2 <- lop2 %>% drop_na(Length)
lop2 <- lop2 %>% drop_na(State)
lop2 <- lop2 %>% drop_na(Platform)
ggplot(lop2, aes(x = reorder(State, Platform_width),
                 y =Platform_width, fill = Platform)) + 
  geom_boxplot(width = 0.8, size = 0.8, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.00) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  scale_y_log10() +
  ylab("
       Rộng diện ghè (mm)") + 
  xlab("") +
  ggtitle("Lớp văn hóa II (41.5-30.0ka) ") +
  labs(fill = "") +
  coord_flip()


# Anova# Anova# AnovaLength# Anova# Anova# Anova
str(lop2)
attach(lop2)
lop2$Platform_width <- as.numeric(lop2$Platform_width)
av = aov(Platform_width ~ Platform)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


## Kieu Platforms voi Chieu day dien ghe
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
lop2$Platform_thickness <- as.numeric(lop2$Platform_thickness)
lop2 <- lop2 %>% drop_na(Platform_thcikness)
lop2 <- lop2 %>% drop_na(State)
lop2 <- lop2 %>% drop_na(Platform)
ggplot(lop2, aes(x = reorder(State, Platform_thickness),
                 y =Platform_thickness, fill = Platform)) + 
  geom_boxplot(width = 0.8, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.0) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  scale_y_log10() +
  ylab("
       Dày diện ghè (mm)") + 
  xlab("") +
  ggtitle("Lớp văn hóa II (41.5-30.0ka)") +
  labs(fill = "") +
  coord_flip()

# Anova
str(lop2)
attach(lop2)
lop2$Platform_thicknes <- as.numeric(lop2$Platform_thickness)
av = aov(Platform_width ~ Platform)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)

## So vet ghe tren Dien ghe
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
lop2$Sovetghe_dienghe <- as.numeric(lop2$Sovetghe_dienghe)
lop2 <- lop2 %>% drop_na(Sovetghe_dienghe)
lop2 <- lop2 %>% drop_na(State)
lop2 <- lop2 %>% drop_na(Material)
ggplot(lop2, aes(x = reorder(Platform, Sovetghe_dienghe),
                 y = Sovetghe_dienghe, fill = Material)) + 
  geom_boxplot(width = 0.5, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.8) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust=.5)) +
  theme(legend.position="right") +
  ylab("Số vết ghè ở diện ghè (mm)") + 
  xlab("") +
  ggtitle("Lớp văn hóa II ") +
  labs(fill = "")

# Anova
str(lop2)
attach(lop2)
lop2$Sovetghe_dienghe <- as.numeric(lop2$Sovetghe_dienghe)
av = aov(Sovetghe_dienghe ~ Platform)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


## External platform-angle and Materila/Platforms
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
lop2$External_platform_angle <- as.numeric(lop2$External_platform_angle)
lop2 <- lop2 %>% drop_na(External_platform_angle)
lop2 <- lop2 %>% drop_na(Platform)
lop2 <- lop2 %>% drop_na(Material)
ggplot(lop2, aes(x = reorder(Platform,External_platform_angle),
                 y = External_platform_angle, fill = Material)) + 
  geom_boxplot(width = 0.5, size = 0.6, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  ylab("Góc ghè ngoài (o)") + 
  xlab("") +
  ggtitle("Lớp văn hóa II (41.5-30.0ka) ") +
  labs(fill = "")

# Anova
str(lop2)
attach(lop2)
lop2$External_platform_angle <- as.numeric(lop2$External_platform_angle)
av = aov(External_platform_angle ~ Context)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


## Internal platform-angle and Material/Platforms
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
lop2$Internal_platform_angle <- as.numeric(lop2$Internal_platform_angle)
lop2 <- lop2 %>% drop_na(Internal_platform_angle)
lop2 <- lop2 %>% drop_na(Platform)
lop2 <- lop2 %>% drop_na(Material)
ggplot(lop2, aes(x = reorder(Platform, Internal_platform_angle),
                 y = Internal_platform_angle, fill = Material)) + 
  geom_boxplot(width = 0.5, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.02) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  scale_y_log10() +
  ylab("Góc ghè trong (o)") + 
  xlab("") +
  ylim(40, 150) +
  ggtitle("Lớp văn hóa II (41.5-30.0ka)") +
  labs(fill = "")

# Anova
str(lop2)
attach(lop2)
lop2$Internal_platform_angle <- as.numeric(lop2$Internal_platform_angle)
av = aov(Internal_platform_angle~ Context)
summary(av)
av
TukeyHSD(av)
lop21 = TukeyHSD(av)
plot(lop21)


#-----------------------DAU MANH TUOC-------------------------------------------
# Hinh thai Dau manh tuoc
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
lop2 %>% 
  group_by(`Inititation`) %>%
  tally()
lop2_Inititation_tally <- lop2 %>% 
  group_by(`Inititation`) %>% 
  tally() %>% 
  filter(`Inititation` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop2_Inititation_tally,
       aes(x = reorder(`Inititation`, n), 
           y = n, fill = Context)) +
  geom_bar(stat="identity", width = 0.6, color="black", size = 1.3, fill="white") +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust = 0.5)) +
  xlab("Hình dáng") +
  ylab("So luong")


# Nhap 

# Chua co so lieu
#-------------------------------------------------------------
# DIMENSIONS for Cortical flakes (SECONDARY FLAKES) (Not 100%)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
Cortexflake$mass <- as.numeric(Cortexflake$mass)
ggplot(Cortexflake, aes(x=reorder(material, mass),
                        y = mass)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Mass(gram)") +
  scale_y_log10() +
  ggtitle("")

# Anova of States with Mass
str(Cortexflake)
attach(Cortexflake)
av = aov(mass ~ material)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)




