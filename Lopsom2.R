#-------------------------QUY MO LOP VAN HOA 41.5-30.0KA------------------------
# For Mass
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
library(sf)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
attach(Tongso2)
str(Tongso2)
Tongso2$Mass <- as.numeric(Tongso2$Mass)
Tongso2 <- Tongso2 %>% drop_na(Mass)
Tongso2 <- Tongso2 %>% drop_na(Material)
ggplot(Tongso2, aes(x=reorder(Material, Mass),
                   y = Mass)) +
  geom_boxplot(outlier.shape = NA, width = 0.8, size = 0.6) +
  geom_jitter(alpha = 0.6) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Mass (gram)") +
  scale_y_log10() +
  ggtitle("Trọng lượng công cụ mảnh Tvh II (41.5-30.0ka)") +
  coord_flip()

# Anova of Material and Mass
str(Tongso2)
attach(Tongso2)
av = aov(Mass ~ State)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# Do voi Length
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
attach(Tongso2)
str(Tongso2)
Tongso2$Length <- as.numeric(Tongso2$Length)
Tongso2 <- Tongso2 %>% drop_na(Length)
Tongso2 <- Tongso2 %>% drop_na(Material)
ggplot(Tongso2, aes(x=reorder(Material, Length),
                   y = Length)) +
  geom_boxplot(size = 1, width = 0.7, fatten = 2) +
  geom_jitter(alpha = 0.6) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Chiều dài (mm)") +
  scale_y_log10() +
  ggtitle("") +
  coord_flip() +
  ggtitle("Trọng lượng công cụ mảnh Tvh II (41.5-30.0ka)")


# Anova of States with Length- Lop van hoa II - voi State
str(Tongso2)
attach(Tongso2)
av = aov(Length ~ State)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# Anova of States with Length- Lop van hoa II - voi Material
str(Tongso2)
attach(Tongso2)
av = aov(Length ~ State)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# Doi voi Width - Nguyen lieu
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
attach(Tongso2)
str(Tongso2)
Tongso2$Thickness <- as.numeric(Tongso2$Thickness)
Tongso2$Width <- as.numeric(Tongso2$Width)
Tongso2 <- Tongso2 %>% drop_na(State)
ggplot(Tongso2, aes(x=reorder(State, Width),
                   y = Width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.6) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("width (mm)") +
  scale_y_log10() +
  ggtitle("") +
  coord_flip()


# Anova of Material and Width
str(Tongso2)
attach(Tongso2)
av = aov(Width ~ State)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# Doi voi chieu day - Thickness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
attach(Tongso2)
str(Tongso2)
Tongso2$Thickness <- as.numeric(Tongso2$Thickness)
Tongso2 <- Tongso2 %>% drop_na(Thickness)
Tongso2 <- Tongso2 %>% drop_na(State)
ggplot(Tongso2, aes(x=reorder(State, Thickness),
                   y = Thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Thickness (mm)") +
  ggtitle("") +
  coord_flip()

# Anova of States with Length- Unit 1
str(Tongso2)
attach(Tongso2)
av = aov(Thickness ~ State)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# For Platform_Width
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
attach(Tongso2)
str(Tongso2)
Tongso2$Platform_width <- as.numeric(Tongso2$Platform_width)
Tongso2 <- Tongso2 %>% drop_na(State)
Tongso2$Platform_width <- as.numeric(Tongso2$Platform_width)
ggplot(Tongso2, aes(x=reorder(State, Platform_width),
                    y = Platform_width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Chiều dày diện ghè (mm)") +
  ggtitle("") +
  coord_flip()


# Anova of States with Length- Unit 1
str(Tongso2)
attach(Tongso2)
av = aov(Platform_width ~ State)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# For Platform_Thickness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso2)
attach(Tongso2)
Tongso2$Platform_thickness <- as.numeric(Tongso2$Platform_thickness)
ggplot(Tongso2, aes(x=reorder(Material, Platform_thickness),
                   y = Platform_thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Dày diện ghè (mm)") +
  scale_y_log10() +
  ggtitle("") +
  coord_flip()

# Anova  co CHIEU DAY VA NGUYEN LIEU
str(Tongso2)
attach(Tongso2)
av = aov(Platform_thickness ~ Material)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


#----------------------------------VI TRI TU CHINH------------------------------
# Mat Lung
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
View(Tongso2)
attach(Tongso2)
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
  ylab("Tần suất") +
  coord_flip() +
  ggtitle("Vị trí tu chỉnh mặt lưng Tvh - II")


# Mat Bung
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
View(Tongso2)
attach(Tongso2)
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
  ylab("Tần suất") +
  coord_flip() +
  ggtitle("Vị trí tu chỉnh mặt bụng Tvh-II")


##------------------------------LPO 41.5KA - 35.0KA---------------------
#---------------------------HINH THAI DAU/DUOI/DIEN GHE/----------------
# HINH THAI DAU VA DUOI TUONG UNG VOI LOAI HINH NGUYEN LIEU 41.5ka-34.1ka
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
View(Tongso2)
attach(Tongso2)
ggplot(Tongso2) +
  aes(x = Initiation, fill = Material) +
  geom_bar(position = "fill", width=0.8, size = 0.9) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', position=position_fill(vjust=0.3)) +
  theme(aspect.ratio = 12/6) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.3),
        axis.text.x = element_text(size = 25, angle = 0, vjust = 0.5)) +
  theme(legend.position="top") +
  xlab("Excavation contexts") +
  ylab("Percentage") +
  ggtitle("Unit 1 (Contexts 3-6)") +
  labs(fill = "States") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous()


# HE SO TUONG QUAN VE QUY MO DIEN GHE
# Platform Thickness/Width
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(ggpubr)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
attach(Tongso2)
Tongso2$Platform_width <- as.numeric(Tongso2$Platform_width)
Tongso2$Platform_thickness <- as.numeric(Tongso2$Platform_thickness)
ggscatter(Tongso2 , x = "Platform_width", y = "Platform_thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 40, size=13) +
  geom_point(alpha = 0.1, size = 8, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw(base_size = 25) +
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
library(ggpubr)
library(readxl)
Tongso2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(Tongso2)
attach(Tongso2)
Tongso2$External_platform_angle <- as.numeric(Tongso2$External_platform_angle)
Tongso2$Internal_platform_angle <- as.numeric(Tongso2$Internal_platform_angle)
ggscatter(Tongso2 , x = "External_platform_angle", y = "Internal_platform_angle",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 40, size = 15) +
  geom_point(alpha = 0.1, size = 8, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Lớp văn hóa II") +
  xlab("Góc ghè ngoài (o)") +
  ylab("Góc ghè trong (o)")

# Mass/Length
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
View(lop2)
lop2$Mass <- as.numeric(lop2$Mass)
lop2$Length <- as.numeric(lop2$Length)
ggscatter(lop2 , x = "Length", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 500, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều dài (mm)") +
  ylab("Trọng lượng (g)")


# Mass/Width
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
lop2$Mass <- as.numeric(lop2$Mass)
lop2$Width <- as.numeric(lop2$Width)
ggscatter(lop2 , x = "Width", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 500, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều rộng (mm)") +
  ylab("Trọng lượng (g)")

# Mass/Thickness
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
lop2$Mass <- as.numeric(lop2$Mass)
lop2$Thickness <- as.numeric(lop2$Thickness)
ggscatter(lop2 , x = "Thickness", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 500, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
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


#-----------------------DUOI MANH TUOC------------------
# Hinh thai duoi manh tuoc
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
# Vị trí vỏ cuội -rat OK
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
  group_by(`Vitrivocuoi`) %>%
  tally()
lop2_Vitrivocuoi_tally <- lop2 %>% 
  group_by(`Vitrivocuoi`) %>% 
  tally() %>% 
  filter(`Vitrivocuoi` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop2_Vitrivocuoi_tally,
       aes(x = reorder(`Vitrivocuoi`, n), 
           y = n, fill = State)) +
  geom_bar(stat="identity", size = 0.8, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5)) +
  xlab("Vị trí vỏ cuội mặt lưng") +
  ylab("Tần suất") +
  coord_flip()


# Hinh thai duoi manh tuoc - rat OK
library(ggplot2)
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
  group_by(`Termination`) %>%
  tally()
lop2_Termination_tally <- lop2 %>% 
  group_by(`Termination`) %>% 
  tally() %>% 
  filter(`Termination` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop2_Termination_tally,
       aes(x = reorder(`Termination`, n), 
           y = n, fill = State)) +
  geom_bar(stat="identity", width = 0.4, color="black", size = 0.8, fill="white") +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5)) +
  xlab("Kiểu đuôi") +
  ylab("Số lượng")


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

