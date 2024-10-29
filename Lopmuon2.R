#----------------------------------VI TRI TU CHINH----------------------
# Mat Lung
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
View(lop1)
attach(lop1)
lop1 %>% 
  group_by(`Tuchinhlung`) %>%
  tally()
lop1_Tuchinhlung_tally <-lop1 %>% 
  group_by(`Tuchinhlung`) %>% 
  tally() %>% 
  filter(`Tuchinhlung` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop1_Tuchinhlung_tally,
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
  ggtitle("Vị trí tu chỉnh mặt lưng Tvh-I")


# Mat Bung
library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
View(lop1)
attach(lop1)
lop1 %>% 
  group_by(`Tuchinhbung`) %>%
  tally()
lop1_Tuchinhbung_tally <-lop1 %>% 
  group_by(`Tuchinhbung`) %>% 
  tally() %>% 
  filter(`Tuchinhbung` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop1_Tuchinhbung_tally,
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
  ggtitle("Vị trí tu chỉnh mặt bụng Tvh-I")



##------------------------------Lop 28.0 - 22.5KA---------------------
#---------------------------HINH THAI DAU/DUOI/DIEN GHE/----------------
# HINH THAI DAU VA DUOI TUONG UNG VOI LOAI HINH NGUYEN LIEU 41.5ka-34.1ka
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
ggplot(lop1) +
  aes(x = Initiation, fill = Material) +
  geom_bar(position = "fill", width=0.6, size = 0.9) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', position=position_fill(vjust=0.3)) +
  theme(aspect.ratio = 12/9) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=0.3),
        axis.text.x = element_text(size = 25, angle= 0, vjust=0.5)) +
  theme(legend.position="top") +
  xlab("Excavation contexts") +
  ylab("Percentage") +
  ggtitle("Unit 1 (Contexts 3-6)") +
  labs(fill = "States") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous()


# HE SO TUONG QUAN VE QUY MO DIEN GHE
# Mass/Max_dimension
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
ggplot(lop1) +
lop1$Mass <- as.numeric(lop1$Mass)
lop1$Max_dimension <- as.numeric(lop1$Max_dimension)
ggscatter(lop1 , x = "Max_dimension", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 300, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Lớp văn hóa I (L4-L10)") +
  xlab("Dài tối đa (mm)") +
  ylab("Trọng lượng (g)")


# Mass/Length
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
str(lop1)
View(lop1)
lop1$Mass <- as.numeric(lop1$Mass)
lop1$Length <- as.numeric(lop1$Length)
ggscatter(lop1 , x = "Length", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 150, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Chiều dài (mm)") +
  ylab("Trọng lượng (g)")


# He so tuong quan Platform width/Thickness
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
str(lop1)
View(lop1)
lop1$Platform_thickness <- as.numeric(lop1$Platform_thickness)
lop1$Platform_width <- as.numeric(lop1$Platform_thickness)
ggscatter(lop1 , x = "Platform_thickness", y = "Platform_width",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 22, size=11) +
  geom_point(alpha = 0.1, size = 8, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw(base_size = 18) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Hệ số tương quan - Lớp văn hóa I") +
  xlab("Chiều dày diện ghè (mm)") +
  ylab("Chiều rộng diện ghè (mm)")


# He so tuong quan Internal/External Angle
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
str(lop1)
View(lop1)
lop1$External_platform_angle <- as.numeric(lop1$External_platform_angle)
lop1$Internal_platform_angle <- as.numeric(lop1$Internal_platform_angle)
ggscatter(lop1 , x = "External_platform_angle", y = "Internal_platform_angle",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 80, size=13) +
  geom_point(alpha = 0.1, size = 8, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw(base_size = 25) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Hệ số tương quan - Lớp văn hóa I") +
  xlab("Góc ghè ngoài (o)") +
  ylab("Góc ghè trong (o)")


# Mass/Width
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
str(lop1)
lop1$Mass <- as.numeric(lop1$Mass)
lop1$Width <- as.numeric(lop1$Width)
ggscatter(lop1 , x = "Width", y = "Mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 150, size=13) +
  geom_point(alpha = 0.09, size = 6, fill = "black" , color = "black") +
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
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1$Mass <- as.numeric(lop1$Mass)
lop1$Thickness <- as.numeric(lop1$Thickness)
ggscatter(lop1 , x = "Thickness", y = "Mass",
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
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1 %>% 
  group_by(`Platform_types`) %>%
  tally()
lop1_Platform_types_tally <- lop1 %>% 
  group_by(`Platform_types`) %>% 
  tally() %>% 
  filter(`Platform_types` != 'NA') %>%  # REMOVE NAs 
  arrange(desc(n))
ggplot(lop1_Platform_types_tally,
       aes(x = reorder(`Platform_types`, n), y = n)) +
  geom_bar(stat="identity", size = 1, 
           fill = "white", color = "black", 
           width = 0.8) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Frequency") +
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
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
str(lop1)
lop1
str(lop1)
lop1$Platform_width <- as.numeric(lop1$Platform_width)
lop1$Platform <- as.numeric(lop1$Platform)
ggplot(lop1, aes(x= reorder(Context, Platform_width),
                  y = Platform_width)) + 
  geom_boxplot(width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap(~ Platform) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 15, angle= 45, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Rộng diện ghè (mm)") +
  ggtitle("Tầng văn hóa I") +
  labs(fill = "Flakes")


# Anova of States with Thickness - Unit 1
str(lop1)
attach(lop1)
lop1$Platform_width <- as.numeric(lop1$Platform_width)
av = aov(Platform_width ~ Platform)
summary(av)
av
TukeyHSD(av)
lop21 = TukeyHSD(av)
plot(lop21)


# Loai hinh dien ghe va chieu day tang van hoa som  - 1 
# Chua Ok
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(ggpubr)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
lop1$Platform_thickness <- as.numeric(lop1$Platform_thickness)
ggplot(lop1 , aes(x = reorder(Platform, Platform_thickness),
                  y = Platform_thickness)) + 
  geom_boxplot(outlier.shape = NA, width = 0.6, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap(~ Context) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Rộng diện ghè (mm)") +
  ggtitle("Tầng văn hóa I (28.0-22.5ka)") +
  labs(fill = "Flakes") +
  coord_flip()

# Anova of States with Thickness - Unit 1
str(lop1)
attach(lop1)
lop1$Platform_thickness <- as.numeric(lop1$Platform_thickness)
av = aov(Platform_thickness ~ Platform)
summary(av)
av
TukeyHSD(av)
lop21 = TukeyHSD(av)
plot(lop21)


# Ty le vo cuoi - rat OK
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
lop1$Tylevocuoi <- as.numeric(lop1$Tylevocuoi)
lop1_Tylevocuoi_tally <- lop1 %>% 
  group_by(`Tylevocuoi`) %>% 
  tally() %>% 
  filter(`Tylevocuoi` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop1_Tylevocuoi_tally,
       aes(x = reorder(`Tylevocuoi`, n), 
           y = n, fill = unit)) +
  geom_bar(stat="identity", size = 0.5, linewidth = 0.8, color="black", fill="gray") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5)) +
  xlab("Số lượng mảnh tước còn vỏ cuội từ 5% đến 100%") +
  ylab("Tần suất") +
  ggtitle("Tỷ lệ vỏ cuội trên các công cụ mảnh Tvh-I")


# Vi tri vo cuoi - rat OK
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
lop1 %>% 
  group_by(`Vitrivocuoi`) %>%
  tally()
lop1_Vitrivocuoi_tally <- lop1 %>% 
  group_by(`Vitrivocuoi`) %>% 
  tally() %>% 
  filter(`Vitrivocuoi` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop1_Vitrivocuoi_tally,
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
  ggtitle("Vị trí vỏ cuội trên các công cụ mảnh Tvh-I")


## Dimensions-----------------------------------------------------
# Lop MUON
## Kieu Platoforms voi Quy mo
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1$Mass <- as.numeric(lop1$Mass)
lop1 <- lop1 %>% drop_na(Mass)
lop1 <- lop1 %>% drop_na(State)
lop1 <- lop1 %>% drop_na(Platform)
ggplot(lop1, aes(x = reorder(State, Mass),
                 y = Mass, fill = Platform)) + 
  geom_boxplot(width = 1.0, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.5) +
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
str(lop1)
attach(lop1)
lop1$Platform_width <- as.numeric(lop1$Platform_width)
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
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1$Platform_width <- as.numeric(lop1$Platform_width)
lop1 <- lop1 %>% drop_na(Length)
lop1 <- lop1 %>% drop_na(State)
lop1 <- lop1 %>% drop_na(Platform)
ggplot(lop1, aes(x = reorder(State, Platform_width),
                 y =Platform_width, fill = Platform)) + 
  geom_boxplot(width = 1.0, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.8) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  scale_y_log10() +
  ylab("Rộng diện ghè (g)") + 
  xlab("") +
  ggtitle("Lớp văn hóa II ") +
  labs(fill = "") +
  coord_flip()

# Anova
str(lop1)
attach(lop1)
lop1$Platform_width <- as.numeric(lop1$Platform_width)
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
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1$Platform_thickness <- as.numeric(lop1$Platform_thickness)
lop1 <- lop1 %>% drop_na(Platform_thickness)
lop1 <- lop1 %>% drop_na(State)
lop1 <- lop1 %>% drop_na(Platform)
ggplot(lop1, aes(x = reorder(State, Platform_thickness),
                 y =Platform_thickness, fill = Platform)) + 
  geom_boxplot(width = 0.8, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.8) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  scale_y_log10() +
  ylab("Rộng diện ghè (g)") + 
  xlab("") +
  ggtitle("Lớp văn hóa II ") +
  labs(fill = "") +
  coord_flip()

# Anova
str(lop1)
attach(lop1)
lop1$Platform_thickness <- as.numeric(lop1$Platform_thickness)
av = aov(Platform_thickness ~ Platform)
summary(av)
av
TukeyHSD(av)
lop1_1 = TukeyHSD(av)
plot(lop1_1)

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
ggplot(lop2, aes(x = reorder(Platform, External_platform_angle),
                 y = External_platform_angle, fill = Material)) + 
  geom_boxplot(width = 0.5, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.02) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  ylab("Góc ghè ngoài (o)") + 
  xlab("") +
  ggtitle("Lớp văn hóa II ") +
  labs(fill = "")
# Anova
str(lop2)
attach(lop2)
lop2$External_platform_angle <- as.numeric(lop2$External_platform_angle)
av = aov(External_platform_angle ~ Platform)
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
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  scale_y_log10() +
  ylab("Góc ghè trong (o)") + 
  xlab("") +
  ylim(40, 150) +
  ggtitle("Lớp văn hóa II ") +
  labs(fill = "")

# Anova
str(lop2)
attach(lop2)
lop2$External_platform_angle <- as.numeric(lop2$External_platform_angle)
av = aov(External_platform_angle ~ Platform)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)
# Ngay 4-7-2024



#-------------------------------------------------------------
# DIMENSIONS for Cortical flakes (SECONDARY FLAKES) (Not 100%)
# For Mass
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Tongso <- read_excel("B_Retouchedtools.xlsx", sheet = 'TiengViet')
str(Tongso)
attach(Tongso)
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


# For Length
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Cortexflake <- read_excel("flake.xlsx", sheet = 'Cortex_flake')
str(Cortexflake)
View(Cortexflake)
attach(Cortexflake)
Cortexflake$length <- as.numeric(Cortexflake$length)
ggplot(Cortexflake, aes(x=reorder(material, length),
                        y = length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Length (mm)") +
  scale_y_log10() +
  ggtitle("")


# Anova of States with Length
str(Cortexflake)
attach(Cortexflake)
av = aov(length ~ material)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# For Width
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Cortexflake <- read_excel("flake.xlsx", sheet = 'Cortex_flake')
str(Cortexflake)
attach(Cortexflake)
Cortexflake$width <- as.numeric(Cortexflake$width)
ggplot(Cortexflake, aes(x=reorder(material, width),
                        y = width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Width (mm)") +
  scale_y_log10() +
  ggtitle("")


# Anova of States with Width
str(Cortexflake)
attach(Cortexflake)
av = aov(width ~ material)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# For Thickness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Cortexflake <- read_excel("flake.xlsx", sheet = 'Cortex_flake')
str(Cortexflake)
attach(Cortexflake)
Cortexflake$thickness <- as.numeric(Cortexflake$thickness)
ggplot(Cortexflake, aes(x=reorder(material, thickness),
                        y = thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Thickness (mm)") +
  scale_y_log10() +
  ggtitle("")


# Anova of States with Thickness
str(Cortexflake)
attach(Cortexflake)
av = aov(thickness ~ material)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)



#-------------------------------------------------------------
# DIMENSIONS for Non-cortical flakes (TERRITARY FLAKES) (Not 100%)
# For Mass
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Non_cortex <- read_excel("flake.xlsx", sheet = 'Noncortex')
str(Non_cortex)
View(Non_cortex)
attach(Non_cortex)
Non_cortex$mass <- as.numeric(Non_cortex$mass)
ggplot(Non_cortex, aes(x=reorder(material, mass),
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
str(Non_cortex)
attach(Non_cortex)
av = aov(mass ~ material)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# For Length
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Non_cortex <- read_excel("flake.xlsx", sheet = 'Noncortex')
str(Non_cortex)
View(Non_cortex)
attach(Non_cortex)
Non_cortex$length <- as.numeric(Non_cortex$length)
ggplot(Non_cortex, aes(x=reorder(material, length),
                       y = length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Length(mm)") +
  scale_y_log10() +
  ggtitle("")


# Anova of States with Length
str(Non_cortex)
attach(Non_cortex)
av = aov(length ~ material)
summary(av)
av
TukeyHSD(av)
unit = TukeyHSD(av)
plot(unit)


# For Width
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Non_cortex <- read_excel("flake.xlsx", sheet = 'Noncortex')
str(Non_cortex)
View(Non_cortex)
attach(Non_cortex)
Non_cortex$width <- as.numeric(Non_cortex$width)
mean(width)
ggplot(Non_cortex, aes(x=reorder(material, width),
                       y = width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Width(mm)") +
  scale_y_log10() +
  ggtitle("")


# Anova of States with Width
str(Non_cortex)
attach(Non_cortex)
av = aov(width ~ material)
summary(av)
av
TukeyHSD(av)
unit = TukeyHSD(av)
plot(unit)



# For Thickness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Non_cortex <- read_excel("flake.xlsx", sheet = 'Noncortex')
str(Non_cortex)
View(Non_cortex)
attach(Non_cortex)
Non_cortex$thickness <- as.numeric(Non_cortex$thickness)
ggplot(Non_cortex, aes(x=reorder(material, thickness),
                       y = thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab(" Thickness(mm)") +
  scale_y_log10() +
  ggtitle("")

# Anova of States with Thickness
str(Non_cortex)
attach(Non_cortex)
av = aov(thickness ~ material)
summary(av)
av
TukeyHSD(av)
unit = TukeyHSD(av)
plot(unit)



##------------------------------------------------------------------
# Types of STRIKING PLATFORMS
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
View(Platform)
str(Platform)
Platform$mass <- as.numeric(Platform$mass)
Platform$max_dimension <- as.numeric(Platform$max_dimension)
Platform$length <- as.numeric(Platform$length)
Platform$width <- as.numeric(Platform$width)
Platform$thickness <- as.numeric(Platform$thickness)
Platform$platform_width <- as.numeric(Platform$platform_width)
Platform$platform_thickness <- as.numeric(Platform$platform_thickness)
Platform$external_platform_angle <- as.numeric(Platform$external_platform_angle)
Platform$internal_platform_angle<- as.numeric(Platform$internal_platform_angle)
Platform$dorsal_cortex_percentage <- as.numeric(Platform$dorsal_cortex_percentage)
Platform$number_of_dorsal_scars<- as.numeric(Platform$number_of_dorsal_scars)
names(Platform)
Platform %>% 
  group_by(`platform_types`) %>%
  tally()
Platform_platforms_tally <- Platform %>% 
  group_by(`platforms`) %>% 
  tally() %>% 
  filter(`platforms` != 'NA') %>% 
  arrange(desc(n))
ggplot(Platform_platforms_tally,
       aes(x = reorder(`platforms`, n), y = n)) +
  geom_bar(stat="identity",  fill = "black" , width = .6) + 
  theme_bw() +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Frequency") +
  ggtitle("Unit 1 and Unit 2")


# Types of platform according to Unit Excavation of Mau A 2015
library(readxl)
library(ggplot2)
library(dplyr)
library('RColorBrewer')
library(scales)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
View(Platform)
str(Platform)
names(Platform)
attach(Platform)
Platform
str(Platform)
ggplot(Platform, aes(x = reorder(unit, platforms), 
                     y = unit, fill = material)) +
  geom_bar(stat="identity",  fill = "black" , width = .6) + 
  facet_grid(~ unit) + 
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 20, angle = 0, hjust= 1),
        axis.text.x = element_text(size = 20, angle = 0, hjust= 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("Unit 1 (3-6) and Unit 2 (7-13)") +
  xlab("") +
  ylab("Frequency")



# Percentage of types of Platforms over time (Unit 1  + 2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
View(Platform)
str(Platform)
ggplot(Platform) +
  aes(x = material, fill = platforms) +
  geom_bar(position = "fill", width= 0.75, size = 0.5, na.rm = TRUE, color = "black") +
  facet_grid(~ unit) + 
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='', position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("Nguy?n li???u") +
  ylab("(%)") +
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)") +
  labs(fill = "Lo???i di???n gh?") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent)


# Types of Flake Initiation (Unit 1  + 2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
View(Platform)
str(Platform)
ggplot(Platform) +
  aes(x = material, fill = initiation) +
  geom_bar(position = "fill", width= 0.75, size = 0.5, na.rm = TRUE, color = "black") +
  facet_grid(~ unit) + 
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='', position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("Nguy?n li???u") +
  ylab("(%)") +
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)") +
  labs(fill = "H?nh d?ng kh???i d???u") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent)



# Platform Angles ==========================================================
## Distribution of "Internal Platform Angle"
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
str(Platform)
ggplot(Platform, aes(x = reorder(material, internal),
                     y = internal, fill = material)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust =0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("Internal Platform Angle") +
  xlab("") +
  ylab("(G?c gh? trong (D???)") +
  ylim(0, 120) +
  labs(fill = "Nguy?n li???u") + 
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)")



# Correlations between Internal/External Platform Angles
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
str(Platform)
ggplot(Platform, aes(x = reorder(material, external),
                     y = external, fill = material)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust =0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("Internal Platform Angle") +
  xlab("") +
  ylab("(G?c gh? ngo?i (D???)") +
  ylim(0, 120) +
  labs(fill = "Nguy?n li???u") + 
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)")



# Goc gh? ngo?i tuong ???ng v???i kh???i H?nh d?ng kh???i ph?t m???nh tu???c
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
str(Platform)
ggplot(Platform, aes(x = reorder(initiation, external),
                     y = external, fill = initiation)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust =0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("") +
  ylab("(G?c gh? ngo?i (D???)") +
  ylim(0, 120) +
  labs(fill = "Nguy?n li???u") + 
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)")


# Goc gh? trong tuong ???ng v???i kh???i H?nh d?ng kh???i ph?t m???nh tu???c
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
str(Platform)
ggplot(Platform, aes(x = reorder(initiation, internal),
                     y = internal, fill = initiation)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust =0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("") +
  ylab("(G?c gh? trong (D???)") +
  ylim(0, 120) +
  labs(fill = "Nguy?n li???u") + 
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)")



#==========================================================================
# Chi???u d?y v? chi???u r???ng di???n gh?
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
str(Platform)
ggplot(Platform, aes(x = reorder(initiation, platform_thickness),
                     y = platform_thickness, fill = initiation)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust =0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("") +
  ylab("(Chi???u d?y di???n gh? (mm)") +
  ylim(0, 15) +
  labs(fill = "H?nh th?i m???t b???ng khu v???c d???u m???nh tu???c") + 
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)")


# Chi???u r???ng di???n gh?
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
str(Platform)
ggplot(Platform, aes(x = reorder(initiation, platform_width),
                     y = platform_width, fill = initiation)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust =0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("") +
  ylab("(Chi???u r???ng di???n gh? (mm)") +
  ylim(0, 30) +
  labs(fill = "Lo???i di???n gh?") + 
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)")


# Tuong quan chi???u d?y v? r???ng di???n gh?
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
View(Platform)
str(Platform)
attach(Platform)
Platform$platform_width <- as.numeric(Platform$platform_width)
Platform$platform_thickness <- as.numeric(Platform$platform_thickness)
ggscatter(Platform, x = "platform_thickness", y = "platform_width",
          add = "reg.line", size =10,                   # Add regression line
          color = "platforms", palette = "platforms",   # Color by groups "cyl"
          shape = "",                                   # Change point shape by groups "cyl"
          fullrange = TRUE,                             # Extending the regression line
          rug = TRUE                                    # Add marginal rug
)+
  stat_cor(method = "pearson", label.x = 1, label.y = 100, size = 10) +
  geom_point(alpha = 1/10, size = 4) +
  geom_smooth(method = "lm") +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size = 40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust = 0.5)) +
  ylab("Chi???u d?y (mm)") +
  xlab("Chi???u r???ng (mm)") +
  xlim(0, 60) +
  ylim(0, 100) +
  labs(color = "Lo???i di???n gh?") +
  ggtitle("")


# Tuong quan chi???u d?y v? r???ng di???n gh? d???i v???i nguy?n li???u
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
View(Platform)
str(Platform)
attach(Platform)
Platform$platform_width <- as.numeric(Platform$platform_width)
Platform$platform_thickness <- as.numeric(Platform$platform_thickness)
ggscatter(Platform, x = "platform_thickness", y = "platform_width",
          add = "reg.line", size =10,                   # Add regression line
          color = "material", palette = "material",   # Color by groups "cyl"
          shape = "",                                   # Change point shape by groups "cyl"
          fullrange = TRUE,                             # Extending the regression line
          rug = TRUE                                    # Add marginal rug
)+
  stat_cor(method = "pearson", label.x = 1, label.y = 100, size = 10) +
  geom_point(alpha = 1/10, size = 4) +
  geom_smooth(method = "lm") +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size = 40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 60, angle = 0, hjust = 0.5)) +
  ylab("Chi???u d?y (mm)") +
  xlab("Chi???u r???ng (mm)") +
  xlim(0, 60) +
  ylim(0, 100) +
  labs(color = "Nguy?n li???u") +
  ggtitle("")


# Anova of States with Thickness - Unit 1
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
View(Platform)
str(Platform)
attach(Platform)
Platform$platform_thickness <- as.numeric(Platform$platform_thickness)
Platform$platform_width <- as.numeric(Platform$platform_width)
av = aov(platform_width ~ material)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



# To find out correlation between Internal and External Platform Angle of Flakes
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
Platform <- read_excel("flake.xlsx", sheet = 'Platforms')
View(Platform)
str(Platform)
attach(Platform)
Platform$external <- as.numeric(Platform$external)
Platform$internal <- as.numeric(Platform$internal)
ggscatter(Platform, x = "external", y = "internal",
          add = "reg.line", size =10,                   # Add regression line
          color = "platforms", palette = "platforms",   # Color by groups "cyl"
          shape = "",                                   # Change point shape by groups "cyl"
          fullrange = TRUE,                             # Extending the regression line
          rug = TRUE                                    # Add marginal rug
)+
  stat_cor(method = "pearson", label.x = 100, label.y = 160, size = 10) +
  geom_point(alpha = 1/10, size = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(legend.position = "right") +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 30, angle = 0, hjust = 0.5)) +
  ylab("G?c gh? trong (D???)") +
  xlab("G?c gh? ngo?i (D???)") +
  labs(fill = "Di???n gh?") 


#=======================================================================
# T??? l??? v??? cu???i lung m???nh tu???c tuong ???ng v???i NGUY?N LI???U/H?NH TH?I D???U
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
Cortex <- read_excel("flake.xlsx", sheet = 'Cortex_flake')
str(Cortex)
attach(Cortex)
Cortex$cortex <- as.numeric(Cortex$cortex)
ggplot(Cortex, aes(x = reorder(material, cortex),
                   y = cortex, fill = inititation)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust =0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("Nguyên liệu") +
  ylab("(Tỷ lệ vỏ cuội mặt lưng (%)") +
  ylim(0, 100) +
  labs(fill = "Dáng u ghe mặt bụng") + 
  ggtitle("Tầng I (lớp 3-6) v? Tầng II (lớp 7-13)")


# T??? l??? v??? cu???i lung m???nh tu???c tuong ???ng v???i NGUY?N LI???U/H?NH TH?I DU?I
Cortex <- read_excel("flake.xlsx", sheet = 'Cortex_flake')
str(Cortex)
attach(Cortex)
Cortex$cortex <- as.numeric(Cortex$cortex)
ggplot(Cortex, aes(x = reorder(material, cortex),
                   y = cortex, fill = termination)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  facet_grid(~ unit) + 
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust =0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("Nguy?n li???u") +
  ylab("(T??? l??? v??? cu???i m???t lung (%)") +
  ylim(0, 100) +
  labs(fill = "Du?i m???nh tu???c") + 
  ggtitle("T???ng I (l???p 3-6) v? T???ng II (l???p 7-13)")

#
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
Cortex <- read_excel("flake.xlsx", sheet = 'Cortex_flake')
str(Cortex)
attach(Cortex)
Cortex %>% 
  group_by(`cortex`) %>%
  tally()
Cortex_cortex_tally <- Cortex %>% 
  group_by(`cortex`) %>% 
  tally() %>% 
  filter(`cortex` != 'NA') %>% 
  arrange(desc(n))
ggplot(Cortex_cortex_tally,
       aes(x = reorder(`cortex`, n), 
           y = n, fill = unit)) +
  geom_bar(stat="identity", size = 1, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust = 0.5)) +
  xlab("Số lượng mảnh tước còn vỏ cuội từ 5% đến 100%") +
  ylab("Số lượng")


# Vị trí vỏ cuội
Cortex <- read_excel("flake.xlsx", sheet = 'Cortex_flake')
str(Cortex)
attach(Cortex)
Cortex %>% 
  group_by(`localization`) %>%
  tally()
Cortex_localization_tally <- Cortex %>% 
  group_by(`localization`) %>% 
  tally() %>% 
  filter(`localization` != 'NA') %>% 
  arrange(desc(n))
ggplot(Cortex_localization_tally,
       aes(x = reorder(`localization`, n), 
           y = n, fill = unit)) +
  geom_bar(stat="identity", size = 1, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust = 0.5)) +
  xlab("V??? tr? v??? cu???i tr?n lung") +
  ylab("S??? lu???ng") +
  coord_flip()


# H?nh th?i du?i c???a m???nh tu???c t???ng s??? 4610/4733
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
total <- read_excel("flakes_total.xlsx", sheet = 'Sheet1')
str(total)
attach(total)
total %>% 
  group_by(`termination`) %>%
  tally()
total_termination_tally <- total %>% 
  group_by(`termination`) %>% 
  tally() %>% 
  filter(`termination` != 'NA') %>% 
  arrange(desc(n))
ggplot(total_termination_tally,
       aes(x = reorder(`termination`, n), 
           y = n, fill = unit)) +
  geom_bar(stat="identity", width = 0.6, color="black", size = 1.3, fill="white") +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust = 0.5)) +
  xlab("Du?i m???nh tu???c") +
  ylab("S??? lu???ng")


# T??? l??? % du?i m???nh tu???c qua th???i gian
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
total <- read_excel("flakes_total.xlsx", sheet = 'Sheet1')
str(total)
attach(total)
ggplot(total) +
  aes(x = context, fill = termination) +
  geom_bar(position = "fill", width= 0.8, size = 0.8, color = "black", na.rm =FALSE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat = '', position=position_fill(vjust= 0.2)) +
  theme(aspect.ratio = 15/2) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="right") +
  xlab("L???p khai qu???t") +
  xlim(0, 13) +
  ylab("%") +
  ggtitle("T???ng 1 (2-6), T???ng 2 (7-13)") +
  labs(fill = "Raw material") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) +
  labs(fill = "Du?i m???nh tu???c")

# T??? l??? du?i m???nh tu???c v???i nguy?n li???u
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
total <- read_excel("flakes_total.xlsx", sheet = 'Sheet1')
str(total)
attach(total)
ggplot(total) +
  aes(x = material, fill = termination) +
  geom_bar(position = "fill", width= 0.8, size = 0.8, color = "black", na.rm =FALSE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat = 'count', angle = 90, size = 10, position=position_fill(vjust= 0.)) +
  facet_grid(~ unit) + 
  theme(aspect.ratio = 15/2) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 40, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 35, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("Nguy?n li???u") +
  ylab("(%)") +
  ggtitle("T???ng 1 (2-6), T???ng 2 (7-13)") +
  labs(fill = "Raw material") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) +
  labs(fill = "Du?i m???nh tu???c") +
  coord_flip()


# T??? l??? du?i m???nh tu???c v???i h?nh th?i d???u
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
total <- read_excel("flakes_total.xlsx", sheet = 'Sheet1')
str(total)
attach(total)
ggplot(total) +
  aes(x = inititation, fill = termination) +
  geom_bar(position = "fill", width= 0.8, size = 0.8, color = "black", na.rm = TRUE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat = 'count', size = 10, angle = 90, position=position_fill(vjust= 0.2)) +
  facet_grid(~ unit) + 
  theme(aspect.ratio = 15/2) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 40, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 35, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("H?nh d?ng u gh? m???t b???ng") +
  ylab("(%)") +
  ggtitle("T???ng 1 (2-6), T???ng 2 (7-13)") +
  labs(fill = "Raw material") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) +
  labs(fill = "Du?i m???nh tu???c") +
  coord_flip()

#=======================================================================
# CU???NG D??? GH? D???O
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
total <- read_excel("flakes_total.xlsx", sheet = 'Sheet1')
str(total)
attach(total)
total %>% 
  group_by(`scars`) %>%
  tally()
total_scars_tally <- total %>% 
  group_by(`scars`) %>% 
  tally() %>% 
  filter(`scars` != 'NA') %>% 
  arrange(desc(n))
ggplot(total_scars_tally,
       aes(x = reorder(`scars`, n), 
           y = n, fill = unit)) +
  geom_bar(stat="identity", size = 1, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust = 0.5)) +
  xlab("S??? nh?t gh? tr?n m???t lung m???nh tu???c") +
  ylab("S??? lu???ng")

# S??? nh?t gh? v? nguy?n li???u
total <- read_excel("flakes_total.xlsx", sheet = 'Sheet1')
str(total)
attach(total)
ggplot(total) +
  aes(x = inititation, fill = termination) +
  geom_bar(position = "fill", width= 0.8, size = 0.8, color = "black", na.rm = TRUE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat = 'count', size = 10, angle = 90, position=position_fill(vjust= 0.2)) +
  facet_grid(~ unit) + 
  theme(aspect.ratio = 15/2) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 40, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 35, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("H?nh d?ng u gh? m???t b???ng") +
  ylab("(%)") +
  ggtitle("T???ng 1 (2-6), T???ng 2 (7-13)") +
  labs(fill = "Raw material") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) +
  labs(fill = "Du?i m???nh tu???c") +
  coord_flip()



# Mean of Dorsal Scars per Each Flake
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
total_t <- read_excel("flakes_total.xlsx", sheet = 'Sheet2')
str(total_t)
attach(total_t)
total_t$scars <- as.numeric(total_t$scars)
total_t$percentage <- as.numeric(total_t$percentage)
ggplot(total_t, aes(x = reorder(localization, scars,
                                FUN = mean), y = scars)) + 
  geom_point(stat="summary", fun.y="mean", size = 4) + 
  geom_errorbar(stat="summary", fun.data="mean_se", 
                fun.args = list(mult = 3), width=0) +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust= .5)) +
  ggtitle("") +
  ylim(0,7) +
  xlab("V??? tr? v??? cu???i") +
  ylab("Trung b?nh s??? nh?t gh?") +
  coord_flip()

# Cuu???ng d??? gh? d???o v???i d???u m???nh tu???c
total_t <- read_excel("flakes_total.xlsx", sheet = 'Sheet2')
str(total_t)
attach(total_t)
total_t$scars <- as.numeric(total_t$scars)
total_t$percentage <- as.numeric(total_t$percentage)
ggplot(total_t, aes(x = reorder(initiation, scars,
                                FUN = mean), y = scars)) + 
  geom_point(stat="summary", fun.y="mean", size = 4) + 
  geom_errorbar(stat="summary", fun.data="mean_se", 
                fun.args = list(mult = 3), width=0.6) +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust= .5)) +
  ggtitle("") +
  ylim(0,10) +
  xlab("H?nh d?ng u gh? ??? m???t b???ng") +
  ylab("Trung b?nh s??? nh?t gh?") +
  coord_flip()

#
str(total_t)
attach(total_t)
av = aov(scars ~ initiation)
summary(av)
av
TukeyHSD(av)
unit = TukeyHSD(av)
plot(unit)



# S??? v???t gh? ???ng v???i nguy?n li???u
total_t <- read_excel("flakes_total.xlsx", sheet = 'Sheet2')
str(total_t)
attach(total_t)
total_t$scars <- as.numeric(total_t$scars)
total_t$percentage <- as.numeric(total_t$percentage)
ggplot(total_t, aes(x = reorder(material, scars,
                                FUN = mean), y = scars)) + 
  geom_point(stat="summary", fun.y="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", 
                fun.args = list(mult = 1.96), width=0) +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust= .5)) +
  ggtitle("") +
  ylim(0,7) +
  xlab("V??? tr? v??? cu???i m???t") +
  ylab("Trung b?nh s??? nh?t gh?")

# Ki???m d???nh t?m hi???u s??? kh?c nhau v??? s??? v???t gh? ???ng v???i nguy?n li???u
str(total_t)
attach(total_t)
av = aov(scars ~ material)
summary(av)
av
TukeyHSD(av)
unit = TukeyHSD(av)
plot(unit)

#
total_t <- read_excel("flakes_total.xlsx", sheet = 'Sheet2')
str(total_t)
attach(total_t)
total_t$scars <- as.numeric(total_t$scars)
total_t$percentage <- as.numeric(total_t$percentage)
ggplot(total_t, aes(x = reorder(initiation, scars,
                                FUN = mean), y = scars)) + 
  geom_point(stat="summary", fun.y="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", 
                fun.args = list(mult = 1.96), width=0) +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust= .5)) +
  ggtitle("") +
  ylim(0,7) +
  xlab("V??? tr? v??? cu???i m???t") +
  ylab("Trung b?nh s??? nh?t gh?")


# S??? v???t gh? tr?n m???i t?nh tr???ng m???nh tu???c
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
total_t <- read_excel("flakes_total.xlsx", sheet = 'Sheet2')
str(total_t)
attach(total_t)
ggplot(total_t) +
  aes(x = scars, y = localization) +
  geom_bar(position = "fill", width= 0.8, size = 1, na.rm = FALSE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle= 90, hjust=.5)) +
  theme(legend.position="right") +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Unit 1 (3-6) and Unit 2 (7-13)") +
  labs(fill = "Termination") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) 


#
library(ggplot2)
library(dplyr)
library(devtools)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
library(shiny)
total_t <- read_excel("flakes_total.xlsx", sheet = 'Sheet2')
str(total_t)
attach(total_t)
total_t %>% 
  group_by(`scars`) %>%
  tally()
total_t_scars_tally <- total_t %>% 
  group_by(`scars`) %>% 
  tally() %>% 
  filter(`scars` != 'NA') %>% 
  arrange(desc(n))
ggplot(total_t_scars_tally,
       aes(x = reorder(`scars`, n), 
           y = n, fill = unit)) +
  geom_bar(stat="identity", size = 1, color="black", fill="white") +
  theme_bw() +
  theme(text = element_text(size=40),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 40, angle = 0, hjust = 0.5)) +
  xlab("S??? nh?t gh? tr?n m???t lung m???nh tu???c") +
  ylab("S??? lu???ng")







