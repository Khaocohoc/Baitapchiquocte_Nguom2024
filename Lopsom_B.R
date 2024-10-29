# Ngay 01-07-2024
# Su phan bo nguyen lieu cua manh tuoc qua thoi gian

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
View(lop2)
str(lop2)
lop2$Mass <- as.numeric(lop2$Mass)
lop2$zone13t <- as.numeric(lop2$zone13t)
lop2$zne14t <- as.numeric(lop2$zone13t)
median(lop2$zone13t, na.rm = TRUE)
IQR(lop2$zone13t, na.rm = TRUE)
names(lop2)
lop2 %>% 
  group_by(`Material`) %>%
  tally()
lop2_Material_tally <-lop2 %>% 
  group_by(`Material`) %>% 
  tally() %>% 
  filter(`Material` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop2_Material_tally,
       aes(x = reorder(`Material`, n), y = n)) +
  geom_bar(stat="identity", width = 0.8, size = 0.6, fill ="white", color = "black") +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Frequency") +
  ggtitle("Unit 1 and Unit 2") +
  coord_flip()

### Computing "Median and Interquartile range" Lop muon
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
View(lop2)
str(lop2)
median(lop2$Mass, na.rm = TRUE)
IQR(lop2$Mass, na.rm = TRUE)

median(lop2$Max_dimension, na.rm = TRUE)
IQR(lop2$Max_dimension, na.rm = TRUE)

median(lop2$Length, na.rm = TRUE)
IQR(lop2$Length, na.rm = TRUE)

median(lop2$Width, na.rm = TRUE)
IQR(lop2$Width, na.rm = TRUE)

median(lop2$Thickness, na.rm = TRUE)
IQR(lop2$Thickness, na.rm = TRUE)

median(lop2$Platform_width, na.rm = TRUE)
IQR(lop2$Platform_width, na.rm = TRUE)

median(lop2$Platform_thickness, na.rm = TRUE)
IQR(lop2$Platform_thickness, na.rm = TRUE)

median(lop2$External_platform_angle, na.rm = TRUE)
IQR(lop2$External_platform_angle, na.rm = TRUE)

median(lop2$Internal_platform_angle, na.rm = TRUE)
IQR(lop2$Internal_platform_angle, na.rm = TRUE)

median(lop2$Tylevocuoi, na.rm = TRUE)
IQR(lop2$Tylevocuoi, na.rm = TRUE)

median(lop2$Sovetghe_dienghe, na.rm = TRUE)
IQR(lop2$Sovetghe_dienghe, na.rm = TRUE)

median(lop2$Sovetghe_lung, na.rm = TRUE)
IQR(lop2$Sovetghe_lung, na.rm = TRUE)

median(lop2$Sovetghe_bung, na.rm = TRUE)
IQR(lop2$Sovetghe_bung, na.rm = TRUE)

median(lop2$zone1t, na.rm = TRUE)
IQR(lop2$zone1t, na.rm = TRUE)

median(lop2$zone2t, na.rm = TRUE)
IQR(lop2$zone2t, na.rm = TRUE)

median(lop2$zone3t, na.rm = TRUE)
IQR(lop2$zone3t, na.rm = TRUE)

median(lop2$zone4t, na.rm = TRUE)
IQR(lop2$zone4t, na.rm = TRUE)

median(lop2$zone5t, na.rm = TRUE)
IQR(lop2$zone5t, na.rm = TRUE)

median(lop2$zone6t, na.rm = TRUE)
IQR(lop2$zone6t, na.rm = TRUE)

median(lop2$zone7t, na.rm = TRUE)
IQR(lop2$zone7t, na.rm = TRUE)

median(lop2$zone8t, na.rm = TRUE)
IQR(lop2$zone8t, na.rm = TRUE)

median(lop2$zone9t, na.rm = TRUE)
IQR(lop2$zone9t, na.rm = TRUE)

median(lop2$zone10t, na.rm = TRUE)
IQR(lop2$zone10t, na.rm = TRUE)

median(lop2$zone11t, na.rm = TRUE)
IQR(lop2$zone11t, na.rm = TRUE)

median(lop2$zone12t, na.rm = TRUE)
IQR(lop2$zone12t, na.rm = TRUE)

median(lop2$zone13t, na.rm = TRUE)
IQR(lop2$zone13t, na.rm = TRUE)

median(lop2$zone14t, na.rm = TRUE)
IQR(lop2$zone14t, na.rm = TRUE)

median(lop1$zone15t, na.rm = TRUE)
IQR(lop1$zone15t, na.rm = TRUE)

median(lop2$zone16t, na.rm = TRUE)
IQR(lop2$zone16t, na.rm = TRUE)

## Ngay 02-07-2024 -  LOI CODE- chua cahy
# Kiem tra su khac biet trong luong
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
attach(lop2)
lop2
str(lop2)
ggplot(lop2, aes(x=Material, y=Context)) +
  geom_boxplot(size = 0.6, width = 0.3, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Mass (g)") +
  ggtitle("Unit 1 and Unit 2") +
  coord_flip()
#
lop2$Mass <- as.numeric(lop2$Mass)
median(lop2$Mass, na.rm  = TRUE)
t.test(lop2$Mass, na.rm  = TRUE)
sd(lop2$Mass, na.rm  = TRUE)
mean(flake$Mass, na.rm  = TRUE)
IQR(lop2$Mass, na.rm  = TRUE)


###--------------------------------------------------------------------------------
# Ty le % theo tinh trang ung với lop khi quat 41.5ka-34.1ka
 
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
lop2
str(lop2)
ggplot(lop2) +
  aes(x = Context, fill = State) +
  geom_bar(position = "fill", width=0.8, size = 0.9, color = "black") +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', size = 7, position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 9/10) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=0.3, color = "black"),
        axis.text.x = element_text(size = 25, angle= 0, vjust=0.5, color = "black")) +
  theme(legend.position="bottom") +
  xlab("Lớp văn hóa II (L11-15) năm 2017") +
  ylab("%") +
  ggtitle("") +
  labs(fill = "Tình trạng") # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"


# Tinh trang vA LENGTH
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
attach(lop2)
lop2$Length <- as.numeric(lop2$Length)
ggplot(lop2, aes(x=reorder(State, Length),
                     y = Length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Chiều dài (mm)") +
  scale_y_log10() +
  ggtitle("Lớp văn hóa II (L11-L15)") +
  coord_flip()

# Anlop2# Anova of States with Length
str(lop2)
attach(lop2)
av = aov(Length ~ State)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)



# Tinh trang va Width
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
attach(lop2)
lop2$Width <- as.numeric(lop2$Width)
lop2 <- lop2 %>% drop_na(State)
ggplot(lop2, aes(x=reorder(State, Width),
                 y = Width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 22, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 22, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Chiều rộng (mm)") +
  scale_y_log10() +
  ggtitle("Lớp văn hóa II (L11-L15)") +
  coord_flip()


# Anlop2# Anova of States with Length- Unit 1
str(lop2)
attach(lop2)
w = aov(Width ~ State)
summary(w)
w
TukeyHSD(w)
unit1 = TukeyHSD(w)
plot(unit1)


# Tinh trang vA Thickness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
attach(lop2)
lop2$Thickness <- as.numeric(lop2$Thickness)
ggplot(lop2, aes(x=reorder(State, Thickness),
                 y = Thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()

# Anlop2# Anova of States with Length- Unit 1
str(lop2)
attach(lop2)
T1 = aov(Thickness ~ State)
summary(T1)
T1
TukeyHSD(T1)
unit1 = TukeyHSD(T1)
plot(unit1)


# Tinh trang vA Platform_width
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
attach(lop2)
lop2$Platform_width <- as.numeric(lop2$Platform_width)
lop2 <- lop2 %>% drop_na(State)
ggplot(lop2, aes(x=reorder(State, Platform_width),
                 y = Platform_width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 22, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 22, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Rộng diện ghè (mm)") +
  scale_y_log10() +
  ggtitle("Lớp văn hóa II (L11-L15)") +
  coord_flip()
  

# Anova cua Platform_width
str(lop2)
attach(lop2)
T2 = aov(Platform_width ~ State)
summary(T2)
T2
TukeyHSD(T2)
unit2 = TukeyHSD(T2)
plot(unit2)


# Tinh trang vA Platform_thickness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
attach(lop2)
lop2$Platform_thickness <- as.numeric(lop2$Platform_thickness)
ggplot(lop2, aes(x=reorder(State, Platform_thickness),
                 y = Platform_thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()

# Anova cua Platform_thickness
str(lop2)
attach(lop2)
T2 = aov(Platform_thickness ~ State)
summary(T2)
T2
TukeyHSD(T2)
unit2 = TukeyHSD(T2)
plot(unit2)
# Tinh trang va Mass
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
str(lop2)
attach(lop2)
lop2$Mass <- as.numeric(lop2$Mass)
ggplot(lop2, aes(x=reorder(State, Mass),
                 y = Mass)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()

# Anlop2# Anova of States with Length- Unit 1
str(lop2)
attach(lop2)
w3 = aov(Mass ~ State)
summary(w3)
w3
TukeyHSD(w3)
unit1 = TukeyHSD(w3)
plot(unit1)


# HINH THAI DAU VA DUOI TUONG UNG VOI LOAI HINH NGUYEN LIEU 41.5ka-34.1ka
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop2 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang41_35ka')
attach(lop2)
ggplot(lop2) +
  aes(x = Flake_initiation, fill = Material) +
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



# HE SO TUONG QUAN
# Mass/Max_dimension
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
lop2$Max_dimension <- as.numeric(lop2$Max_dimension)
ggscatter(lop2 , x = "Max_dimension", y = "Mass",
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
  ggtitle("Unit 1") +
  xlab("Max dimension (mm)") +
  ylab("Mass (g)")


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
  geom_bar(stat="identity", size = 1, 
           fill = "white", color = "black", 
           width = 0.8) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Frequency") +
  coord_flip()


# Distribution of Platform Width/Types of Striking Platforms - ca hai tang van hoa I va II
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
ggplot(lop2 , aes(x = reorder(Platform_types, Platform_width),
                  y = Platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ State) +
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


# Loai hinh dien ghe va chieu day tang van hoa som  - 2
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
ggplot(lop2 , aes(x = reorder(Platform_types, Platform_thickness),
                  y = Platform_thickness)) + 
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
  