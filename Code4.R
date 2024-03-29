library(readxl)
Canadian_Health <- read_excel("Proj_data.xlsx", sheet = "Canadian Perceived Health")
Indigenous_Health <- read_excel("Proj_data.xlsx", sheet = "Indigenous Perceived Health")

Health_Data  <- bind_rows(
  mutate(Indigenous_Health, Group = "Indigenous"),
  mutate(Canadian_Health, Group = "Canadian")
)

Perceived <- Health_Data %>%                         
  filter(Gender != "Both") %>%
  mutate(
    Percentage = Percentage / 100,
    Year = factor(Year),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous")),
    Category = factor(Category, levels = c("Fair+Poor", "Good", "Excellent+Very good"), ordered = TRUE)
  )

longData <- Perceived %>%                      
  group_by(Year, Gender, Category, Group) %>%
  summarize(1 ,response = rep(1, Count)) %>%
  ungroup()

model1 <- multinom(Category ~ Gender * Group, data = longData)
summary(model1)
modelReduced <- multinom(Category ~ Gender + Group, data = longData)