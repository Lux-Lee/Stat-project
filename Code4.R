Health_Data  <- bind_rows(
  mutate(Indigenous_Health, Group = "Indigenous"),
  mutate(Canadian_Health, Group = "Canadian")
)

Perceived <- Health_Data %>%                          ## Data formatting and factoring
  filter(Gender != "Both") %>%
  mutate(
    Percentage = Percentage / 100,
    Year = factor(Year),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous")),
    Category = factor(Category, levels = c("Fair+Poor", "Good", "Excellent+Very good"), ordered = TRUE)
  )
