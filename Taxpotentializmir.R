# Gerekli k??t??phaneler
library(readxl)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(scales)

# Veriyi y??kleme
Mdata <- read_excel("C:/Users/Bilgisayarim/Desktop/mdata.xlsx")
Rdata <- read_excel("C:/Users/Bilgisayarim/Desktop/rayicdata.xlsx")

  
# Toplam m?? ve Ortalama m?? Fiyat?? hesaplama
Mmeans_by_district <- Mdata %>%
  group_by(district) %>%
  summarise(Mean_M2Price = mean(price / m2, na.rm = TRUE)
)
# ??zmir i??in t??m il??elerin ortalamas??n?? ekle
Maverage_izmir <- Mmeans_by_district %>%
  summarise(district = "??zmir", 
            Mean_M2Price = mean(Mean_M2Price, na.rm = TRUE))
Mmeans_by_district <- bind_rows(Mmeans_by_district, Maverage_izmir)


Rmeans_by_district <- Rdata %>%
  group_by(district) %>%
  summarise(Mean_R2Price = mean(rayic, na.rm = TRUE)
  )
Raverage_izmir <- Rmeans_by_district %>%
  summarise(district = "??zmir", 
            Mean_R2Price = mean(Mean_R2Price, na.rm = TRUE))
Rmeans_by_district <- bind_rows(Rmeans_by_district, Raverage_izmir)


Total_m2_by_district <- Mdata %>%
  group_by(district) %>%
  summarise(Total_M2 = sum(m2, na.rm = TRUE))

Totalm2 <- Total_m2_by_district %>%
  summarise(district = "??zmir", 
            Total_M2 = sum(Total_M2, na.rm = TRUE))
Total_m2_by_district <- bind_rows(Total_m2_by_district, Totalm2)

combined_data <- Mmeans_by_district %>%
  left_join(Rmeans_by_district, by = "district") %>%
  left_join(Total_m2_by_district, by = "district")
combined_data <- combined_data %>%
  mutate(Tax_Potential_Difference = (Mean_M2Price - Mean_R2Price) * Total_M2 * 0.002)

# Mmeans_by_district tablosu i??in g-------------------------------------------------------------------------
Mmeans_by_district %>%
  arrange(desc(Mean_M2Price)) %>%  # B??y??kten k????????e s??ralama
  kable("html", col.names = c("District", "Mean M2 Price")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


# Rmeans_by_district tablosu ------------------------------------------------------------------------------
Rmeans_by_district %>%
  arrange(desc(Mean_R2Price)) %>%  # B??y??kten k????????e s??ralama
  kable("html", col.names = c("District", "Mean Rayic Price")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
# Grafik: ??l??elere g??re Mean_R2Price (??st??nde de??erler ile)
ggplot(
  Rmeans_by_district %>%
    filter(district != "??zmir") %>%
    arrange(Mean_R2Price) %>%
    mutate(district = factor(district, levels = unique(district))),
  aes(x = district, y = Mean_R2Price, fill = district)
) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = scales::comma(Mean_R2Price, accuracy = 1)), 
    vjust = -0.5, 
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = "Mean Current Price by District",
    x = "",
    y = ""
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
# ??zmir i??in Ratio_M2_R2'yi tabloya ekle
izmir_markettimes <- combined_data %>%
  filter(district == "??zmir") %>%
  mutate(Ratio_M2_R2 = Mean_M2Price / Mean_R2Price)

# Tabloyu g??rselle??tir
izmir_markettimes %>%
  select(district, Mean_M2Price, Mean_R2Price, Ratio_M2_R2) %>%
  kable("html", col.names = c("District", "Mean M2 Price", "Mean R2 Price", "Ratio M2/R2")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


# Tax_Potential_Difference i??in --------------------------------------------------------------------------
combined_data %>%
  select(district, Tax_Potential_Difference) %>%
  arrange(desc(Tax_Potential_Difference)) %>%  # B??y??kten k????????e s??ralama
  kable("html", col.names = c("District", "Tax Potential Difference")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


# Tax_Potential_Difference i??in ------------------------------------------------------------------------------
ggplot(
  combined_data %>%
    filter(district != "??zmir") %>%
    arrange(Tax_Potential_Difference) %>%
    mutate(district = factor(district, levels = unique(district))),
  aes(x = district, y = Tax_Potential_Difference / 1000000, fill = district)
) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Tax Potential Difference by District (In Millions TL)", 
       x = "", 
       y = "") +
  scale_y_continuous(labels = scales::comma_format(scale = 1), 
                     breaks = seq(0, max(combined_data$Tax_Potential_Difference / 1000000, na.rm = TRUE), by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# ??zmir i??in Tax Potential Difference'?? tabloya ekle
Total_tax_potential <- combined_data %>%
  filter(district == "??zmir") %>%
  select(Tax_Potential_Difference)

# Tabloyu g??rselle??tir
Total_tax_potential %>%
  kable("html", col.names = c("Total Tax Potential Difference")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


# Her il??enin en y??ksek %30'luk dilimini se??me
Mdata_top_30_percent <- Mdata %>%
  mutate(M2Price = price / m2) %>%   # Metrekare fiyat?? hesapla
  group_by(district) %>%            # ??l??elere g??re grupland??r
  arrange(desc(M2Price), .by_group = TRUE) %>%  # Her il??eyi kendi i??inde s??rala
  slice(1:ceiling(0.3 * n()))       # Her il??eden en y??ksek %30'u se??

# Se??ilen %30'luk il??elerin m2 toplam??n?? hesapla
Mdata_top_30_percent_summary <- Mdata_top_30_percent %>%
  summarise(
    Total_M2 = sum(m2, na.rm = TRUE),            # m2'lerin toplam??n?? hesa
  )




# Her il??enin en y??ksek %30'u i??in ortalama metrekare fiyat??n?? hesaplama
Mdata_top_30_summary <- Mdata_top_30_percent %>%
  group_by(district) %>%
  summarise(Mean_Top30_M2Price = mean(M2Price, na.rm = TRUE))
mean_top30_m2price <- Mdata_top_30_summary %>%
  summarise(Overall_Mean_Top30_M2Price = mean(Mean_Top30_M2Price, na.rm = TRUE))
mean_top30_m2price
# ??l??elere g??re g??rselle??tirme
ggplot(
  Mdata_top_30_summary %>%
    mutate(district = factor(district, levels = unique(district))),
  aes(x = district, y = Mean_Top30_M2Price, fill = district)
) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = scales::comma(Mean_Top30_M2Price, accuracy = 1)), 
    vjust = -0.5, 
    size = 2  # Yaz?? boyutunu k??????ltme
  ) +
  theme_minimal() +
  labs(title = "Top 30% Mean M2 Price by District", 
       x = "", 
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

mean_top30_m2price %>%
  kable("html", col.names = c("Overall Mean Top 30 M2 Price")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

Mdata_top_30_taxpotential <- Mdata_top_30_summary %>%
  left_join(Mdata_top_30_percent_summary, by = "district") %>%
  mutate(
    Value_Top30 = (( Mean_Top30_M2Price - Mean_R2Price ) * Total_M2*0.002)  # Ortalama metrekare fiyat??n?? m2 toplam?? ile ??arpma
  )

# Sonucu g??rselle??tirme: Tablo olu??turma
Mdata_top_30_taxpotential %>%
  kable("html", col.names = c("District", "Mean Top 30 M2 Price", "Total M2", "Value Top 30")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
# Value_Top30 de??erlerinin toplam??n?? hesapla
total_value_top_30 <- Mdata_top_30_taxpotential %>%
  summarise(
    Overall_Value_Top30 = sum(Value_Top30, na.rm = TRUE)  # Toplam de??eri hesapla
  )

# Overall taxpotential top30 g??rselle??tirme
total_value_top_30 %>%
  kable("html", col.names = c("Overall Value Top 30")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

