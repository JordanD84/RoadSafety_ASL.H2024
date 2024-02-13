# Land use data: https://donnees.montreal.ca/dataset/schema-affectation-densite

mat = st_intersects(data_sf, land_use, sparse = FALSE)
rownames(mat) <- data$int_no
colnames(mat) <- land_use$AFFECTATIO

aug_land_use <- 
  as_tibble(mat, .name_repair = "universal", rownames = NA) |> 
  rownames_to_column("int_no") |> 
  pivot_longer(!int_no) |> 
  mutate(name = 
           str_replace(name, "...\\d+", "") |> 
           str_replace_all("\\.", "_") |> str_to_lower()) |>
  group_by(int_no, name) |> 
  summarise(value = sum(value), .groups = "drop") |> 
  filter(value >= 1) |> 
  pivot_wider(names_from = name, values_from = value, values_fill = 0) |> 
  right_join(select(data, int_no), by = "int_no") |> 
  mutate(across(everything(), ~replace_na(., 0))) |> 
  pivot_longer(!int_no) |> 
  filter(value == 1) |>  
  # intersections only have one land use (checked)
  # However, 4 intersections missing
  select(int_no, land_use = name)

# Combine grand_espace_vert_ou_récréation and conservation with dominante_résidentielle
# grande_emprise_ou_grande_infrastructure_publique with industrie 
# NA with something