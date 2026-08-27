
## Background

Biodiversity underpins ecosystem processes and services, described in policy terms as nature's contributions to people (NCP). Agricultural land conversion is a leading driver of biodiversity loss, yet how it affects multiple NCPs via direct and cascading indirect effects remains poorly understood. Here, we develop a network-based approach to quantify direct and indirect effects of land conversion on multiple NCPs, using an empirical multi-interaction network from an organic farm in Southwest England. Simulating a sequential transition from extensive organic to intensive non-organic management, we assess how species and interaction loss shape seven NCPs. Cascading indirect effects were the primary drivers of inferred NCP change, with indirect losses (97\%) three-fold greater than direct losses (31\%) across the gradient. Effects varied among NCPs: pollination declined by nearly 95\%, while total crop biomass rose by up to 191\%, revealing a trade-off between production and other benefits such as pollination and cultural services. Null model analyses show that NCP losses are driven disproportionately by species identity rather than richness alone. These findings demonstrate that indirect species interactions are central to sustaining multifunctional agroecosystems, and that their erosion under land conversion amplifies trade-offs among NCPs, underscoring the need to incorporate community structure into land management strategies.

---

## 📁 Repository structure

```
├── 🧑‍💻 R/
│   ├── functions.R
│   ├── 1_Abundances.R
│   ├── 2_Multi_object.R
│   ├── 3_Biomass.R
│   ├── 4_Land_conversion_simulation.R
│   ├── 5_Null_model.R
│   ├── 6_Sps_indirect_contribution.R
│   ├── 7_Graph_networks.R
│   └── 8_Graphs.R
├── 📊 Data/       Processed outputs, raw data (Raw_data/), and external repository data (Repositories/)
└── 🖼️ Graphs/     Every figure produced by the project
```

> ℹ️ Data files are not detailed here — see the [Data wiki page](https://github.com/Ecological-Complexity-Lab/Norwood_farm/wiki/Data) for the complete list.

---
## 🧰 Utility

| Script           | What it does                                                                                                                          |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| ⚙️ `functions.R` | Shared functions used across more than one analysis script. Functions are grouped by purpose, each with a short explanation above it. |

---

## 🔄 Data processing

*Run in order — each step feeds into the next.*

| Script             | What it does                                                                                                            |
| ------------------ | ----------------------------------------------------------------------------------------------------------------------- |
| `1_Abundances.R`   | Estimates species abundances and builds the species and interaction lists from raw field data.                          |
| `2_Multi_object.R` | Organizes everything into a multilayer network object: nodes, layers (habitats), interactions, and species-per-habitat. |
| `3_Biomass.R`      | Assigns biomass to species using databases and literature (see Appendix S1 for sources).                                |

> ℹ️ Inputs and outputs for each script are not listed here — see the [wiki](https://github.com/Ecological-Complexity-Lab/Norwood_farm/wiki) for full I/O details, and the [Data wiki page](https://github.com/Ecological-Complexity-Lab/Norwood_farm/wiki/Data) for the complete data file list.

---

## 📈 Main analysis

| Script                            | What it does                                                                                                                                |
| --------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| `4_Land_conversion_simulation.R`  | Simulates land conversion from extensive organic to intensive non-organic management, and estimates NCP provision + proportion retained.      |
| `5_Null_model.R`                  | Builds a null model that removes the same number of species as the real simulation, but at random, and calculates Z-scores against it.        |
| `6_Sps_indirect_contribution.R`   | Estimates each species' indirect contribution to NCP provision per scenario, and tests how land conversion affects it.                        |
| `7_Graph_networks.R`              | Produces **Figure 2:** network plots for the E, M, and IN management scenarios.                                                                |
| `8_Graphs.R`                      | Produces **Figures 3–5** and the supplementary figures.                                                                                        |

> ℹ️ Inputs and outputs for each script are not listed here — see the [wiki](https://github.com/Ecological-Complexity-Lab/Norwood_farm/wiki) for full I/O details, and the [Data wiki page](https://github.com/Ecological-Complexity-Lab/Norwood_farm/wiki/Data) for the complete data file list.

---

## 📚 More information

For full documentation, including the complete data file list and descriptions, see the project [wiki](https://github.com/Ecological-Complexity-Lab/Norwood_farm/wiki).
