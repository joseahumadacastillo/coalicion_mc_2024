# MC y la coalición opositora: análisis electoral distrital 2000–2022

Análisis en R del comportamiento electoral en México a nivel **distrito federal**, elaborado en junio de 2022. Revisa la competitividad de las elecciones presidenciales desde 2000, la volatilidad del voto por partido entre 2012 y 2018, y la relación entre el voto presidencial de 2018 y la consulta de **revocación de mandato de 2022**, con atención al papel de Movimiento Ciudadano frente a la coalición opositora.

---

## 🔎 Qué contiene el análisis

**1. Elecciones presidenciales 2000–2018 (nacional).** Resultados por partido o cabeza de coalición, diferencias entre primero, segundo y tercer lugar, e índice de competitividad de Taagepera.

**2. Volatilidad del voto 2012 → 2018 por distrito.** Cambio en el porcentaje de votos de cada partido (PRI, PAN, PRD, PVEM, PT y MC) en los 300 distritos federales, con diagramas de caja.

**3. Voto presidencial 2018 vs. revocación de mandato 2022 por distrito.** Diagramas de dispersión entre el voto de cada partido en 2018 y el porcentaje de votos para que el presidente siguiera o fuera revocado en 2022. Se hace a nivel nacional y para estados seleccionados: Jalisco, Nuevo León, Ciudad de México y Yucatán, entre otros.

**4. Mapas del voto presidencial 2012 por partido.**

## 🗂️ Estructura

```
├── Inputs/          # Resultados oficiales públicos (INE)
├── code/            # Script de análisis en R
├── outputs/         # Gráficas generadas
├── presidencial_2018_distritos_federales.rds
├── revocacion_distrital.rds
└── presidencia_2018_revocacion_distrito/
```

## 🧰 Herramientas

`R` · `tidyverse` · `ggplot2` · `ggrepel` · `patchwork` · mapas · datos de cómputos públicos del INE (presidencial 2000–2018 y revocación de mandato 2022)

## ▶️ Cómo reproducirlo

Abrir el proyecto en RStudio con el directorio del repositorio como directorio de trabajo y correr el script en `code/`. Los datos procesados están en formato `.rds` en la raíz del repositorio.

---
Autor: **José Ahumada Castillo** · [LinkedIn](https://www.linkedin.com/in/jos%C3%A9-ahumada-castillo-495b7519a/)
