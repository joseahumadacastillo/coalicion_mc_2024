# Análisis electoral por distrito: escenarios de coalición rumbo a 2024

Análisis en R de resultados electorales a nivel **distrito federal** para estimar el peso territorial de posibles coaliciones de cara a la elección de 2024, combinando la **elección presidencial de 2018** y la **consulta de revocación de mandato de 2022**.

---

## ❓ Pregunta

¿Cómo se distribuye territorialmente el voto entre distritos y qué escenarios de coalición resultan de combinar los resultados de 2018 con la participación y el voto de la revocación de mandato de 2022?

## 🗂️ Estructura

```
├── Inputs/                     # Datos de entrada (resultados oficiales públicos)
├── code/                       # Script de análisis en R
├── outputs/                    # Tablas y gráficas generadas
├── presidencial_2018_distritos_federales.rds
├── revocacion_distrital.rds
└── presidencia_2018_revocacion_distrito/
```

## 🔎 Método

1. **Limpieza y homologación** de resultados oficiales por distrito federal.
2. **Unión** de la elección presidencial 2018 con la revocación de mandato 2022 a nivel distrito.
3. **Cálculo de votación por fuerza política** y de escenarios de coalición.
4. **Comparación territorial** de participación y voto entre distritos.

## 🧰 Herramientas

`R` · `tidyverse` · `ggplot2` · datos electorales públicos (INE)

## ▶️ Cómo reproducirlo

Abrir el proyecto en RStudio y correr el script en `code/`. Los datos procesados están en formato `.rds` en la raíz del repositorio.

---
Autor: **José Ahumada Castillo** · [LinkedIn](https://www.linkedin.com/in/jos%C3%A9-ahumada-castillo-495b7519a/)
