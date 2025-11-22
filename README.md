# 🌍 Monitor Global de Transición Energética

> **Demo en vivo:** [Click aquí para ver el Dashboard](https://fpn6a9-jos0de0jes0s-falc0n0v0zquez.shinyapps.io/prueba_shynyapp/)

## 1. Descripción General

Este proyecto es una aplicación web interactiva desarrollada en **R Shiny** que permite monitorear, visualizar y analizar la evolución de la escena energética mundial.

La herramienta aborda la necesidad de comprender la velocidad y los determinantes de la transición desde combustibles fósiles hacia fuentes renovables. Utilizando datos históricos consolidados ([Our World in Data, Energy Institute](https://ourworldindata.org/renewable-energy)) y modelos avanzados, el dashboard no solo presenta estadísticas descriptivas, sino que ofrece explicaciones econométricas sobre la adopción de renovables y proyecciones futuras basadas en Inteligencia Artificial.

## 2. Características Principales

El dashboard está dividido en módulos de análisis que incluyen:

* **📊 Panorama Global:** Visualización de la evolución histórica (1965-2024) de la generación de energía en TWh, desglosada por fuente (Solar, Eólica, Hidro, Bio, Geo).
* **🗺️ Análisis Geoespacial:** Mapas interactivos que muestran el porcentaje de participación de renovables en la energía primaria por país, permitiendo identificar líderes y rezagados geográficos.
* **💰 Patrones Socioeconómicos:** Comparativas de adopción de energías limpias agrupadas por nivel de ingresos del Banco Mundial (Low, Middle, High Income) y dispersión regional.
* **📈 Modelo Econométrico (Panel Dinámico):** Implementación de un modelo explicativo que estima cómo influyen variables como la dependencia previa de fósiles, el nivel de ingresos y las metas *net-zero* (años restantes para el objetivo) en la adopción de renovables.
* **🤖 Proyecciones con IA (LSTM):** Uso de redes neuronales recurrentes (Long Short-Term Memory) para predecir la tendencia futura de la participación de combustibles fósiles vs. renovables.

## 3. Organización del Proyecto

La estructura del repositorio está diseñada para separar el flujo de trabajo de ciencia de datos (limpieza y modelado) del despliegue de la aplicación web.

```text
RENEWABLE-ENERGY/
│
├── Data/                            # Datos limpios y crudos
│
├── Procesamiento/                   # Códigos para procesar la información y generar variables y tablas nuevas
│ 
│
├── ShinyApp/                        # Carpeta de PRODUCCIÓN del Dashboard (Despliegue)

```

## 4. Tecnologías Utilizadas

* **Lenguaje:** R

* **Framework Web:** Shiny, ShinyDashboard

* **Visualización:** Plotly (gráficos interactivos), Leaflet (mapas), Highcharter (series temporales).

* **Manipulación de Datos:** Tidyverse (dplyr, tidyr).

* **Modelado:**
    * `plm`: Para modelos de datos de panel (Econometría).
    * `keras` / `tensorflow`: Para el modelo de Deep Learning (LSTM).
