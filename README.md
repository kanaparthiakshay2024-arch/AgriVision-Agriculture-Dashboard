# 🌾 AgriVision — India Agriculture Crop Production Dashboard

An interactive **R Shiny dashboard** for exploring agricultural crop production, seasonal patterns, state-wise comparisons, and relationships between agricultural variables in India.

## 📌 Project Overview

AgriVision transforms agricultural production data into interactive visualizations that help users explore crop production trends and compare agricultural performance across states, crops, seasons, and years.

**Course:** CSI3005 — Advanced Data Visualization Techniques  
**Project Type:** Data Visualization and Analytics  
**Technology:** R Shiny

## 🎯 Objectives

- Analyze historical crop production trends across India.
- Compare agricultural production across states and districts where data are available.
- Explore seasonal patterns in crop production.
- Investigate relationships between production, cultivated area, yield, and year where the required data are available.
- Provide interactive filters for exploring agricultural data.

## ✨ Features

- 📊 Annual production trend visualization
- 🌱 Top crops by total production
- 🗺️ State-wise and district-wise production comparisons
- 🌦️ Seasonal production heatmap
- 🔥 Correlation heatmap
- 📈 Production versus cultivated area scatter plot
- 📉 Crop yield distribution
- 🎛️ Interactive filters for state, crop, season, and year
- 📁 Interactive data table with filtered CSV download

## 🛠️ Technologies Used

- **R**
- **Shiny**
- **shinydashboard**
- **tidyverse**
- **ggplot2**
- **Plotly**
- **DT**
- **viridis**
- **scales**

## 📂 Project Structure

```text
AgriVision-Agriculture-Dashboard/
├── app.R
├── README.md
└── crop_production.csv
```

The CSV file may be stored externally if it is too large to upload to GitHub.

## 📊 Dataset


## 📊 Dataset

**Dataset Name:** India Agriculture Crop Production Dataset

**Source:** Kaggle

🔗 **[Download the Dataset from Kaggle](https://www.kaggle.com/datasets/pyatakov/india-agriculture-crop-production)**

The dataset contains historical agricultural crop production records from India and supports analysis across states, districts, crops, seasons, and years.

### Dataset Attributes
- State
- District
- Crop Year
- Season
- Crop
- Production
- Area and Yield, where available

### Dataset Usage
1. Open the Kaggle dataset link above.
2. Download the dataset as a CSV file.
3. Rename the file to `crop_production.csv` if necessary.
4. Place the CSV file in the same directory as `app.R`.
5. Run the R Shiny application.

**Note:** The dataset is hosted on Kaggle, so users may need to sign in to download it. The dataset is not stored directly in this GitHub repository.


## ⚙️ Installation and Setup

### 1. Install R

Download R from [CRAN](https://cran.r-project.org/). RStudio is recommended for development.

### 2. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/AgriVision-Agriculture-Dashboard.git
cd AgriVision-Agriculture-Dashboard
```

### 3. Install the required packages

Open R or RStudio and run:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "DT",
  "plotly",
  "viridis",
  "scales"
))
```

### 4. Add the dataset

Download `crop_production.csv` using the dataset link above and place it in the same directory as `app.R`.

If your CSV has a different filename, update `DATA_FILE` in `app.R` accordingly.

### 5. Run the dashboard

```r
shiny::runApp()
```

Alternatively, open `app.R` in RStudio and click **Run App**.

## 📈 Visualizations

AgriVision includes interactive charts for:

1. Annual crop production trends
2. Seasonal production comparisons
3. Top crops by production
4. State and district comparisons
5. Crop production distribution
6. Production-area relationships
7. Correlation heatmap
8. Parallel coordinates visualization

Some visualizations depend on whether the corresponding fields are available in the dataset.

## 🔮 Future Enhancements

- Add an interactive geographic map of India.
- Integrate updated agricultural data sources.
- Add crop production forecasting using machine learning.
- Include weather and rainfall indicators.
- Deploy the dashboard online for public access.

## 👨‍💻 Author

**Kanaparthi Akshay**

Integrated M.Tech — Computer Science and Engineering  
VIT Vellore

## 📄 Disclaimer

This project is intended for academic and analytical purposes. Interpretations depend on the coverage, quality, and units of the source dataset. The dashboard does not establish causation from correlations.

