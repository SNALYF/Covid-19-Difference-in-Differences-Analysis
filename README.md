# Empirical Project 114 - COVID-19 Policy Analysis Pipeline

This project implements a Difference-in-Differences (DiD) analysis to evaluate the impact of different COVID-19 policy responses (Mandatory vs. Optional vs. No Control) on infection and death rates across multiple countries.

The codebase has been refactored from monolithic scripts into a modular, reproducible R pipeline.

## Project Structure

The project is organized into a standard R project structure for clarity and reproducibility:

```text
project_root/
├── data/                 # Raw and processed data
│   ├── clean_data.rds
│   └── WHO-COVID-19-global-data_selected.csv
├── scripts/              # R analysis scripts
│   ├── 01_load_clean.R
│   ├── 02_descriptive_analysis.R
│   ├── 03_regression_analysis.R
│   └── archive/          # Original legacy scripts
├── R/                    # Helper functions
│   └── utils.R
├── docs/                 # Documentation and references
├── output/               # Generated results
│   ├── img/              # Beautified plots
│   └── regression/       # Regression tables (txt & png)
├── main.R                # Master script to execute the pipeline
└── README.md             # This file
```
-   **`main.R`**: The master execution script that runs the entire pipeline from start to finish.

## How to Run

You can run the entire analysis pipeline with a single command. The script handles data loading, visualization, and regression analysis automatically.

### Option 1: Using RStudio
1.  Open the project folder in RStudio.
2.  Open `main.R`.
3.  Click the **Source** button (or press `Ctrl + Shift + Enter`).

### Option 2: Using Terminal
Run the following command in your terminal:
```bash
Rscript main.R
```

## Robustness & Troubleshooting

-   **Package Installation**: `main.R` will automatically attempt to install missing packages (`dplyr`, `stargazer`, etc.).
-   **Plotting Engine**: The pipeline includes a robust plotting engine. If `ggplot2` fails due to local environment issues (e.g., missing system fonts), the scripts will automatically switch to **Enhanced Base R Plotting**. This ensures you always get high-quality, high-fidelity charts with confidence intervals, regardless of your setup.

## Outputs

After running the pipeline, check the `output/` directory:
-   **Visualizations**: Go to `output/img/` for trend lines and policy impact graphs.
-   **Tables**: Go to `output/regression/` for regression results (available as copy-pasteable text and ready-to-use PNG images).
