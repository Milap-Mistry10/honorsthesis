# **DBSCAN Clustering Implementation for SMLM Analysis**

## **Overview**  
This repository contains an implementation of the **DBSCAN (Density-Based Spatial Clustering of Applications with Noise)** algorithm, designed for clustering analysis in **Single Molecule Localization Microscopy (SMLM) datasets**. The purpose of this project is to enhance molecular cluster detection while optimizing parameter selection and visualization.

This implementation is based on the original **DBSCAN formulation by Ester et al. (1996)** and is modified from an open-source implementation by **Hahsler et al. (2019)**, available [here](https://github.com/mhahsler/dbscan). The code has been adapted to improve **parameter exploration, visualization, and result summarization** to better support SMLM data analysis.

---

## **Features & Code Highlights**  

- **Data Scaling:** Ensures all features are standardized, improving distance-based clustering performance.  
- **k-NN Distance Plot:** Assists in selecting an appropriate `eps` value for DBSCAN.  
- **Nested Parameter Search:**  
  - Iterates through multiple combinations of `eps` and `minPts` to systematically explore parameter space.  
  - Stores results for further analysis.  
- **Result Summarization:**  
  - Aggregates results into a structured dataframe.  
  - Saves output to a CSV file for external use.  
- **Visualization Tools:**  
  - **Heatmaps** showing the effect of `eps` and `minPts` on the number of detected clusters.  
  - **Scatter plots** illustrating relationships between parameters, noise points, and clusters.  
  - **Cluster visualization plots** for selected parameter combinations.  

---

## **Package Installation**  

### **Requirements:**  
This project is implemented in **R** and requires the following R packages:  
- **[`dbscan`](https://cran.r-project.org/package=dbscan)** – Implements the DBSCAN clustering algorithm in R.  
- **[`ggplot2`](https://cran.r-project.org/package=ggplot2)** – Used for visualizing clustering results.  
- **[`readr`](https://cran.r-project.org/package=readr)** – Enables efficient data handling and CSV file operations.

You can install them using the following command:

```r
install.packages(c("dbscan", "ggplot2", "readr"))
```

---

## **Files in This Repository**  

- **`DBSCAN_WORKING_COPY.R`** → The primary script for running the DBSCAN clustering algorithm.  
- **`README.md`** → Documentation explaining the repository.  
- **`LICENSE`** → GNU General Public License (GPL) v2 or later.

---

## **How to Use**  

### **1. Prepare Your Data**  
- Replace the file path in `DBSCAN_WORKING_COPY.R` with your dataset.  
- Ensure your dataset is properly formatted and cleaned before running the algorithm.  
- If testing against benchmark datasets, **publicly available datasets from Nieves et al. (2023)** can be used. These datasets provide real-world and simulated clustering scenarios for evaluating algorithm performance.  
- The datasets are available at:  
  [https://github.com/DJ-Nieves/ARI-and-IoU-cluster-analysis-evaluation](https://github.com/DJ-Nieves/ARI-and-IoU-cluster-analysis-evaluation).  

### **2. Adjust Parameters**  
- Modify `eps_values` and `minPts_values` in the script to match your dataset’s expected density.  

### **3. Run the Script**  
- Execute `DBSCAN_WORKING_COPY.R` in **RStudio** or a compatible **R environment**.  

### **4. Analyze the Results**  
- Check the **heatmaps and scatter plots** to understand how parameter choices impact clustering.  
- Use the saved **CSV summary file** for further analysis.  
- Visualize the identified clusters using the **cluster visualization plots** generated by the script.  
- Compare the clustering results against expected molecular structures to assess the accuracy and effectiveness of the algorithm.

---

## License  

This project is licensed under the **GNU General Public License v3 (GPL-3.0)**. You are free to **use, modify, and redistribute** this code under the terms of the license.  

The full text of the **GNU General Public License v3** is included in the [`LICENSE`](LICENSE) file.  

For more details, visit: [GNU GPL v3 License](https://www.gnu.org/licenses/gpl-3.0.html).  

---

## References  

Hahsler, M., Piekenbrock, M., & Doran, D. (2019). dbscan: Fast density-based clustering with R. *Journal of Statistical Software, 91*(1). https://doi.org/10.18637/jss.v091.i01  

Ester, M., Kriegel, H.-P., Sander, J., & Xu, X. (1996). A density-based algorithm for discovering clusters in large spatial databases with noise (pp. 226–331). *Second International Conference on Knowledge Discovery and Data Mining (KDD’96).* Proceedings of a conference held August 2–4. https://ui.adsabs.harvard.edu/abs/1996kddm.conf..226E  

Nieves, D. J., Pike, J. A., Levet, F., Williamson, D. J., Baragilly, M., Oloketuyi, S., De Marco, A., Griffié, J., Sage, D., Cohen, E. A. K., Sibarita, J.-B., Heilemann, M., & Owen, D. M. (
