# 🐘 Interactive Elephant Movement & Behavioral Analysis Platform

[![GitHub Pages](https://img.shields.io/badge/demo-live-success)](https://your-username.github.io/iSSA/)
[![License](https://img.shields.io/badge/license-MIT-blue)](LICENSE)

An interactive web-based visualization platform for exploring elephant movement trajectories, behavioral states, and habitat selection patterns across BACI (Before-After-Control-Impact) study periods.

## 🌟 Features

### 📍 **Trajectory Explorer**
- Animated playback of GPS trajectories with behavioral color coding
- Real-time statistics and time budget analysis
- Interactive filtering by period and behavioral state
- Satellite and street map basemap options

### 📊 **Behavioral Analysis**
- Time budget pie charts
- Density heatmaps with behavioral overlays
- 24-hour temporal activity patterns
- Period comparison visualizations

### 🗺️ **RSF Comparison**
- Interactive Resource Selection Function (RSF) maps
- Three comparison modes: Single, Side-by-Side, and Overlay
- Green-to-red habitat selection intensity scale
- 74 georeferenced RSF rasters across all elephants, periods, and behaviors

### 📚 **Methodology**
- Comprehensive study workflow infographic
- HMM and RSF methodology documentation
- Academic references and citations
- Data download information

## 🚀 Quick Start

### Local Development

1. **Clone the repository**
   ```bash
   git clone https://github.com/your-username/iSSA.git
   cd iSSA/web_visualization
   ```

2. **Start a local HTTP server**
   ```bash
   # Using Python 3
   python -m http.server 8000
   
   # Or using Python 2
   python -m SimpleHTTPServer 8000
   
   # Or using Node.js
   npx http-server -p 8000
   ```

3. **Open in browser**
   ```
   http://localhost:8000
   ```

### GitHub Pages Deployment

1. **Push to GitHub**
   ```bash
   git add .
   git commit -m "Deploy elephant analysis platform"
   git push origin main
   ```

2. **Enable GitHub Pages**
   - Go to repository Settings → Pages
   - Source: Deploy from branch
   - Branch: `main` → `/web_visualization`
   - Save

3. **Access your site**
   ```
   https://your-username.github.io/iSSA/
   ```

## 📱 Mobile Support

The platform is fully responsive and optimized for mobile devices:
- ✅ Touch-friendly controls
- ✅ Responsive layouts (breakpoints at 768px and 1024px)
- ✅ Mobile-optimized maps
- ✅ Adaptive navigation

## 🎨 Technology Stack

- **Frontend**: HTML5, CSS3 (Vanilla), JavaScript (ES6+)
- **Mapping**: Leaflet.js with satellite imagery support
- **Charts**: Chart.js
- **Data Processing**: PapaParse (CSV), Proj4js (coordinate transformation)
- **Heatmaps**: Leaflet.heat
- **Styling**: Custom CSS with glassmorphism and dark mode

## 📂 Project Structure

```
web_visualization/
├── index.html              # Landing page
├── explorer.html           # Trajectory explorer
├── behavioral.html         # Behavioral analysis
├── rsf-comparison.html     # RSF comparison tool
├── methodology.html        # Methods & references
├── css/
│   ├── styles.css         # Global styles
│   ├── behavioral.css     # Behavioral page styles
│   └── rsf-comparison.css # RSF page styles
├── js/
│   ├── main.js            # Global JavaScript
│   ├── explorer.js        # Trajectory logic
│   ├── behavioral.js      # Behavioral analysis logic
│   └── rsf-comparison.js  # RSF comparison logic
├── data/
│   ├── rsf_maps/          # 74 RSF PNG rasters + metadata
│   ├── kw_boundary.geojson
│   ├── hv_boundary.geojson
│   └── fence_line.geojson
├── elephants/             # Elephant profile images
└── .nojekyll             # GitHub Pages configuration
```

## 🐘 Elephant Profiles

| ID | Name | Home Range | Periods Available |
|----|------|------------|-------------------|
| E1 | Kamva | Kariega West (KW) | PRE, INTERIM |
| E2 | Kambaku | Kariega West (KW) | PRE, INTERIM |
| E3 | Bukela | Howieson's Poort (HV) | PRE, INTERIM, POST |
| E4 | Half Moon | Howieson's Poort (HV) | PRE, INTERIM, POST |
| E5 | Beauty | Howieson's Poort (HV) | PRE, INTERIM, POST |
| E6 | Balu | Kariega West (KW) | PRE, INTERIM |

## 📊 Data

### Behavioral Points
- **Format**: CSV files with HMM-decoded behavioral states
- **Location**: `../results/RSF/behavioral_points/`
- **Columns**: `x_m`, `y_m`, `date`, `behavior`, `Stage`, `Zone`

### RSF Rasters
- **Format**: GeoTIFF (converted to PNG for web)
- **Resolution**: 30m
- **Color Scale**: Green (low selection) → Red (high selection)
- **Total**: 74 rasters (6 elephants × multiple periods × 3 behaviors)

## 🔬 Methodology

### Hidden Markov Models (HMM)
Three-state behavioral classification:
- **Resting**: Short steps, random turns (Blue)
- **Foraging**: Medium steps, moderate directionality (Green)
- **Movement**: Long steps, directional (Orange)

### Resource Selection Functions (RSF)
GLM-based habitat selection modeling:
- **Covariates**: Vegetation, water distance, terrain, elevation
- **Scale**: 30m resolution
- **Validation**: k-fold cross-validation

## 📚 References

- Buderman, F. E., et al. (2024). Integrated movement models for individual tracking and species distribution data. *Methods in Ecology and Evolution* 16(2), 345–361.
- Getz, W. M. (2023). An animal movement track segmentation framework for forecasting range adaptation under global change. *Frontiers in Ecology and Evolution* 11.
- Pohle, J., et al. (2024). How to account for behavioral states in step-selection analysis: a model comparison. *PeerJ* 12, e16509.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- Wageningen University & Research
- Kariega Private Game Reserve
- All contributors to the elephant tracking and analysis project

## 📧 Contact

For questions or collaborations, please open an issue or contact [harinaiyanna@gmail.com]

---

**Built with ❤️ for elephant conservation and behavioral ecology research**
