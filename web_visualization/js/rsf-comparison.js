// RSF Comparison JavaScript - Rewritten to use GeoTIFF files directly
// Fixes: Direct raster loading, dynamic heatmap colors, data availability checks, home range mapping

// ============================================================================
// GLOBAL STATE
// ============================================================================

let currentElephant = 'E3';
let currentBehavior = 'Foraging';
let currentMapType = 'Realized';
let currentPeriod = 'pre';
let comparisonMode = 'single';
let periodLeft = 'pre';
let periodRight = 'post';

// Map instances
let singleMap = null;
let leftMap = null;
let rightMap = null;

// Layer references
let currentRasterLayer = null;
let leftRasterLayer = null;
let rightRasterLayer = null;
let boundaryInstanceMap = {};
let fenceInstanceMap = {};

// Data availability mapping
const DATA_AVAILABILITY = {
    'E1': ['pre', 'interim'],      // No POST
    'E2': ['pre', 'interim'],      // No POST
    'E3': ['pre', 'interim', 'post'],
    'E4': ['pre', 'interim', 'post'],
    'E5': ['pre', 'interim', 'post'],
    'E6': ['pre', 'interim']       // No POST
};

// Home range mapping
const HOME_RANGES = {
    'E1': 'KW',
    'E2': 'KW',
    'E3': 'KW',
    'E4': 'KW',
    'E5': 'HV',  // Harvestvale for PRE, Kariega for INTERIM/POST
    'E6': 'HV'   // Harvestvale for PRE, Kariega for INTERIM/POST
};

// ============================================================================
// INITIALIZATION
// ============================================================================

document.addEventListener('DOMContentLoaded', async () => {
    initializeEventListeners();
    initializeMaps();
    updatePeriodAvailability();
    await updateVisualization();
});

// ============================================================================
// EVENT LISTENERS
// ============================================================================

function initializeEventListeners() {
    // Elephant selection
    document.querySelectorAll('.elephant-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.elephant-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentElephant = e.target.dataset.elephant;
            updatePeriodAvailability();
            updateVisualization();
        });
    });

    // Behavior selection
    document.querySelectorAll('.behavior-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.behavior-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentBehavior = e.target.dataset.behavior;
            updateVisualization();
        });
    });

    // Map type selection
    document.querySelectorAll('.map-type-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.map-type-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentMapType = e.target.dataset.type;
            updateVisualization();
        });
    });

    // Comparison mode selection
    document.querySelectorAll('.comparison-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.comparison-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            comparisonMode = e.target.dataset.mode;
            switchComparisonMode(comparisonMode);
        });
    });

    // Period selection (single mode)
    document.querySelectorAll('.period-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.period-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentPeriod = e.target.dataset.period;
            updateVisualization();
        });
    });

    // Period comparison selects
    document.getElementById('period-left').addEventListener('change', (e) => {
        periodLeft = e.target.value;
        updateVisualization();
    });

    document.getElementById('period-right').addEventListener('change', (e) => {
        periodRight = e.target.value;
        updateVisualization();
    });

    // Opacity slider
    document.getElementById('opacity-slider').addEventListener('input', (e) => {
        const opacity = e.target.value / 100;
        document.getElementById('opacity-value').textContent = `${e.target.value}%`;
        if (currentRasterLayer) {
            currentRasterLayer.setOpacity(opacity);
        }
    });

    // Layer toggles
    document.getElementById('show-boundaries').addEventListener('change', (e) => {
        const checked = e.target.checked;
        Object.keys(boundaryInstanceMap).forEach(mapKey => {
            const map = mapKey === 'single' ? singleMap : (mapKey === 'left' ? leftMap : rightMap);
            const layers = boundaryInstanceMap[mapKey];
            if (checked) {
                layers.kw.addTo(map);
                layers.hv.addTo(map);
            } else {
                map.removeLayer(layers.kw);
                map.removeLayer(layers.hv);
            }
        });
    });

    document.getElementById('show-fence').addEventListener('change', (e) => {
        const checked = e.target.checked;
        Object.keys(fenceInstanceMap).forEach(mapKey => {
            const map = mapKey === 'single' ? singleMap : (mapKey === 'left' ? leftMap : rightMap);
            const layer = fenceInstanceMap[mapKey];
            if (checked) {
                layer.addTo(map);
            } else {
                map.removeLayer(layer);
            }
        });
    });

    // Reset view button
    document.getElementById('reset-view').addEventListener('click', () => {
        const center = [-33.60, 26.55];
        const zoom = 12;
        singleMap.setView(center, zoom);
        leftMap.setView(center, zoom);
        rightMap.setView(center, zoom);
    });
}

// ============================================================================
// MAP INITIALIZATION
// ============================================================================

function initializeMaps() {
    const center = [-33.60, 26.55];
    const zoom = 12;

    // Initialize single map
    singleMap = L.map('single-map').setView(center, zoom);

    // Initialize side-by-side maps
    leftMap = L.map('left-map').setView(center, zoom);
    rightMap = L.map('right-map').setView(center, zoom);

    // Define base layers
    const streetMap = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '© OpenStreetMap contributors',
        maxZoom: 18
    });

    const satellite = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: '© Esri',
        maxZoom: 18
    });

    const baseMaps = {
        "Street Map": streetMap,
        "Satellite": satellite
    };

    // Add default layer and controls to all maps
    satellite.addTo(singleMap);
    L.control.layers(baseMaps).addTo(singleMap);

    const streetMap2 = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '© OpenStreetMap contributors',
        maxZoom: 18
    });
    const satellite2 = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: '© Esri',
        maxZoom: 18
    });
    satellite2.addTo(leftMap);
    L.control.layers({ "Street Map": streetMap2, "Satellite": satellite2 }).addTo(leftMap);

    const streetMap3 = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '© OpenStreetMap contributors',
        maxZoom: 18
    });
    const satellite3 = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: '© Esri',
        maxZoom: 18
    });
    satellite3.addTo(rightMap);
    L.control.layers({ "Street Map": streetMap3, "Satellite": satellite3 }).addTo(rightMap);

    // Create panes for all maps to ensure correct layer stacking
    [singleMap, leftMap, rightMap].forEach(map => {
        if (!map.getPane('rasterPane')) {
            map.createPane('rasterPane');
            map.getPane('rasterPane').style.zIndex = 450;
            map.getPane('rasterPane').style.pointerEvents = 'none';
        }
        if (!map.getPane('boundariesPane')) {
            map.createPane('boundariesPane');
            map.getPane('boundariesPane').style.zIndex = 650;
            map.getPane('boundariesPane').style.pointerEvents = 'none';
        }
    });

    // Ensure proj4 is globally available for georaster projection
    if (typeof proj4 !== 'undefined') {
        window.proj4 = proj4;
    }

    // Sync map views
    singleMap.sync(leftMap);
    singleMap.sync(rightMap);
    leftMap.sync(singleMap);
    leftMap.sync(rightMap);
    rightMap.sync(singleMap);
    rightMap.sync(leftMap);

    // Load boundaries
    loadBoundaries();
}

// Load study area boundaries
async function loadBoundaries() {
    try {
        const [kwResponse, hvResponse, fenceResponse] = await Promise.all([
            fetch('data/kw_boundary.geojson'),
            fetch('data/hv_boundary.geojson'),
            fetch('data/fence_line.geojson')
        ]);

        const kwData = await kwResponse.json();
        const hvData = await hvResponse.json();
        const fenceData = await fenceResponse.json();

        // Panes are already created in initializeMaps

        const kwStyle = { color: '#10b981', weight: 2, fillOpacity: 0.1 };
        const hvStyle = { color: '#3b82f6', weight: 2, fillOpacity: 0.1 };
        const fenceStyle = { color: '#ef4444', weight: 3, dashArray: '5, 5' };

        // Create separate layer instances for each map
        boundaryInstanceMap = {
            'single': {
                kw: L.geoJSON(kwData, { style: kwStyle, pane: 'boundariesPane', interactive: false }).addTo(singleMap),
                hv: L.geoJSON(hvData, { style: hvStyle, pane: 'boundariesPane', interactive: false }).addTo(singleMap)
            },
            'left': {
                kw: L.geoJSON(kwData, { style: kwStyle, pane: 'boundariesPane', interactive: false }).addTo(leftMap),
                hv: L.geoJSON(hvData, { style: hvStyle, pane: 'boundariesPane', interactive: false }).addTo(leftMap)
            },
            'right': {
                kw: L.geoJSON(kwData, { style: kwStyle, pane: 'boundariesPane', interactive: false }).addTo(rightMap),
                hv: L.geoJSON(hvData, { style: hvStyle, pane: 'boundariesPane', interactive: false }).addTo(rightMap)
            }
        };

        fenceInstanceMap = {
            'single': L.geoJSON(fenceData, { style: fenceStyle, pane: 'boundariesPane', interactive: false }).addTo(singleMap),
            'left': L.geoJSON(fenceData, { style: fenceStyle, pane: 'boundariesPane', interactive: false }).addTo(leftMap),
            'right': L.geoJSON(fenceData, { style: fenceStyle, pane: 'boundariesPane', interactive: false }).addTo(rightMap)
        };

    } catch (error) {
        console.warn('Could not load boundaries:', error);
    }
}

// ============================================================================
// PERIOD AVAILABILITY
// ============================================================================

function updatePeriodAvailability() {
    const availablePeriods = DATA_AVAILABILITY[currentElephant];

    // Update single period buttons
    document.querySelectorAll('.period-btn').forEach(btn => {
        const period = btn.dataset.period;
        if (availablePeriods.includes(period)) {
            btn.disabled = false;
            btn.style.opacity = '1';
        } else {
            btn.disabled = true;
            btn.style.opacity = '0.3';
        }
    });

    // Update comparison period selects
    const leftSelect = document.getElementById('period-left');
    const rightSelect = document.getElementById('period-right');

    ['pre', 'interim', 'post'].forEach(period => {
        const leftOption = leftSelect.querySelector(`option[value="${period}"]`);
        const rightOption = rightSelect.querySelector(`option[value="${period}"]`);

        if (availablePeriods.includes(period)) {
            leftOption.disabled = false;
            rightOption.disabled = false;
        } else {
            leftOption.disabled = true;
            rightOption.disabled = true;
        }
    });

    // Reset to valid period if current is unavailable
    if (!availablePeriods.includes(currentPeriod)) {
        currentPeriod = availablePeriods[0];
        document.querySelector(`.period-btn[data-period="${currentPeriod}"]`).classList.add('active');
    }

    console.log(`[Period Availability] ${currentElephant}: ${availablePeriods.join(', ')}`);
}

// ============================================================================
// RASTER PATH CONSTRUCTION
// ============================================================================

function getRasterPath(elephant, period, behavior, type) {
    // Path: ../results/RSF/rasters/{period}/{Type}_E{X}_{period}_{Behavior}.tif
    // Example: ../results/RSF/rasters/pre/Realized_E3_pre_Foraging.tif

    const elephantNum = elephant.replace('E', '');
    const filename = `${type}_E${elephantNum}_${period}_${behavior}.tif`;
    const path = `../results/RSF/rasters/${period}/${filename}`;

    console.log(`[Raster Path] ${path}`);
    return path;
}

// ============================================================================
// RASTER LOADING WITH GEOTIFF
// ============================================================================

async function loadRasterLayer(elephant, period, behavior, type, map) {
    try {
        const rasterPath = getRasterPath(elephant, period, behavior, type);

        // Fetch the GeoTIFF file
        const response = await fetch(rasterPath);
        if (!response.ok) {
            throw new Error(`Failed to load raster: ${response.status} ${response.statusText}`);
        }

        const arrayBuffer = await response.arrayBuffer();

        // Parse GeoTIFF
        const georaster = await parseGeoraster(arrayBuffer);

        console.log(`[GeoRaster] Loaded: ${georaster.width}x${georaster.height}, Range: ${georaster.mins[0]} - ${georaster.maxs[0]}`);
        console.log(`[GeoRaster] Bounds: [${georaster.ymin}, ${georaster.xmin}] to [${georaster.ymax}, ${georaster.xmax}]`);
        console.log(`[GeoRaster] NoData Value: ${georaster.noDataValue}`);
        console.log(`[GeoRaster] Projection: ${georaster.projection}`);
        console.log(`[GeoRaster] EPSG Code: ${georaster.epsg || 'Not specified'}`);

        // Check if projection is UTM (EPSG:32736) - common for South African data
        const proj = georaster.projection ? String(georaster.projection) : "";
        if (proj.includes('32736')) {
            console.warn('[GeoRaster] Raster is in UTM Zone 36S (EPSG:32736) - will be reprojected to WGS84 for display');
        }

        // Create dynamic heatmap color scale (optimized for visibility on satellite)
        const layer = new GeoRasterLayer({
            georaster: georaster,
            pane: 'rasterPane', // Explicitly use our high-z-index pane
            opacity: 0.95,  // High base opacity for better visibility
            pixelValuesToColorFn: function (values) {
                const value = values[0];

                // Handle no-data values
                if (value === null ||
                    value === undefined ||
                    value === georaster.noDataValue ||
                    isNaN(value)) {
                    return null;
                }

                // Normalization
                const min = georaster.mins[0];
                const max = georaster.maxs[0];
                const normalized = (max === min) ? 0.5 : (value - min) / (max - min);

                // High-visibility palette (optimized for satellite maps):
                // Deep Purple/Vibrant Blue (Low) -> Cyan -> Green -> Yellow -> Orange -> Red (High)
                let r, g, b;

                if (normalized < 0.2) {
                    // Deep Purple -> Blue
                    const t = normalized / 0.2;
                    r = Math.floor(t * 100);
                    g = Math.floor(t * 100);
                    b = 255;
                } else if (normalized < 0.4) {
                    // Blue -> Cyan
                    const t = (normalized - 0.2) / 0.2;
                    r = 100 + Math.floor(t * 50);
                    g = 100 + Math.floor(t * 155);
                    b = 255;
                } else if (normalized < 0.6) {
                    // Cyan -> Green
                    const t = (normalized - 0.4) / 0.2;
                    r = 150 - Math.floor(t * 150);
                    g = 255;
                    b = 255 - Math.floor(t * 255);
                } else if (normalized < 0.8) {
                    // Green -> Yellow
                    const t = (normalized - 0.6) / 0.2;
                    r = Math.floor(t * 255);
                    g = 255;
                    b = 0;
                } else {
                    // Yellow -> Red
                    const t = (normalized - 0.8) / 0.2;
                    r = 255;
                    g = 255 - Math.floor(t * 255);
                    b = 0;
                }

                return `rgba(${r}, ${g}, ${b}, 1.0)`; // Solid alpha, managed by layer opacity
            },
            resolution: 512, // Increased resolution for sharper display
            pixelValuesToColorFn_noDataValue: georaster.noDataValue,
            // Ensure exact bounds - no extrapolation
            bounds: [[georaster.ymin, georaster.xmin], [georaster.ymax, georaster.xmax]]
        });

        layer.addTo(map);

        // Fit map to EXACT raster bounds (not shapefile bounds)
        const bounds = [
            [georaster.ymin, georaster.xmin],
            [georaster.ymax, georaster.xmax]
        ];
        map.fitBounds(bounds, { padding: [20, 20] });

        // Fixate map to the extent of the raster to prevent shifting
        map.setMaxBounds(bounds);
        map.setMinZoom(map.getZoom() - 2); // Allow some zooming out but not too much

        return layer;

    } catch (error) {
        console.error(`Error loading raster layer:`, error);
        alert(`Failed to load raster for ${elephant} ${period} ${behavior}. Check console for details.`);
        return null;
    }
}

// ============================================================================
// VISUALIZATION UPDATES
// ============================================================================

async function updateVisualization() {
    showLoading(true);

    try {
        if (comparisonMode === 'single') {
            await renderSingleMap();
        } else if (comparisonMode === 'side-by-side') {
            await renderSideBySide();
        } else if (comparisonMode === 'overlay') {
            await renderOverlay();
        }

        updateMapTitle();
        updateMapInfo();
        showLoading(false);
    } catch (error) {
        console.error('Error updating visualization:', error);
        showLoading(false);
    }
}

// Render single map
async function renderSingleMap() {
    // Remove existing raster layer
    if (currentRasterLayer && singleMap.hasLayer(currentRasterLayer)) {
        singleMap.removeLayer(currentRasterLayer);
    }

    // Load new raster
    currentRasterLayer = await loadRasterLayer(
        currentElephant,
        currentPeriod,
        currentBehavior,
        currentMapType,
        singleMap
    );

    if (currentRasterLayer) {
        const opacity = document.getElementById('opacity-slider').value / 100;
        currentRasterLayer.setOpacity(opacity);
    }
}

// Render side-by-side
async function renderSideBySide() {
    // Remove existing raster layers
    if (leftRasterLayer && leftMap.hasLayer(leftRasterLayer)) {
        leftMap.removeLayer(leftRasterLayer);
    }
    if (rightRasterLayer && rightMap.hasLayer(rightRasterLayer)) {
        rightMap.removeLayer(rightRasterLayer);
    }

    // Update map titles
    document.getElementById('left-map-title').textContent = `${periodLeft.toUpperCase()} Period`;
    document.getElementById('right-map-title').textContent = `${periodRight.toUpperCase()} Period`;

    // Load left raster
    leftRasterLayer = await loadRasterLayer(
        currentElephant,
        periodLeft,
        currentBehavior,
        currentMapType,
        leftMap
    );

    // Load right raster
    rightRasterLayer = await loadRasterLayer(
        currentElephant,
        periodRight,
        currentBehavior,
        currentMapType,
        rightMap
    );
}

// Render overlay
async function renderOverlay() {
    // Remove existing raster layer
    if (currentRasterLayer && singleMap.hasLayer(currentRasterLayer)) {
        singleMap.removeLayer(currentRasterLayer);
    }

    // Load primary layer (with adjustable opacity)
    currentRasterLayer = await loadRasterLayer(
        currentElephant,
        periodLeft,
        currentBehavior,
        currentMapType,
        singleMap
    );

    const opacity = document.getElementById('opacity-slider').value / 100;
    if (currentRasterLayer) {
        currentRasterLayer.setOpacity(opacity);
    }

    // Load secondary layer (50% opacity, below primary)
    const secondaryLayer = await loadRasterLayer(
        currentElephant,
        periodRight,
        currentBehavior,
        currentMapType,
        singleMap
    );

    if (secondaryLayer) {
        secondaryLayer.setOpacity(0.5);
        secondaryLayer.bringToBack();
    }
}

// ============================================================================
// UI UPDATES
// ============================================================================

function switchComparisonMode(mode) {
    // Hide all containers
    document.getElementById('single-map-container').classList.add('hidden');
    document.getElementById('side-by-side-container').classList.add('hidden');

    // Hide/show period controls
    document.getElementById('single-period-controls').classList.add('hidden');
    document.getElementById('comparison-period-controls').classList.add('hidden');
    document.getElementById('overlay-controls').classList.add('hidden');

    // Show appropriate container and controls
    if (mode === 'single') {
        document.getElementById('single-map-container').classList.remove('hidden');
        document.getElementById('single-period-controls').classList.remove('hidden');
        setTimeout(() => {
            singleMap.invalidateSize();
        }, 100);
    } else if (mode === 'side-by-side') {
        document.getElementById('side-by-side-container').classList.remove('hidden');
        document.getElementById('comparison-period-controls').classList.remove('hidden');
        setTimeout(() => {
            leftMap.invalidateSize();
            rightMap.invalidateSize();
        }, 100);
    } else if (mode === 'overlay') {
        document.getElementById('single-map-container').classList.remove('hidden');
        document.getElementById('comparison-period-controls').classList.remove('hidden');
        document.getElementById('overlay-controls').classList.remove('hidden');
        setTimeout(() => {
            singleMap.invalidateSize();
        }, 100);
    }

    updateVisualization();
}

function updateMapTitle() {
    let title = `RSF ${currentMapType} - ${currentElephant} ${currentBehavior}`;

    if (comparisonMode === 'single') {
        title += ` (${currentPeriod.toUpperCase()})`;
    } else if (comparisonMode === 'side-by-side') {
        title += ` (${periodLeft.toUpperCase()} vs ${periodRight.toUpperCase()})`;
    } else if (comparisonMode === 'overlay') {
        title += ` (${periodLeft.toUpperCase()} + ${periodRight.toUpperCase()} Overlay)`;
    }

    document.getElementById('map-title').textContent = title;
}

function updateMapInfo() {
    // Home range mapping based on actual elephant locations and data availability
    // E1, E2, E3, E4: KW (Kariega West) - all periods
    // E5, E6: HV (Harvestvale) for PRE, then Kariega for INTERIM and POST
    const homeRangeMap = {
        'E1': { range: 'KW (Kariega West)', periods: ['pre', 'interim'] },
        'E2': { range: 'KW (Kariega West)', periods: ['pre', 'interim'] },
        'E3': { range: 'KW (Kariega West)', periods: ['pre', 'interim', 'post'] },
        'E4': { range: 'KW (Kariega West)', periods: ['pre', 'interim', 'post'] },
        'E5': { range: 'HV (Harvestvale) / KW (Interim, Post)', periods: ['pre', 'interim', 'post'] },
        'E6': { range: 'HV (Harvestvale) / KW (Interim, Post)', periods: ['pre', 'interim'] }
    };

    const elephantInfo = homeRangeMap[currentElephant];

    if (elephantInfo) {
        document.getElementById('home-range').textContent = elephantInfo.range;
        document.getElementById('map-extent').textContent = elephantInfo.periods.map(p => p.toUpperCase()).join(', ');
    }
}

function showLoading(show) {
    const overlay = document.getElementById('loading-overlay');
    if (show) {
        overlay.style.opacity = '1';
        overlay.style.pointerEvents = 'all';
    } else {
        overlay.style.opacity = '0';
        overlay.style.pointerEvents = 'none';
    }
}
