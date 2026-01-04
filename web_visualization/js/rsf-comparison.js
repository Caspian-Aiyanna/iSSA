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

    // GIS Map Download
    document.getElementById('download-gis-map').addEventListener('click', exportGISMap);
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
        maxZoom: 18,
        crossOrigin: true
    });

    const satellite = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: '© Esri',
        maxZoom: 18,
        crossOrigin: true
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
        maxZoom: 18,
        crossOrigin: true
    });
    const satellite2 = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: '© Esri',
        maxZoom: 18,
        crossOrigin: true
    });
    satellite2.addTo(leftMap);
    L.control.layers({ "Street Map": streetMap2, "Satellite": satellite2 }).addTo(leftMap);

    const streetMap3 = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '© OpenStreetMap contributors',
        maxZoom: 18,
        crossOrigin: true
    });
    const satellite3 = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: '© Esri',
        maxZoom: 18,
        crossOrigin: true
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

    // Ensure proj4 is globally available and define local South African UTM Zone
    if (typeof proj4 !== 'undefined') {
        window.proj4 = proj4;
        // Define UTM Zone 35S (Primary CRS for Kariega/Eastern Cape at 26.5E)
        proj4.defs("EPSG:32735", "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs");
        // Define UTM Zone 36S just in case for surrounding areas
        proj4.defs("EPSG:32736", "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs");
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

        const response = await fetch(rasterPath);
        if (!response.ok) {
            throw new Error(`Failed to load raster: ${response.status} ${response.statusText}`);
        }

        const arrayBuffer = await response.arrayBuffer();
        const georaster = await parseGeoraster(arrayBuffer);

        // SPATIAL ALIGNMENT: Enforce UTM Zone 35S (EPSG:32735) for Kariega study area
        const isProjected = Math.abs(georaster.xmin) > 1000 || Math.abs(georaster.ymin) > 1000;
        if (isProjected) {
            georaster.projection = 32735;
            georaster.crs = "EPSG:32735";
        }

        const layer = new GeoRasterLayer({
            georaster: georaster,
            pane: 'rasterPane',
            opacity: 0.95,
            pixelValuesToColorFn: function (values) {
                const value = values[0];
                if (value === null || value === undefined || value === georaster.noDataValue || isNaN(value)) {
                    return null;
                }

                const min = georaster.mins[0];
                const max = georaster.maxs[0];
                const normalized = (max === min) ? 0.5 : (value - min) / (max - min);

                let r, g, b;
                if (normalized < 0.2) {
                    const t = normalized / 0.2;
                    r = Math.floor(t * 100); g = Math.floor(t * 100); b = 255;
                } else if (normalized < 0.4) {
                    const t = (normalized - 0.2) / 0.2;
                    r = 100 + Math.floor(t * 50); g = 100 + Math.floor(t * 155); b = 255;
                } else if (normalized < 0.6) {
                    const t = (normalized - 0.4) / 0.2;
                    r = 150 - Math.floor(t * 150); g = 255; b = 255 - Math.floor(t * 255);
                } else if (normalized < 0.8) {
                    const t = (normalized - 0.6) / 0.2;
                    r = Math.floor(t * 255); g = 255; b = 0;
                } else {
                    const t = (normalized - 0.8) / 0.2;
                    r = 255; g = 255 - Math.floor(t * 255); b = 0;
                }
                return `rgba(${r}, ${g}, ${b}, 1.0)`;
            },
            resolution: 512, // High resolution for publication quality
            pixelValuesToColorFn_noDataValue: georaster.noDataValue,
            // DO NOT pass manual 'bounds' here - let the engine reproject correctly from projection metadata
        });

        layer.addTo(map);

        // Precision fit after a short delay for projection logic
        setTimeout(() => {
            try {
                const bounds = layer.getBounds();
                if (bounds && bounds.isValid()) {
                    map.fitBounds(bounds, { padding: [15, 15] });
                }
            } catch (e) {
                console.warn('[Spatial Sync] Viewing extent adjustment failed');
            }
        }, 200);

        return layer;
    } catch (error) {
        console.error(`Error loading raster layer:`, error);
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
// ============================================================================
// GIS EXPORT SYSTEM
// ============================================================================

async function exportGISMap() {
    showLoading(true);

    // 1. Clean up UI for export
    const controls = document.querySelectorAll('.leaflet-control-container');
    controls.forEach(c => c.style.display = 'none');

    // Small delay to ensure any layout shifts or tile loads settle
    await new Promise(r => setTimeout(r, 600));

    try {
        const mapContainerId = comparisonMode === 'single' ? 'single-map' : 'left-map';
        const mapElement = document.getElementById(mapContainerId);

        // Capture with high scale for GIS clarity
        const canvas = await html2canvas(mapElement, {
            useCORS: true,
            allowTaint: true,
            backgroundColor: '#0a0e1a',
            scale: 2,
            logging: false
        });

        // Restore UI
        controls.forEach(c => c.style.display = '');

        const dateStr = new Date().toLocaleDateString();
        const elephantName = document.getElementById('elephant-name').textContent;
        const behaviorName = currentBehavior;
        const periodName = currentPeriod.toUpperCase();

        const gisCanvas = document.createElement('canvas');
        const gisCtx = gisCanvas.getContext('2d');
        gisCanvas.width = 1200;
        gisCanvas.height = 1600;

        // Background
        gisCtx.fillStyle = '#111827';
        gisCtx.fillRect(0, 0, gisCanvas.width, gisCanvas.height);

        // 2. Aspect Ratio Correction
        const sourceAspect = canvas.width / canvas.height;
        const mapY = 150;
        const mapWidth = gisCanvas.width - 80;
        let mapHeight = mapWidth / sourceAspect;

        // Prevent overflow
        if (mapHeight > 1150) mapHeight = 1150;

        // Draw Map
        gisCtx.drawImage(canvas, 40, mapY, mapWidth, mapHeight);

        // Header
        gisCtx.fillStyle = '#ffffff';
        gisCtx.font = 'bold 36px Inter, sans-serif';
        gisCtx.fillText('Elephant Habitat Selection Analysis', 40, 60);

        gisCtx.font = '24px Inter, sans-serif';
        gisCtx.fillStyle = '#94a3b8';
        gisCtx.fillText(`${elephantName} | ${behaviorName} | BACI: ${periodName}`, 40, 100);

        // 3. Dynamic Elements Positioning
        drawNorthArrow(gisCtx, gisCanvas.width - 100, mapY + 60);
        drawScaleBar(gisCtx, 80, mapY + mapHeight - 30);

        const legendY = mapY + mapHeight + 60;
        drawGISLegend(gisCtx, 40, legendY);

        // Footer
        gisCtx.fillStyle = '#475569';
        gisCtx.font = '16px Inter, sans-serif';
        gisCtx.fillText(`Source: Kariega Elephant Study | Exported: ${dateStr}`, 40, 1550);
        gisCtx.fillText('Coordinate System: WGS 84 / Web Mercator', 40, 1575);

        const link = document.createElement('a');
        link.download = `GIS_Map_${currentElephant}_${currentPeriod}_${behaviorName}.png`;
        link.href = gisCanvas.toDataURL('image/png', 1.0);
        link.click();

        showLoading(false);
    } catch (error) {
        console.error('GIS Export Error:', error);
        controls.forEach(c => c.style.display = '');
        alert('Failed to generate GIS map.');
        showLoading(false);
    }
}

function drawNorthArrow(ctx, x, y) {
    ctx.save();
    ctx.translate(x, y);
    ctx.strokeStyle = '#ffffff';
    ctx.lineWidth = 3;

    // Draw N
    ctx.fillStyle = '#ffffff';
    ctx.font = 'bold 24px serif';
    ctx.fillText('N', -10, -40);

    // Draw Arrow
    ctx.beginPath();
    ctx.moveTo(0, -30);
    ctx.lineTo(15, 20);
    ctx.lineTo(0, 5);
    ctx.lineTo(-15, 20);
    ctx.closePath();
    ctx.stroke();
    ctx.fill();
    ctx.restore();
}

function drawScaleBar(ctx, x, y) {
    ctx.strokeStyle = '#ffffff';
    ctx.lineWidth = 2;
    ctx.fillStyle = '#ffffff';
    ctx.font = '18px sans-serif';

    const barWidth = 200;
    ctx.beginPath();
    ctx.moveTo(x, y);
    ctx.lineTo(x, y + 10);
    ctx.lineTo(x + barWidth, y + 10);
    ctx.lineTo(x + barWidth, y);
    ctx.stroke();

    ctx.fillText('0', x - 5, y + 35);
    ctx.fillText('1 km', x + barWidth - 20, y + 35);
}

function drawGISLegend(ctx, x, y) {
    ctx.fillStyle = '#ffffff';
    ctx.font = 'bold 22px sans-serif';
    ctx.fillText('Selection Intensity', x, y);

    const gradWidth = 400;
    const gradHeight = 30;
    const gradY = y + 20;

    // Create gradient
    const grad = ctx.createLinearGradient(x, 0, x + gradWidth, 0);
    // Custom visible palette
    grad.addColorStop(0.0, 'rgba(100, 100, 255, 1)');
    grad.addColorStop(0.5, 'rgba(0, 255, 0, 1)');
    grad.addColorStop(1.0, 'rgba(255, 0, 0, 1)');

    ctx.fillStyle = grad;
    ctx.fillRect(x, gradY, gradWidth, gradHeight);

    ctx.strokeStyle = '#ffffff';
    ctx.strokeRect(x, gradY, gradWidth, gradHeight);

    ctx.fillStyle = '#e2e8f0';
    ctx.font = '16px sans-serif';
    ctx.fillText('Low (Avoidance)', x, gradY + 50);
    ctx.fillText('High (Selection)', x + gradWidth - 110, gradY + 50);
}
