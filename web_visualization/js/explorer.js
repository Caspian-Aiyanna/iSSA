/**
 * Trajectory Explorer JavaScript - REAL DATA VERSION
 * Loads actual elephant behavioral data from CSV files
 */

// Global state
let map = null;
let currentElephant = 'E1';
let currentPeriod = 'pre';
let trajectoryData = null;
let fullDataset = null;
let trajectoryLayer = null;
let currentMarker = null;
let markerLayer = null;
let isPlaying = false;
let currentIndex = 0;
let animationInterval = null;

// ===================================
// INITIALIZATION
// ===================================
document.addEventListener('DOMContentLoaded', () => {
    // Get elephant from URL parameter
    const urlParams = new URLSearchParams(window.location.search);
    const elephantParam = urlParams.get('elephant');
    if (elephantParam) {
        currentElephant = elephantParam;
        updateElephantSelection();
    }

    initializeMap();
    initializeControls();

    // Auto-load data for selected elephant
    setTimeout(() => {
        loadRealData();
    }, 500);
});

// ===================================
// MAP INITIALIZATION
// ===================================
async function initializeMap() {
    // Load map configuration
    const configResponse = await fetch('data/map_config.json');
    const mapConfig = await configResponse.json();

    // Initialize Leaflet map centered on study area
    map = L.map('map').setView(
        [mapConfig.center.lat, mapConfig.center.lng],
        mapConfig.zoom
    );

    // Add OpenStreetMap tiles
    const osmLayer = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '© OpenStreetMap contributors',
        maxZoom: 18
    });

    // Add satellite imagery option
    const satellite = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: 'Tiles © Esri',
        maxZoom: 18
    }).addTo(map);

    // Layer control
    const baseMaps = {
        "Satellite": satellite,
        "Street Map": osmLayer
    };

    L.control.layers(baseMaps).addTo(map);

    // Add scale
    L.control.scale().addTo(map);

    // Create layer group for markers
    markerLayer = L.layerGroup().addTo(map);

    // Load and display study area boundaries
    try {
        const kwResponse = await fetch('data/kw_boundary.geojson');
        const kwData = await kwResponse.json();
        L.geoJSON(kwData, {
            style: {
                color: '#667eea',
                weight: 2,
                fillOpacity: 0.1
            }
        }).addTo(map).bindPopup('KW Study Area');

        const hvResponse = await fetch('data/hv_boundary.geojson');
        const hvData = await hvResponse.json();
        L.geoJSON(hvData, {
            style: {
                color: '#764ba2',
                weight: 2,
                fillOpacity: 0.1
            }
        }).addTo(map).bindPopup('HV Study Area');

        // Load fence line (will be shown/hidden based on period)
        const fenceResponse = await fetch('data/fence_line.geojson');
        const fenceData = await fenceResponse.json();
        window.fenceLayer = L.geoJSON(fenceData, {
            style: {
                color: '#ef4444',
                weight: 3,
                opacity: 0.8
            }
        }).addTo(map).bindPopup('Fence Line');

        console.log('Study area boundaries and fence line loaded');
    } catch (error) {
        console.error('Error loading boundaries:', error);
    }

    // Set map bounds to study area
    const bounds = L.latLngBounds(
        [mapConfig.bounds.south, mapConfig.bounds.west],
        [mapConfig.bounds.north, mapConfig.bounds.east]
    );
    map.fitBounds(bounds, { padding: [50, 50] });
    map.setMaxBounds(bounds);
    map.setMinZoom(10);

    // Fix map size issue
    setTimeout(() => {
        map.invalidateSize();
    }, 100);

    console.log('Map initialized successfully!');
}

// ===================================
// REAL DATA LOADING
// ===================================
async function loadRealData() {
    showDataStatus('loading', `Loading data for ${currentElephant}...`);
    document.getElementById('loading').style.display = 'flex';

    try {
        // Construct path to CSV file
        const csvPath = `../results/RSF/behavioral_points/${currentElephant}_behavioral_points.csv`;

        console.log('Loading CSV from:', csvPath);

        // Fetch and parse CSV
        const response = await fetch(csvPath);

        if (!response.ok) {
            throw new Error(`Failed to load data: ${response.status} ${response.statusText}`);
        }

        const csvText = await response.text();

        // Parse CSV with PapaParse
        Papa.parse(csvText, {
            header: true,
            dynamicTyping: true,
            skipEmptyLines: true,
            complete: function (results) {
                console.log(`Loaded ${results.data.length} points for ${currentElephant}`);
                processLoadedData(results.data);
            },
            error: function (error) {
                console.error('CSV parsing error:', error);
                showDataStatus('error', `Error parsing CSV: ${error.message}`);
                document.getElementById('loading').style.display = 'none';
            }
        });

    } catch (error) {
        console.error('Error loading data:', error);
        showDataStatus('error',
            `<strong>Error loading data:</strong><br>` +
            `${error.message}<br>` +
            `<small>Make sure the CSV file exists at: ../results/RSF/behavioral_points/${currentElephant}_behavioral_points.csv</small>`
        );
        document.getElementById('loading').style.display = 'none';
    }
}

function processLoadedData(data) {
    // Store full dataset
    fullDataset = data;

    // Filter by current period
    filterDataByPeriod();

    document.getElementById('loading').style.display = 'none';
}

function filterDataByPeriod() {
    if (!fullDataset) return;

    // Filter by stage (period)
    let filteredData = fullDataset.filter(row => {
        return row.Stage && row.Stage.toLowerCase() === currentPeriod.toLowerCase();
    });

    if (filteredData.length === 0) {
        showDataStatus('warning',
            `No data found for ${currentElephant} in ${currentPeriod.toUpperCase()} period.<br>` +
            `<small>Try selecting a different period.</small>`
        );
        trajectoryData = null;
        return;
    }

    // Get max points setting
    const maxPointsSetting = document.getElementById('max-points').value;
    const maxPoints = maxPointsSetting === 'all' ? filteredData.length : parseInt(maxPointsSetting);

    // Sample data if needed
    if (filteredData.length > maxPoints) {
        const step = Math.floor(filteredData.length / maxPoints);
        filteredData = filteredData.filter((_, index) => index % step === 0);
        console.log(`Sampled ${filteredData.length} points from ${fullDataset.length} total`);
    }

    // Convert to our format with proper coordinate transformation
    // Define UTM Zone 35S (EPSG:32735) - this is the CRS used in the R script
    proj4.defs("EPSG:32735", "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs");

    trajectoryData = filteredData.map(row => {
        // Use x_m and y_m which are in UTM meters
        const utmEasting = parseFloat(row.x_m);
        const utmNorthing = parseFloat(row.y_m);

        // Convert from UTM to WGS84 [longitude, latitude]
        const [lng, lat] = proj4('EPSG:32735', 'EPSG:4326', [utmEasting, utmNorthing]);

        return {
            lat: lat,
            lng: lng,
            behavior: row.behavior || 'Unknown',
            time: String(row.date || ''),
            zone: row.Zone || ''
        };
    }).filter(point => !isNaN(point.lat) && !isNaN(point.lng));

    console.log(`Processed ${trajectoryData.length} valid points`);

    if (trajectoryData.length === 0) {
        showDataStatus('error', 'No valid coordinate data found');
        return;
    }

    // Update UI
    renderTrajectory();
    updateStatistics();

    showDataStatus('success',
        `<strong>Data Loaded Successfully!</strong><br>` +
        `${trajectoryData.length} points for ${currentElephant} (${currentPeriod.toUpperCase()} period)<br>` +
        `<small>Date range: ${trajectoryData[0].time} to ${trajectoryData[trajectoryData.length - 1].time}</small>`
    );
}

// ===================================
// TRAJECTORY RENDERING
// ===================================
function renderTrajectory() {
    if (!trajectoryData || trajectoryData.length === 0) return;

    // Clear existing layers
    if (trajectoryLayer) {
        map.removeLayer(trajectoryLayer);
    }
    if (currentMarker) {
        map.removeLayer(currentMarker);
    }
    markerLayer.clearLayers();

    // Get active behavioral filters
    const activeFilters = getActiveFilters();
    const filteredData = trajectoryData.filter(point => activeFilters.includes(point.behavior));

    if (filteredData.length === 0) {
        showDataStatus('warning', 'No data matches current filters');
        return;
    }

    // Create polyline for trajectory
    const showTrail = document.getElementById('show-trail').checked;
    if (showTrail) {
        const points = filteredData.map(p => [p.lat, p.lng]);
        trajectoryLayer = L.polyline(points, {
            color: '#667eea',
            weight: 2,
            opacity: 0.6
        }).addTo(map);
    }

    // Add colored markers for each point (sample if too many)
    const markerStep = Math.max(1, Math.floor(filteredData.length / 200));
    filteredData.forEach((point, index) => {
        if (index % markerStep !== 0) return;

        const color = getBehaviorColor(point.behavior);
        const marker = L.circleMarker([point.lat, point.lng], {
            radius: 4,
            fillColor: color,
            color: '#fff',
            weight: 1,
            opacity: 1,
            fillOpacity: 0.7
        });

        marker.bindPopup(`
            <strong>${point.behavior}</strong><br>
            Time: ${point.time}<br>
            Zone: ${point.zone}<br>
            Lat: ${point.lat.toFixed(6)}<br>
            Lng: ${point.lng.toFixed(6)}
        `);

        markerLayer.addLayer(marker);
    });

    // Fit map to trajectory bounds
    if (trajectoryLayer) {
        map.fitBounds(trajectoryLayer.getBounds(), { padding: [50, 50] });
    } else if (filteredData.length > 0) {
        const bounds = L.latLngBounds(filteredData.map(p => [p.lat, p.lng]));
        map.fitBounds(bounds, { padding: [50, 50] });
    }

    // Reset animation
    currentIndex = 0;
    document.getElementById('time-slider').value = 0;
    updateCurrentPosition();
}

function getBehaviorColor(behavior) {
    const colors = {
        'Resting': '#3b82f6',
        'Foraging': '#10b981',
        'Movement': '#f59e0b'
    };
    return colors[behavior] || '#667eea';
}

function updateStatistics() {
    if (!trajectoryData || trajectoryData.length === 0) return;

    // Calculate behavior distribution
    const counts = { Resting: 0, Foraging: 0, Movement: 0 };
    trajectoryData.forEach(point => {
        if (counts.hasOwnProperty(point.behavior)) {
            counts[point.behavior]++;
        }
    });

    const total = counts.Resting + counts.Foraging + counts.Movement;
    const restingPct = Math.round((counts.Resting / total) * 100);
    const foragingPct = Math.round((counts.Foraging / total) * 100);
    const movementPct = 100 - restingPct - foragingPct;

    // Update UI
    document.getElementById('total-points').textContent = trajectoryData.length.toLocaleString();

    // Format date range safely
    const firstDate = String(trajectoryData[0].time).split('T')[0];
    const lastDate = String(trajectoryData[trajectoryData.length - 1].time).split('T')[0];
    const dateRange = `${firstDate} to ${lastDate}`;
    document.getElementById('date-range').textContent = dateRange;

    document.getElementById('resting-pct').textContent = `${restingPct}%`;
    document.getElementById('foraging-pct').textContent = `${foragingPct}%`;
    document.getElementById('movement-pct').textContent = `${movementPct}%`;

    // Update distribution bar
    document.querySelector('.bar-segment.resting').style.width = `${restingPct}%`;
    document.querySelector('.bar-segment.foraging').style.width = `${foragingPct}%`;
    document.querySelector('.bar-segment.movement').style.width = `${movementPct}%`;
}

// ===================================
// CONTROLS INITIALIZATION
// ===================================
function initializeControls() {
    // Elephant selection
    document.querySelectorAll('.elephant-btn').forEach(btn => {
        btn.addEventListener('click', () => {
            currentElephant = btn.dataset.elephant;
            updateElephantSelection();
            loadRealData();
        });
    });

    // Period selection
    document.querySelectorAll('.period-btn').forEach(btn => {
        btn.addEventListener('click', () => {
            const period = btn.dataset.period;

            // Check if period is available for this elephant
            const elephant = window.elephantPlatform.elephants[currentElephant];
            if (!elephant.periods.includes(period)) {
                showDataStatus('warning', `${period.toUpperCase()} period data not available for ${currentElephant}`);
                return;
            }

            currentPeriod = period;
            updatePeriodSelection();
            filterDataByPeriod();
        });
    });

    // Behavioral filters
    document.querySelectorAll('.filter-checkbox input').forEach(checkbox => {
        checkbox.addEventListener('change', () => {
            if (trajectoryData) {
                renderTrajectory();
            }
        });
    });

    // Max points selector
    document.getElementById('max-points').addEventListener('change', () => {
        if (fullDataset) {
            filterDataByPeriod();
        }
    });

    // Playback controls
    document.getElementById('play-btn').addEventListener('click', startAnimation);
    document.getElementById('pause-btn').addEventListener('click', pauseAnimation);
    document.getElementById('reset-btn').addEventListener('click', resetAnimation);

    // Speed control
    document.getElementById('speed-select').addEventListener('change', (e) => {
        if (isPlaying) {
            pauseAnimation();
            startAnimation();
        }
    });

    // Timeline slider
    document.getElementById('time-slider').addEventListener('input', (e) => {
        if (!trajectoryData) return;
        const value = parseInt(e.target.value);
        const maxIndex = trajectoryData.length - 1;
        currentIndex = Math.floor((value / 100) * maxIndex);
        updateCurrentPosition();
    });

    // Display options
    document.getElementById('show-fence').addEventListener('change', updateFenceDisplay);
    document.getElementById('show-trail').addEventListener('change', () => {
        if (trajectoryData) renderTrajectory();
    });
    document.getElementById('show-rsf').addEventListener('change', updateRSFDisplay);
}

// ===================================
// UI UPDATES
// ===================================
function updateElephantSelection() {
    document.querySelectorAll('.elephant-btn').forEach(btn => {
        btn.classList.toggle('active', btn.dataset.elephant === currentElephant);
    });

    // Update period availability
    const elephant = window.elephantPlatform.elephants[currentElephant];
    document.querySelectorAll('.period-btn').forEach(btn => {
        const period = btn.dataset.period;
        const isAvailable = elephant.periods.includes(period);
        btn.disabled = !isAvailable;

        if (!isAvailable && btn.classList.contains('active')) {
            currentPeriod = 'pre';
            updatePeriodSelection();
        }
    });
}

function updatePeriodSelection() {
    document.querySelectorAll('.period-btn').forEach(btn => {
        btn.classList.toggle('active', btn.dataset.period === currentPeriod);
    });

    const showFence = window.elephantPlatform.periods[currentPeriod].showFence;
    document.getElementById('show-fence').checked = showFence;
    updateFenceDisplay();
}

function showDataStatus(type, message) {
    const statusEl = document.getElementById('data-status');
    const iconEl = statusEl.querySelector('.status-icon');
    const messageEl = statusEl.querySelector('.status-message');

    const icons = {
        loading: '⏳',
        info: 'ℹ️',
        warning: '⚠️',
        error: '❌',
        success: '✅'
    };

    iconEl.textContent = icons[type] || icons.info;
    messageEl.innerHTML = message;
    statusEl.style.display = 'flex';
}

function getActiveFilters() {
    const filters = [];
    document.querySelectorAll('.filter-checkbox input:checked').forEach(checkbox => {
        filters.push(checkbox.dataset.behavior);
    });
    return filters;
}

// ===================================
// ANIMATION CONTROLS
// ===================================
function startAnimation() {
    if (!trajectoryData || trajectoryData.length === 0) {
        showDataStatus('warning', 'No trajectory data loaded');
        return;
    }

    isPlaying = true;
    document.getElementById('play-btn').style.display = 'none';
    document.getElementById('pause-btn').style.display = 'block';

    const speed = parseFloat(document.getElementById('speed-select').value);
    const interval = 1000 / speed;

    animationInterval = setInterval(() => {
        if (currentIndex >= trajectoryData.length - 1) {
            pauseAnimation();
            return;
        }

        currentIndex++;
        const progress = (currentIndex / (trajectoryData.length - 1)) * 100;
        document.getElementById('time-slider').value = progress;
        updateCurrentPosition();
    }, interval);
}

function pauseAnimation() {
    isPlaying = false;
    document.getElementById('play-btn').style.display = 'block';
    document.getElementById('pause-btn').style.display = 'none';

    if (animationInterval) {
        clearInterval(animationInterval);
        animationInterval = null;
    }
}

function resetAnimation() {
    pauseAnimation();
    currentIndex = 0;
    document.getElementById('time-slider').value = 0;
    updateCurrentPosition();
}

function updateCurrentPosition() {
    if (!trajectoryData || trajectoryData.length === 0) return;

    const point = trajectoryData[currentIndex];

    // Update time display
    document.getElementById('current-time').textContent = point.time;
    document.getElementById('current-behavior').textContent = point.behavior;

    // Remove old marker
    if (currentMarker) {
        map.removeLayer(currentMarker);
    }

    // Add pulsing marker at current position
    currentMarker = L.circleMarker([point.lat, point.lng], {
        radius: 12,
        fillColor: getBehaviorColor(point.behavior),
        color: '#fff',
        weight: 3,
        opacity: 1,
        fillOpacity: 0.9
    }).addTo(map);

    // Pan to current position
    map.panTo([point.lat, point.lng]);
}

// ===================================
// DISPLAY UPDATES
// ===================================
function updateFenceDisplay() {
    const showFence = document.getElementById('show-fence').checked;
    if (window.fenceLayer) {
        if (showFence) {
            map.addLayer(window.fenceLayer);
        } else {
            map.removeLayer(window.fenceLayer);
        }
    }
    console.log('Fence display:', showFence);
}

function updateRSFDisplay() {
    const showRSF = document.getElementById('show-rsf').checked;
    if (showRSF) {
        showDataStatus('info', 'RSF heatmap loading will be implemented with GeoTIFF.js');
    }
}

// ===================================
// EXPORTS
// ===================================
window.explorerApp = {
    map,
    loadRealData,
    startAnimation,
    pauseAnimation,
    resetAnimation
};

console.log('Explorer initialized with real CSV data loading!');
