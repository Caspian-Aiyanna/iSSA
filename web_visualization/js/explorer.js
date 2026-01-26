/**
 * Trajectory Explorer JavaScript - REAL DATA VERSION
 * Loads actual elephant behavioral data from CSV files
 */

// Elephant Metadata
const ELEPHANT_INFO = {
    'E1': { name: 'Kamva', gender: 'male' },
    'E2': { name: 'Kambaku', gender: 'male' },
    'E3': { name: 'Bukela', gender: 'female' },
    'E4': { name: 'Half Moon', gender: 'female' },
    'E5': { name: 'Beauty', gender: 'female' },
    'E6': { name: 'Balu', gender: 'male' }
};

// Global state
let map = null;
let currentElephant = null;
let activeElephants = [];
let currentPeriod = 'pre';
let trajectories = {}; // Store processed data for active elephants: { 'E1': [...] }
let rawDatasets = {};  // Store full raw datasets per elephant (NOW PRE-PROCESSED)
let trajectoryData = null; // Primary focused data (usually trajectories[currentElephant])
let fullDataset = null; // Primary raw dataset (legacy support)
let trajectoryLayer = null; // Primary background trajectory line
let currentMarker = null; // Primary marker (name bubble)
let markerLayer = null; // Layer for behavioral state dots
let layers = {
    trajectories: {}, // Multi-elephant background lines & history trails
    markers: {}       // Multi-elephant current markers (name bubbles)
};
let isPlaying = false;
let currentIndex = 0;
let animationInterval = null;
let animatedPath = null; // Focal white dotted history trail
let rsfLayer = null;
let metricsCharts = {
    speed: null,
    tortuosity: null
};

// ===================================
// INITIALIZATION
// ===================================
document.addEventListener('DOMContentLoaded', () => {
    initializeMap();
    initializeControls();
    initializeMetricsCharts();

    // Load state from URL parameters
    loadStateFromUrl();

    // Listen for URL changes
    window.addEventListener('popstate', () => {
        loadStateFromUrl();
        loadAllActiveTrajectories();
    });

    // Auto-load data
    if (activeElephants.length > 0) {
        setTimeout(() => {
            loadAllActiveTrajectories();
        }, 500);
    } else {
        showDataStatus('info', '<strong>Select an elephant</strong> to begin exploring trajectories.');
    }

    // Citation listener
    const citeBtn = document.getElementById('cite-btn');
    if (citeBtn) {
        citeBtn.addEventListener('click', generateCitation);
    }
});

function initializeMetricsCharts() {
    const ctxSpeed = document.getElementById('speed-chart').getContext('2d');
    const ctxTort = document.getElementById('tortuosity-chart').getContext('2d');

    const commonOptions = {
        responsive: true,
        maintainAspectRatio: false,
        elements: { point: { radius: 0 } },
        scales: {
            x: { display: false },
            y: {
                grid: { color: 'rgba(255, 255, 255, 0.05)' },
                ticks: { display: false }
            }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: true } }
    };

    metricsCharts.speed = new Chart(ctxSpeed, {
        type: 'line',
        data: { labels: [], datasets: [{ label: 'Speed', data: [], borderColor: '#667eea', borderWidth: 2, fill: true, backgroundColor: 'rgba(102, 126, 234, 0.1)' }] },
        options: { ...commonOptions, plugins: { ...commonOptions.plugins, title: { display: true, text: 'Speed (m/min)', color: '#94a3b8', font: { size: 10 } } } }
    });

    metricsCharts.tortuosity = new Chart(ctxTort, {
        type: 'line',
        data: { labels: [], datasets: [{ label: 'Turn Angle', data: [], borderColor: '#10B981', borderWidth: 2, fill: true, backgroundColor: 'rgba(16, 185, 129, 0.1)' }] },
        options: { ...commonOptions, plugins: { ...commonOptions.plugins, title: { display: true, text: 'Turning Angle (°)', color: '#94a3b8', font: { size: 10 } } } }
    });
}

function generateCitation() {
    if (!currentElephant) {
        alert('Please select an elephant first.');
        return;
    }
    const date = new Date().toLocaleDateString();
    const url = window.location.href;
    const citation = `Kariega Elephant Project (2025). Behavioral Dataset: ${currentElephant}, Period: ${currentPeriod.toUpperCase()}. Accessed via Interactive SSA Platform on ${date}. URL: ${url}`;
    navigator.clipboard.writeText(citation).then(() => {
        alert('Citation and Deep Link copied to clipboard!\n\n' + citation);
    });
}

// ===================================
// MAP INITIALIZATION
// ===================================
async function initializeMap() {
    const configResponse = await fetch('data/map_config.json');
    const mapConfig = await configResponse.json();

    map = L.map('map').setView([mapConfig.center.lat, mapConfig.center.lng], mapConfig.zoom);

    const osmLayer = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '© OpenStreetMap contributors', maxZoom: 18
    });

    const satellite = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: 'Tiles © Esri', maxZoom: 18
    }).addTo(map);

    const baseMaps = { "Satellite": satellite, "Street Map": osmLayer };
    L.control.layers(baseMaps).addTo(map);
    L.control.scale().addTo(map);

    markerLayer = L.layerGroup().addTo(map);

    try {
        const kwResponse = await fetch('data/kw_boundary.geojson');
        const kwData = await kwResponse.json();
        L.geoJSON(kwData, { style: { color: '#667eea', weight: 2, fillOpacity: 0.1 } }).addTo(map).bindPopup('KW Study Area');

        const hvResponse = await fetch('data/hv_boundary.geojson');
        const hvData = await hvResponse.json();
        L.geoJSON(hvData, { style: { color: '#764ba2', weight: 2, fillOpacity: 0.1 } }).addTo(map).bindPopup('HV Study Area');

        const fenceResponse = await fetch('data/fence_line.geojson');
        const fenceData = await fenceResponse.json();
        window.fenceData = fenceData;
        window.fenceLayer = L.geoJSON(fenceData, { style: { color: '#ef4444', weight: 3, opacity: 0.8 } }).addTo(map).bindPopup('Fence Line');
    } catch (e) { console.error('Error loading boundaries:', e); }

    const bounds = L.latLngBounds([mapConfig.bounds.south, mapConfig.bounds.west], [mapConfig.bounds.north, mapConfig.bounds.east]);
    map.fitBounds(bounds, { padding: [50, 50] });
    map.setMaxBounds(bounds);
    map.setMinZoom(10);
    setTimeout(() => map.invalidateSize(), 100);
}

// ===================================
// DATA LOADING & OPTIMIZATION
// ===================================
async function loadAllActiveTrajectories() {
    if (activeElephants.length === 0) {
        showDataStatus('info', '<strong>Select an elephant</strong> to begin.');
        renderTrajectory();
        return;
    }

    showDataStatus('loading', `Syncing trajectories...`);
    document.getElementById('loading').style.display = 'flex';

    try {
        for (const id of activeElephants) {
            if (!rawDatasets[id]) {
                await loadElephantData(id);
            }
        }

        if (currentElephant) fullDataset = rawDatasets[currentElephant];
        filterDataByPeriod();
        document.getElementById('loading').style.display = 'none';
        updateElephantSelection();
    } catch (error) {
        console.error('Error loading data:', error);
        showDataStatus('error', `Loading failed: ${error.message}`);
        document.getElementById('loading').style.display = 'none';
    }
}

async function loadElephantData(id) {
    const csvPath = `data/behavioral_points/${id}_behavioral_points.csv`;
    const response = await fetch(csvPath);
    if (!response.ok) throw new Error(`CSV not found for ${id}`);
    const csvText = await response.text();

    return new Promise((resolve, reject) => {
        Papa.parse(csvText, {
            header: true, dynamicTyping: true, skipEmptyLines: true,
            complete: (results) => {
                // OPTIMIZATION: Process coordinates and metrics ONCE during initial load
                proj4.defs("EPSG:32735", "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs");

                const processed = results.data.map((row, i) => {
                    const utmEasting = parseFloat(row.x_m);
                    const utmNorthing = parseFloat(row.y_m);
                    const [lng, lat] = proj4('EPSG:32735', 'EPSG:4326', [utmEasting, utmNorthing]);
                    const dateTime = new Date(row.date);

                    let speed = 0, turnAngle = 0;
                    if (i > 0) {
                        const prev = results.data[i - 1];
                        const dist = Math.sqrt(Math.pow(row.x_m - (prev.x_m || row.x_m), 2) + Math.pow(row.y_m - (prev.y_m || row.y_m), 2));
                        const timeDiff = (dateTime - new Date(prev.date)) / (1000 * 60);
                        speed = timeDiff > 0 ? (dist / timeDiff) : 0;
                        if (i > 1) {
                            const prevPrev = results.data[i - 2];
                            const angle1 = Math.atan2((prev.y_m || row.y_m) - (prevPrev.y_m || prev.y_m), (prev.x_m || row.x_m) - (prevPrev.x_m || prev.x_m));
                            const angle2 = Math.atan2(row.y_m - (prev.y_m || row.y_m), row.x_m - (prev.x_m || row.x_m));
                            turnAngle = (angle2 - angle1) * (180 / Math.PI);
                            if (turnAngle > 180) turnAngle -= 360;
                            if (turnAngle < -180) turnAngle += 360;
                        }
                    }

                    return {
                        lat, lng, x_m: utmEasting, y_m: utmNorthing,
                        behavior: (row.behavior === 'Resting' || row.state === 'Resting') ? 'Low-energy' : (row.behavior || 'Unknown'),
                        time: row.date, timestamp: !isNaN(dateTime) ? dateTime.getTime() : 0,
                        stage: (row.Stage || row.stage || '').toLowerCase(),
                        zone: row.Zone || '', speed, turnAngle: Math.abs(turnAngle)
                    };
                }).filter(p => !isNaN(p.lat));

                // Sort once
                processed.sort((a, b) => a.timestamp - b.timestamp);
                rawDatasets[id] = processed;
                resolve();
            },
            error: (err) => reject(err)
        });
    });
}

function filterDataByPeriod() {
    trajectories = {};
    activeElephants.forEach(id => {
        const dataset = rawDatasets[id];
        if (!dataset) return;

        // Filtering is now extremely fast because results are pre-processed
        let filtered = dataset.filter(p => p.stage === currentPeriod.toLowerCase());
        if (filtered.length === 0) return;

        const maxPointsSetting = document.getElementById('max-points').value;
        const maxPoints = maxPointsSetting === 'all' ? filtered.length : parseInt(maxPointsSetting);
        if (filtered.length > maxPoints) {
            const step = Math.floor(filtered.length / maxPoints);
            filtered = filtered.filter((_, index) => index % step === 0);
        }

        trajectories[id] = filtered;
    });

    if (currentElephant && trajectories[currentElephant]) {
        trajectoryData = trajectories[currentElephant];
    } else if (activeElephants.length > 0) {
        currentElephant = activeElephants[0];
        trajectoryData = trajectories[currentElephant];
    } else {
        trajectoryData = null;
    }

    if (activeElephants.length > 0 && !trajectoryData) {
        showDataStatus('warning', `No data found for ${currentPeriod.toUpperCase()}`);
        return;
    }

    renderTrajectory();
    updateStatistics();
    updateMetricsCharts();
    if (activeElephants.length > 0) {
        showDataStatus('success', `Loaded ${activeElephants.length} active trajectories`);
    }
}

// ===================================
// RENDERING
// ===================================
function renderTrajectory() {
    if (trajectoryLayer) map.removeLayer(trajectoryLayer);
    if (currentMarker) map.removeLayer(currentMarker);
    markerLayer.clearLayers();

    Object.values(layers.trajectories).forEach(l => map.removeLayer(l));
    Object.values(layers.markers).forEach(l => map.removeLayer(l));
    layers.trajectories = {};
    layers.markers = {};

    if (animatedPath) { map.removeLayer(animatedPath); animatedPath = null; }

    if (activeElephants.length === 0) {
        trajectoryData = null;
        updateStatistics();
        return;
    }

    const activeFilters = getActiveFilters();
    const showTrail = document.getElementById('show-trail').checked;

    const allBounds = L.latLngBounds([]);
    let hasData = false;

    activeElephants.forEach(id => {
        const data = trajectories[id];
        if (!data) return;
        const filtered = data.filter(p => activeFilters.includes(p.behavior));

        if (filtered.length > 0) {
            if (id === currentElephant && showTrail) {
                trajectoryLayer = L.polyline(filtered.map(p => [p.lat, p.lng]), {
                    color: '#667eea', weight: 2, opacity: 0.8
                }).addTo(map);
                allBounds.extend(trajectoryLayer.getBounds());
                hasData = true;
            } else if (showTrail) {
                layers.trajectories[id] = L.polyline(filtered.map(p => [p.lat, p.lng]), {
                    color: '#94a3b8', weight: 1.5, opacity: 0.5
                }).addTo(map);
                allBounds.extend(layers.trajectories[id].getBounds());
                hasData = true;
            }

            if (id === currentElephant) {
                // Respect the actual maxPoints setting for discrete markers
                const maxPointsSetting = document.getElementById('max-points').value;
                const markerLimit = maxPointsSetting === 'all' ? filtered.length : 2000;
                const step = Math.max(1, Math.floor(filtered.length / markerLimit));

                filtered.forEach((p, i) => {
                    if (i % step !== 0) return;
                    const m = L.circleMarker([p.lat, p.lng], { radius: 4, fillColor: getBehaviorColor(p.behavior), color: '#fff', weight: 1, fillOpacity: 0.7 });
                    m.bindPopup(`<strong>${p.behavior}</strong> (${ELEPHANT_INFO[id].name})<br>${new Date(p.timestamp).toLocaleString('en-ZA', { timeZone: 'Africa/Johannesburg' })}`);
                    markerLayer.addLayer(m);
                });
            }
        }
    });

    if (hasData) map.fitBounds(allBounds, { padding: [50, 50] });
    updateMetricsCharts();
    resetAnimation();
}

function updateCurrentPosition(forcePan = false) {
    if (!trajectoryData || activeElephants.length === 0) return;

    // Safety check for out-of-bounds index
    if (currentIndex < 0) currentIndex = 0;
    if (currentIndex >= trajectoryData.length) currentIndex = trajectoryData.length - 1;

    const primaryPoint = trajectoryData[currentIndex];
    if (!primaryPoint) return;

    if (currentMarker) map.removeLayer(currentMarker);
    const info = ELEPHANT_INFO[currentElephant];
    currentMarker = L.marker([primaryPoint.lat, primaryPoint.lng], {
        icon: L.divIcon({
            className: 'custom-div-icon',
            html: `<div class="name-bubble ${info.gender}">${info.name}</div>`,
            iconAnchor: [40, 30]
        })
    }).addTo(map);

    // OPTIMIZATION: Use a pre-mapped history array if possible or just slice
    const history = trajectoryData.slice(0, currentIndex + 1).map(p => [p.lat, p.lng]);
    const showTrail = document.getElementById('show-trail').checked;

    if (showTrail) {
        if (!animatedPath) {
            animatedPath = L.polyline(history, {
                color: '#ffffff', weight: 2, opacity: 0.8, dashArray: '5, 10'
            }).addTo(map);
        } else {
            // Only update if points changed
            animatedPath.setLatLngs(history);
        }
    } else if (animatedPath) {
        map.removeLayer(animatedPath);
        animatedPath = null;
    }

    const sastTime = new Date(primaryPoint.timestamp).toLocaleString('en-ZA', {
        timeZone: 'Africa/Johannesburg',
        dateStyle: 'medium',
        timeStyle: 'medium'
    });
    document.getElementById('current-time').textContent = sastTime;
    document.getElementById('current-behavior').textContent = primaryPoint.behavior;
    document.getElementById('current-behavior').style.background = getBehaviorColor(primaryPoint.behavior);
    document.getElementById('avg-speed').textContent = `${primaryPoint.speed.toFixed(1)} m/min`;

    if (window.fenceData && window.fenceData.features) {
        let minDist = Infinity;
        const latLng = L.latLng(primaryPoint.lat, primaryPoint.lng);

        // OPTIMIZATION: Only calculate distance every 5 points during animation for performance
        if (!isPlaying || currentIndex % 5 === 0) {
            window.fenceData.features.forEach(f => {
                const type = f.geometry.type;
                const coords = (type === 'LineString' || type === 'Polygon') ? [f.geometry.coordinates] : f.geometry.coordinates;
                coords.forEach(line => {
                    if (!Array.isArray(line)) return;
                    line.forEach(c => {
                        if (Array.isArray(c) && c.length >= 2) {
                            const d = latLng.distanceTo(L.latLng(c[1], c[0]));
                            if (d < minDist) minDist = d;
                        }
                    });
                });
            });
            const distText = minDist < 1000 ? `${Math.round(minDist)} m` : `${(minDist / 1000).toFixed(2)} km`;
            document.getElementById('fence-dist').textContent = distText;
        }
    }

    activeElephants.forEach(id => {
        if (id === currentElephant) return;
        if (layers.markers[id]) map.removeLayer(layers.markers[id]);

        const data = trajectories[id];
        if (!data || data.length === 0) return;

        let closestIndex = 0;
        let minT = Math.abs(data[0].timestamp - primaryPoint.timestamp);

        // Slightly smarter search if data is roughly sorted
        data.forEach((p, idx) => {
            const t = Math.abs(p.timestamp - primaryPoint.timestamp);
            if (t < minT) { minT = t; closestIndex = idx; }
        });

        const closest = data[closestIndex];
        if (!closest) return;

        if (minT < 3600000) { // Only show if within 1 hour
            const secInfo = ELEPHANT_INFO[id];
            layers.markers[id] = L.marker([closest.lat, closest.lng], {
                icon: L.divIcon({
                    className: 'custom-div-icon',
                    html: `<div class="name-bubble secondary ${secInfo.gender}">${secInfo.name}</div>`,
                    iconAnchor: [40, 30]
                })
            }).addTo(map);

            if (isPlaying) {
                const trailId = `trail_${id}`;
                const otherHistory = data.slice(0, closestIndex + 1).map(p => [p.lat, p.lng]);
                if (!layers.trajectories[trailId]) {
                    layers.trajectories[trailId] = L.polyline(otherHistory, {
                        color: '#ffffff', weight: 1.5, opacity: 0.5, dashArray: '5, 5'
                    }).addTo(map);
                } else {
                    layers.trajectories[trailId].setLatLngs(otherHistory);
                }
            } else {
                const trailId = `trail_${id}`;
                if (layers.trajectories[trailId]) { map.removeLayer(layers.trajectories[trailId]); delete layers.trajectories[trailId]; }
            }
        }
    });

    const nearbyNames = activeElephants.filter(id => id !== currentElephant && layers.markers[id]).map(id => ELEPHANT_INFO[id].name);
    document.getElementById('co-travel-list').textContent = nearbyNames.length > 0 ? nearbyNames.join(', ') : 'None nearby';

    // Smoothly update metrics highlight
    if (!isPlaying || currentIndex % 2 === 0) {
        updateMetricsHighlight(currentIndex);
    }

    if (isPlaying || forcePan) map.panTo([primaryPoint.lat, primaryPoint.lng], { animate: true });
}

// ===================================
// UI HELPERS
// ===================================
function getBehaviorColor(b) {
    const c = { 'Sleeping': '#999999', 'Low-energy': '#E69F00', 'Foraging': '#10B981', 'Movement': '#56B4E9', 'Bounce': '#E41A1C' };
    return c[b] || '#667eea';
}

function updateStatistics() {
    if (!trajectoryData) {
        document.getElementById('total-points').textContent = '0';
        document.getElementById('date-range').textContent = 'None';
        return;
    }
    const counts = { Sleeping: 0, 'Low-energy': 0, Foraging: 0, Movement: 0, Bounce: 0 };
    trajectoryData.forEach(p => { if (counts.hasOwnProperty(p.behavior)) counts[p.behavior]++; });
    const total = Object.values(counts).reduce((a, b) => a + b, 0);
    const getPct = (v) => total > 0 ? Math.round((v / total) * 100) : 0;

    document.getElementById('total-points').textContent = total.toLocaleString();
    const first = new Date(trajectoryData[0].timestamp), last = new Date(trajectoryData[trajectoryData.length - 1].timestamp);
    document.getElementById('date-range').textContent = `${first.toLocaleDateString()} - ${last.toLocaleDateString()}`;

    ['sleeping', 'resting', 'foraging', 'movement', 'bounce'].forEach(key => {
        const label = key === 'resting' ? 'Low-energy' : key.charAt(0).toUpperCase() + key.slice(1);
        const pct = getPct(counts[label] || 0);
        document.getElementById(`${key}-pct`).textContent = `${pct}%`;
        const bar = document.querySelector(`.bar-segment.${key}`);
        if (bar) bar.style.width = `${pct}%`;
    });
}

function initializeControls() {
    document.querySelectorAll('.elephant-selector .elephant-btn').forEach(btn => {
        btn.addEventListener('click', () => {
            const id = btn.dataset.elephant;
            if (activeElephants.includes(id)) {
                activeElephants = activeElephants.filter(x => x !== id);
                if (currentElephant === id) {
                    currentElephant = activeElephants.length > 0 ? activeElephants[activeElephants.length - 1] : null;
                }
            } else {
                activeElephants.push(id);
                currentElephant = id;
            }
            // UPDATE IMMEDIATELY for instant UI feedback
            updateElephantSelection();
            syncStateToUrl();
            loadAllActiveTrajectories();
        });
    });

    document.querySelectorAll('.period-btn').forEach(btn => {
        btn.addEventListener('click', () => {
            currentPeriod = btn.dataset.period;
            updatePeriodSelection();
            syncStateToUrl();
            filterDataByPeriod();
        });
    });

    document.querySelectorAll('.filter-checkbox input').forEach(cb => cb.addEventListener('change', () => {
        syncStateToUrl();
        renderTrajectory();
    }));

    const toggleAllBtn = document.getElementById('toggle-all-behaviors');
    if (toggleAllBtn) {
        toggleAllBtn.addEventListener('change', (e) => {
            const isChecked = e.target.checked;
            document.querySelectorAll('.behavior-filters-grid .filter-checkbox input').forEach(cb => {
                cb.checked = isChecked;
            });
            syncStateToUrl();
            renderTrajectory();
        });
    }

    document.getElementById('max-points').addEventListener('change', filterDataByPeriod);
    document.getElementById('play-btn').addEventListener('click', startAnimation);
    document.getElementById('pause-btn').addEventListener('click', pauseAnimation);
    // Unified control handling for Next/Previous/Reset
    const stopPlayback = () => {
        isPlaying = false;
        if (animationInterval) {
            clearInterval(animationInterval);
            animationInterval = null;
        }
        document.getElementById('play-btn').style.display = 'block';
        document.getElementById('pause-btn').style.display = 'none';
    };

    document.getElementById('prev-btn').addEventListener('click', () => {
        if (!trajectoryData || currentIndex <= 0) return;
        stopPlayback();
        currentIndex--;
        updateFromIndex(true);
    });

    document.getElementById('next-btn').addEventListener('click', () => {
        if (!trajectoryData || currentIndex >= trajectoryData.length - 1) return;
        stopPlayback();
        currentIndex++;
        updateFromIndex(true);
    });

    document.getElementById('reset-btn').addEventListener('click', () => {
        stopPlayback();
        currentIndex = 0;
        updateFromIndex(true);
    });

    const deselectBtn = document.getElementById('deselect-all-elephants');
    if (deselectBtn) {
        deselectBtn.addEventListener('click', () => {
            activeElephants = [];
            currentElephant = null;
            updateElephantSelection();
            syncStateToUrl();
            loadAllActiveTrajectories();
        });
    }

    document.getElementById('show-fence').addEventListener('change', updateFenceDisplay);
    document.getElementById('show-trail').addEventListener('change', renderTrajectory);

    const exportBtn = document.getElementById('export-map');
    if (exportBtn) {
        exportBtn.addEventListener('click', () => {
            alert('To export the map view, please use the Browser Print function (Ctrl+P) or a screen capture tool. High-resolution GIS export is available in the RSF Comparison page.');
        });
    }

    document.getElementById('time-slider').addEventListener('input', e => {
        if (!trajectoryData) return;
        currentIndex = Math.floor((e.target.value / 100) * (trajectoryData.length - 1));
        updateCurrentPosition();
    });
}

function updateElephantSelection() {
    document.querySelectorAll('.elephant-selector .elephant-btn').forEach(btn => {
        btn.classList.toggle('active', activeElephants.includes(btn.dataset.elephant));
    });
}

function updatePeriodSelection() {
    document.querySelectorAll('.period-btn').forEach(btn => btn.classList.toggle('active', btn.dataset.period === currentPeriod));
}

function showDataStatus(type, msg) {
    const el = document.getElementById('data-status');
    if (el) el.querySelector('.status-message').innerHTML = msg;
}

function getActiveFilters() {
    return Array.from(document.querySelectorAll('.filter-checkbox input:checked')).map(cb => cb.dataset.behavior);
}

function startAnimation() {
    if (isPlaying || !trajectoryData) return;
    isPlaying = true;
    document.getElementById('play-btn').style.display = 'none';
    document.getElementById('pause-btn').style.display = 'block';
    animationInterval = setInterval(() => {
        if (currentIndex >= trajectoryData.length - 1) { pauseAnimation(); return; }
        currentIndex++;
        updateFromIndex();
    }, 1000 / parseFloat(document.getElementById('speed-select').value));
}

function updateFromIndex(forcePan = false) {
    if (!trajectoryData) return;
    document.getElementById('time-slider').value = (currentIndex / (trajectoryData.length - 1)) * 100;
    updateCurrentPosition(forcePan);
}

function pauseAnimation() {
    if (!isPlaying) return; // Already paused

    isPlaying = false;
    document.getElementById('play-btn').style.display = 'block';
    document.getElementById('pause-btn').style.display = 'none';

    if (animationInterval) {
        clearInterval(animationInterval);
        animationInterval = null;
    }

    // Final UI sync after pausing
    updateCurrentPosition();
}

function resetAnimation() {
    isPlaying = false;
    if (animationInterval) {
        clearInterval(animationInterval);
        animationInterval = null;
    }
    document.getElementById('play-btn').style.display = 'block';
    document.getElementById('pause-btn').style.display = 'none';

    currentIndex = 0;
    updateFromIndex(true);
}

function updateMetricsHighlight(idx) {
    if (metricsCharts.speed && trajectoryData && metricsCharts.speed.data.datasets[0].data.length > idx) {
        try {
            metricsCharts.speed.setActiveElements([{ datasetIndex: 0, index: idx }]);
            metricsCharts.speed.update('none');
        } catch (e) {
            console.warn('Metrics highlight failed:', e);
        }
    }
}

function updateMetricsCharts() {
    if (!trajectoryData || !metricsCharts.speed || !metricsCharts.tortuosity) return;

    // Sample data if too large for performance
    let displayData = trajectoryData;
    const maxChartPoints = 500;
    if (trajectoryData.length > maxChartPoints) {
        const step = Math.floor(trajectoryData.length / maxChartPoints);
        displayData = trajectoryData.filter((_, i) => i % step === 0);
    }

    const labels = displayData.map(p => new Date(p.timestamp).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' }));
    const speeds = displayData.map(p => p.speed);
    const turns = displayData.map(p => p.turnAngle);

    metricsCharts.speed.data.labels = labels;
    metricsCharts.speed.data.datasets[0].data = speeds;
    metricsCharts.speed.update('none');

    metricsCharts.tortuosity.data.labels = labels;
    metricsCharts.tortuosity.data.datasets[0].data = turns;
    metricsCharts.tortuosity.update('none');
}



function updateFenceDisplay() {
    if (window.fenceLayer) {
        if (document.getElementById('show-fence').checked) map.addLayer(window.fenceLayer);
        else map.removeLayer(window.fenceLayer);
    }
}

// Deep Linking & State Management
function syncStateToUrl() {
    const params = new URLSearchParams();
    if (currentElephant) params.set('elephant', currentElephant);
    if (activeElephants.length > 0) params.set('active', activeElephants.join(','));
    params.set('period', currentPeriod);

    // Behaviors
    const behaviors = Array.from(document.querySelectorAll('.filter-checkbox input:checked'))
        .map(cb => cb.dataset.behavior);
    if (behaviors.length > 0) params.set('behaviors', behaviors.join(','));

    const newUrl = `${window.location.pathname}?${params.toString()}`;
    window.history.pushState({ path: newUrl }, '', newUrl);
}

function loadStateFromUrl() {
    const params = new URLSearchParams(window.location.search);

    if (params.has('elephant')) {
        currentElephant = params.get('elephant');
    }

    if (params.has('active')) {
        activeElephants = params.get('active').split(',');
    } else if (currentElephant) {
        activeElephants = [currentElephant];
    }

    if (params.has('period')) {
        currentPeriod = params.get('period');
        updatePeriodSelection();
    }

    if (params.has('behaviors')) {
        const behaviors = params.get('behaviors').split(',');
        document.querySelectorAll('.filter-checkbox input').forEach(cb => {
            cb.checked = behaviors.includes(cb.dataset.behavior);
        });
    }

    updateElephantSelection();
}
