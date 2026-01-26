// Behavioral Analysis JavaScript
// Handles data loading, processing, and visualization for elephant behavioral states

// Global state
let behavioralData = {};
let currentElephant = 'E1';
let currentPeriod = 'PRE';
let currentAnalysis = 'time-budget';
let currentBaciMode = 'absolute'; // 'absolute' or 'delta'
let currentSeasonalMode = 'percent'; // 'percent' or 'count'
let currentTemporalMode = 'percent'; // 'percent' or 'count'
let charts = {};
let heatmapInstance = null;
let dataCache = {}; // Cache for CSV data (Individual and Period-specific)
let populationCache = null; // High-level cache for ALL elephants (contains all stages)

// Helper to get date components in South African time (UTC+2)
function getSASTComponents(date) {
    if (!(date instanceof Date) || isNaN(date)) return { hour: 0, day: 1, month: 0, year: 2020 };

    // Use Intl API to extract components in SAST
    const options = { timeZone: 'Africa/Johannesburg', hour12: false };
    const parts = new Intl.DateTimeFormat('en-US', {
        ...options,
        year: 'numeric',
        month: 'numeric',
        day: 'numeric',
        hour: 'numeric'
    }).formatToParts(date);

    const mapped = {};
    parts.forEach(p => mapped[p.type] = p.value);

    return {
        hour: parseInt(mapped.hour) % 24,
        day: parseInt(mapped.day),
        month: parseInt(mapped.month) - 1, // JS 0-indexed months
        year: parseInt(mapped.year)
    };
}

// Initialize on page load
document.addEventListener('DOMContentLoaded', () => {
    setChartDefaults();
    initializeEventListeners();

    // Load state from URL parameters before initial data load
    loadStateFromUrl();

    loadBehavioralData(currentElephant, currentPeriod);

    // Listen for URL changes (back/forward buttons)
    window.addEventListener('popstate', () => {
        loadStateFromUrl();
        loadBehavioralData(currentElephant, currentPeriod);
    });

    // Listen for theme changes to update chart colors
    window.addEventListener('themeChanged', (e) => {
        setChartDefaults();
        updateVisualization();
    });
});

function setChartDefaults() {
    const isLight = document.documentElement.getAttribute('data-theme') === 'light';
    const textColor = isLight ? '#334155' : '#e2e8f0';
    const gridColor = isLight ? 'rgba(0, 0, 0, 0.05)' : 'rgba(255, 255, 255, 0.1)';

    Chart.defaults.color = textColor;
    Chart.defaults.font.family = "'Inter', sans-serif";
    Chart.defaults.plugins.tooltip.backgroundColor = isLight ? 'rgba(255, 255, 255, 0.9)' : 'rgba(30, 41, 59, 0.9)';
    Chart.defaults.plugins.tooltip.titleColor = textColor;
    Chart.defaults.plugins.tooltip.bodyColor = textColor;
    Chart.defaults.plugins.tooltip.borderColor = gridColor;
    Chart.defaults.plugins.tooltip.borderWidth = 1;

    // Smoother animations
    Chart.defaults.animation = {
        duration: 1000,
        easing: 'easeOutQuart'
    };
}

// Event Listeners
function initializeEventListeners() {
    // Elephant selection
    document.querySelectorAll('.elephant-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.elephant-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentElephant = e.target.dataset.elephant;
            syncStateToUrl();
            loadBehavioralData(currentElephant, currentPeriod);
        });
    });

    // Period selection
    document.querySelectorAll('.period-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.period-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentPeriod = e.target.dataset.period;
            syncStateToUrl();
            loadBehavioralData(currentElephant, currentPeriod);
        });
    });

    // Analysis type selection
    document.querySelectorAll('.analysis-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.analysis-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentAnalysis = e.target.dataset.analysis;
            syncStateToUrl();
            switchAnalysisView(currentAnalysis);
        });
    });

    // Download chart
    document.getElementById('download-chart').addEventListener('click', downloadCurrentChart);

    // Print report
    const printBtn = document.getElementById('print-report');
    if (printBtn) {
        printBtn.addEventListener('click', () => {
            window.print();
        });
    }

    // Copy Citation Link
    const copyBtn = document.getElementById('copy-link');
    if (copyBtn) {
        copyBtn.addEventListener('click', () => {
            navigator.clipboard.writeText(window.location.href).then(() => {
                const originalText = copyBtn.textContent;
                copyBtn.textContent = 'Copied!';
                setTimeout(() => copyBtn.textContent = originalText, 2000);
            });
        });
    }

    // Global year filter (applies to all analysis types)
    document.getElementById('global-year-select').addEventListener('change', () => {
        updateVisualization();
    });

    // Seasonal pattern controls (month filter only for seasonal)
    document.getElementById('year-select').addEventListener('change', () => {
        if (currentAnalysis === 'seasonal') renderSeasonalPatterns();
    });

    document.getElementById('month-select').addEventListener('change', () => {
        if (currentAnalysis === 'seasonal') renderSeasonalPatterns();
    });

    // BACI Mode toggle
    document.querySelectorAll('.mode-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.mode-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentBaciMode = e.target.dataset.mode;
            if (currentAnalysis === 'comparison') renderPeriodComparison();
        });
    });

    document.querySelectorAll('.seasonal-mode-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.seasonal-mode-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentSeasonalMode = e.target.dataset.mode;
            syncStateToUrl();
            if (currentAnalysis === 'seasonal') renderSeasonalPatterns();
        });
    });

    document.querySelectorAll('.temporal-mode-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.temporal-mode-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentTemporalMode = e.target.dataset.mode;
            syncStateToUrl();
            if (currentAnalysis === 'temporal') renderTemporalPattern();
        });
    });
}

// Deep Linking & State Management
function syncStateToUrl() {
    const params = new URLSearchParams();
    params.set('elephant', currentElephant);
    params.set('period', currentPeriod);
    params.set('analysis', currentAnalysis);
    params.set('seasonalMode', currentSeasonalMode);
    params.set('temporalMode', currentTemporalMode);

    const newUrl = `${window.location.pathname}?${params.toString()}`;
    window.history.pushState({ path: newUrl }, '', newUrl);
}

function loadStateFromUrl() {
    const params = new URLSearchParams(window.location.search);

    if (params.has('elephant')) {
        currentElephant = params.get('elephant');
        // Update UI
        document.querySelectorAll('.elephant-btn').forEach(btn => {
            btn.classList.toggle('active', btn.dataset.elephant === currentElephant);
        });
    }

    if (params.has('period')) {
        currentPeriod = params.get('period');
        // Update UI
        document.querySelectorAll('.period-btn').forEach(btn => {
            btn.classList.toggle('active', btn.dataset.period === currentPeriod);
        });
    }

    if (params.has('analysis')) {
        currentAnalysis = params.get('analysis');
        // Update UI
        document.querySelectorAll('.analysis-btn').forEach(btn => {
            btn.classList.toggle('active', btn.dataset.analysis === currentAnalysis);
        });
        switchAnalysisView(currentAnalysis);
    }

    if (params.has('seasonalMode')) {
        currentSeasonalMode = params.get('seasonalMode');
        document.querySelectorAll('.seasonal-mode-btn').forEach(btn => {
            btn.classList.toggle('active', btn.dataset.mode === currentSeasonalMode);
        });
    }

    if (params.has('temporalMode')) {
        currentTemporalMode = params.get('temporalMode');
        document.querySelectorAll('.temporal-mode-btn').forEach(btn => {
            btn.classList.toggle('active', btn.dataset.mode === currentTemporalMode);
        });
    }
}

// Load behavioral data
async function loadBehavioralData(elephant, period) {
    showLoading(true);

    try {
        if (elephant === 'ALL' || elephant === 'MALES' || elephant === 'FEMALES') {
            await loadAllElephants(period, elephant);
        } else {
            await loadSingleElephant(elephant, period);
        }

        // UI updates are handled within the specific load functions for stability
        showLoading(false);
    } catch (error) {
        console.error('Error loading behavioral data:', error);
        showLoading(false);
        alert('Error loading data. Please check the console for details.');
    }
}

// Load single elephant data
async function loadSingleElephant(elephant, period) {
    const cacheKey = `${elephant}_${period}`;

    // Check cache first
    if (dataCache[cacheKey]) {
        console.log(`Using cached data for ${elephant} (${period})`);
        behavioralData = {
            elephant: elephant,
            period: period,
            data: dataCache[cacheKey].data,
            summary: dataCache[cacheKey].summary
        };
        updateStatistics(behavioralData.summary);
        updateVisualization();
        return;
    }

    const csvPath = `data/behavioral_points/${elephant}_behavioral_points.csv`;

    return new Promise((resolve, reject) => {
        Papa.parse(csvPath, {
            download: true,
            header: true,
            dynamicTyping: true,
            complete: (results) => {
                let data = results.data.filter(row => row.x_m && row.y_m);

                // Filter by period if not ALL
                if (period !== 'ALL') {
                    data = data.filter(row => {
                        const rowStage = (row.Stage || row.stage || '').trim().toUpperCase();
                        return rowStage === period.trim().toUpperCase();
                    });
                }

                const summary = calculateSummary(data);

                // Save to cache
                dataCache[cacheKey] = {
                    data: data,
                    summary: summary
                };

                behavioralData = {
                    elephant: elephant,
                    period: period,
                    data: data,
                    summary: summary
                };

                updateStatistics(behavioralData.summary);
                updateVisualization();
                resolve();
            },
            error: (error) => reject(error)
        });
    });
}

// Load aggregate data (All, Males, or Females)
async function loadAllElephants(period, mode = 'ALL') {
    const cacheKey = `${mode}_${period}`;

    // Check high-level population cache first
    if (populationCache) {
        console.log(`Using population cache for ${mode} period: ${period}`);
        let filteredData = populationCache;

        // Filter by sex if requested
        if (mode === 'MALES') {
            filteredData = populationCache.filter(row => ['E1', 'E2', 'E6'].includes(row.elephant_id));
        } else if (mode === 'FEMALES') {
            filteredData = populationCache.filter(row => ['E3', 'E4', 'E5'].includes(row.elephant_id));
        }

        // Filter by period
        if (period !== 'ALL') {
            filteredData = filteredData.filter(row => {
                const rowStage = (row.Stage || row.stage || '').trim().toUpperCase();
                return rowStage === period.toUpperCase();
            });
        }

        const summary = calculateSummary(filteredData);
        behavioralData = {
            elephant: mode,
            period: period,
            data: filteredData,
            summary: summary
        };
        updateStatistics(behavioralData.summary);
        updateVisualization();
        return;
    }

    const elephants = mode === 'MALES' ? ['E1', 'E2', 'E6'] :
        mode === 'FEMALES' ? ['E3', 'E4', 'E5'] :
            ['E1', 'E2', 'E3', 'E4', 'E5', 'E6'];
    let allData = [];

    const loadPromises = elephants.map(elephant => {
        const csvPath = `data/behavioral_points/${elephant}_behavioral_points.csv`;
        return new Promise((resolve, reject) => {
            Papa.parse(csvPath, {
                download: true,
                header: true,
                dynamicTyping: true,
                complete: (results) => {
                    const data = results.data.filter(row => row.x_m && row.y_m);
                    data.forEach(row => row.elephant_id = elephant);
                    resolve(data);
                },
                error: (error) => reject(error)
            });
        });
    });

    try {
        const results = await Promise.all(loadPromises);
        results.forEach(data => {
            allData = allData.concat(data);
        });

        // Only populate full population cache if we loaded everyone
        if (mode === 'ALL') {
            populationCache = allData;
        }

        const summary = calculateSummary(allData);

        behavioralData = {
            elephant: mode,
            period: period,
            data: allData,
            summary: summary
        };

        updateStatistics(behavioralData.summary);
        updateVisualization();
    } catch (error) {
        console.error('Population load failed:', error);
        throw error;
    }
}

// Calculate summary statistics
function calculateSummary(data) {
    const total = data.length;
    const behaviors = {
        'Sleeping': 0,
        'Low-energy': 0,
        'Foraging': 0,
        'Movement': 0,
        'Bounce': 0
    };

    data.forEach(row => {
        // Case-insensitive behavior matching
        let rawBehavior = row.behavior || row.Behavior || row.state || '';
        let behavior = rawBehavior.charAt(0).toUpperCase() + rawBehavior.slice(1).toLowerCase();

        if (behavior === 'Resting') behavior = 'Low-energy';

        if (behaviors.hasOwnProperty(behavior)) {
            behaviors[behavior]++;
        }
    });

    // Avoid stack overflow by using loop instead of spread operator
    const dates = data.map(row => new Date(row.date || row.Date)).filter(d => !isNaN(d));
    let minDate = null;
    let maxDate = null;

    if (dates.length > 0) {
        minDate = dates[0];
        maxDate = dates[0];
        for (let i = 1; i < dates.length; i++) {
            if (dates[i] < minDate) minDate = dates[i];
            if (dates[i] > maxDate) maxDate = dates[i];
        }
    }

    const duration = minDate && maxDate ? Math.ceil((maxDate - minDate) / (1000 * 60 * 60 * 24)) : 0;

    // Formatting date range for display in SAST
    const formatDate = (d) => d ? d.toLocaleDateString('en-ZA', { timeZone: 'Africa/Johannesburg' }) : 'N/A';

    return {
        total,
        behaviors,
        percentages: {
            sleeping: total > 0 ? ((behaviors.Sleeping / total) * 100).toFixed(1) : 0,
            resting: total > 0 ? ((behaviors['Low-energy'] / total) * 100).toFixed(1) : 0,
            foraging: total > 0 ? ((behaviors.Foraging / total) * 100).toFixed(1) : 0,
            movement: total > 0 ? ((behaviors.Movement / total) * 100).toFixed(1) : 0,
            bounce: total > 0 ? ((behaviors.Bounce / total) * 100).toFixed(1) : 0
        },
        dateRange: {
            min: minDate,
            max: maxDate
        },
        duration
    };
}

// Update statistics display
function updateStatistics(summary) {
    document.getElementById('total-points').textContent = summary.total.toLocaleString();
    document.getElementById('sleeping-pct').textContent = `${summary.percentages.sleeping}%`;
    document.getElementById('resting-pct').textContent = `${summary.percentages.resting}%`;
    document.getElementById('foraging-pct').textContent = `${summary.percentages.foraging}%`;
    document.getElementById('movement-pct').textContent = `${summary.percentages.movement}%`;
    document.getElementById('bounce-pct').textContent = `${summary.percentages.bounce}%`;

    const formatDate = (d) => d ? d.toLocaleDateString('en-ZA', { timeZone: 'Africa/Johannesburg' }) : 'N/A';
    if (summary.dateRange.min && summary.dateRange.max) {
        document.getElementById('date-range').textContent = `${formatDate(summary.dateRange.min)} - ${formatDate(summary.dateRange.max)}`;
    } else {
        document.getElementById('date-range').textContent = '-';
    }

    document.getElementById('duration-days').textContent = summary.duration > 0 ? summary.duration : '-';

    // Update rest-forage ratio
    const restingCount = summary.behaviors['Low-energy'] || 0;
    const foragingCount = summary.behaviors.Foraging || 0;
    const ratio = foragingCount > 0 ? (restingCount / foragingCount).toFixed(2) : '-';
    const ratioEl = document.getElementById('rest-forage-ratio');
    if (ratioEl) ratioEl.textContent = ratio;

    // Update elephant profile card
    updateElephantProfile();

    // Populate global year filter
    populateGlobalYearFilter();
}

// Populate global year filter based on current elephant's data
function populateGlobalYearFilter() {
    const yearSelect = document.getElementById('global-year-select');
    const currentValue = yearSelect.value;

    // Clear and repopulate
    yearSelect.innerHTML = '<option value="all">All Years</option>';

    const years = new Set();
    behavioralData.data.forEach(row => {
        const date = new Date(row.date || row.Date);
        if (!isNaN(date)) {
            years.add(getSASTComponents(date).year);
        }
    });

    const sortedYears = Array.from(years).sort();
    sortedYears.forEach(year => {
        const option = document.createElement('option');
        option.value = year;
        option.textContent = year;
        yearSelect.appendChild(option);
    });

    // Restore selection if valid
    if (sortedYears.includes(parseInt(currentValue))) {
        yearSelect.value = currentValue;
    } else {
        yearSelect.value = 'all';
    }

    console.log(`[Global Year Filter] Populated with years: ${sortedYears.join(', ')}`);
}

// Get filtered data based on global year filter
function getFilteredData() {
    const selectedYear = document.getElementById('global-year-select').value;

    if (selectedYear === 'all') {
        return behavioralData.data;
    }

    return behavioralData.data.filter(row => {
        const date = new Date(row.date || row.Date);
        return !isNaN(date) && getSASTComponents(date).year === parseInt(selectedYear);
    });
}

// Update elephant profile card
function updateElephantProfile() {
    const elephantData = {
        'E1': { name: 'Kamva (E1)', image: 'Kamva_1.png', preRange: 'Kariega West', badge: 'KW → Kariega Game Reserve' },
        'E2': { name: 'Kambaku (E2)', image: 'Kambaku_1.jpg', preRange: 'Kariega West', badge: 'KW → Kariega Game Reserve' },
        'E3': { name: 'Bukela (E3)', image: 'Bukela_1.jpg', preRange: 'Kariega West', badge: 'KW → Kariega Game Reserve' },
        'E4': { name: 'Half Moon (E4)', image: 'Half_moon_1.jpg', preRange: 'Kariega West', badge: 'KW → Kariega Game Reserve' },
        'E5': { name: 'Beauty (E5)', image: 'Beauty_1.jpg', preRange: 'Harvestvale', badge: 'HV → Kariega Game Reserve' },
        'E6': { name: 'Balu (E6)', image: 'Balu_1.jpg', preRange: 'Harvestvale', badge: 'HV → Kariega Game Reserve' },
        'MALES': { name: 'All Males (E1, E2, E6)', image: 'elephant_main.jpg', preRange: 'Both Ranges', badge: 'Sex Aggregate' },
        'FEMALES': { name: 'All Females (E3, E4, E5)', image: 'elephant_main.jpg', preRange: 'Both Ranges', badge: 'Sex Aggregate' },
        'ALL': { name: 'All Elephants', image: 'elephant_main.jpg', preRange: 'Both Ranges', badge: 'Combined Data' }
    };

    const elephant = elephantData[currentElephant];
    if (elephant) {
        const imgEl = document.getElementById('profile-img');
        imgEl.src = `elephants/${elephant.image}`;
        imgEl.onerror = () => { imgEl.src = 'elephants/elephant_main.jpg'; }; // Fallback
        document.getElementById('profile-name').textContent = elephant.name;
        document.getElementById('profile-badge').textContent = elephant.badge;

        // Determine home range based on period
        let homeRange;
        if (currentPeriod === 'PRE') {
            homeRange = elephant.preRange;
        } else if (currentPeriod === 'INTERIM' || currentPeriod === 'POST') {
            const aggTypes = ['ALL', 'MALES', 'FEMALES'];
            homeRange = aggTypes.includes(currentElephant) ? 'Both Ranges' : 'Kariega Game Reserve';
        } else {
            homeRange = 'Multiple Ranges';
        }

        document.getElementById('profile-range').textContent = homeRange;
        document.getElementById('profile-period').textContent = currentPeriod;

        // Update data points
        if (behavioralData && behavioralData.summary) {
            document.getElementById('profile-points').textContent = behavioralData.summary.total.toLocaleString();
        }
    }
}

// Switch analysis view
function switchAnalysisView(analysisType) {
    // Update current analysis state - CRITICAL FIX
    currentAnalysis = analysisType;

    // Hide all containers
    document.querySelectorAll('.viz-container').forEach(container => {
        container.classList.add('hidden');
    });

    // Update title
    const titles = {
        'time-budget': 'Time Budget Analysis',
        'seasonal': 'Seasonal Behavioral Patterns',
        'temporal': 'Temporal Activity Pattern',
        'comparison': 'Period Comparison'
    };
    document.getElementById('viz-title').textContent = titles[analysisType];

    // BACI mode toggle visibility
    const baciContainer = document.getElementById('baci-mode-container');
    if (baciContainer) {
        if (analysisType === 'comparison') {
            baciContainer.classList.remove('hidden');
        } else {
            baciContainer.classList.add('hidden');
        }
    }

    // Seasonal mode toggle visibility
    const seasonalModeContainer = document.getElementById('seasonal-toggle-container');
    if (seasonalModeContainer) {
        if (analysisType === 'seasonal') {
            seasonalModeContainer.classList.remove('hidden');
        } else {
            seasonalModeContainer.classList.add('hidden');
        }
    }

    // Temporal mode toggle visibility
    const temporalModeContainer = document.getElementById('temporal-toggle-container');
    if (temporalModeContainer) {
        if (analysisType === 'temporal') {
            temporalModeContainer.classList.remove('hidden');
        } else {
            temporalModeContainer.classList.add('hidden');
        }
    }

    // Show selected container
    const containerMap = {
        'time-budget': 'time-budget-container',
        'seasonal': 'seasonal-container',
        'temporal': 'temporal-container',
        'comparison': 'comparison-container'
    };
    const targetId = containerMap[analysisType];
    const targetElement = document.getElementById(targetId);
    if (targetElement) {
        targetElement.classList.remove('hidden');
    }

    // Update visualization with a small delay to ensure DOM is ready
    requestAnimationFrame(() => {
        updateVisualization();
    });
}

// Update visualization based on current analysis type
function updateVisualization() {
    // Check if we have data to work with
    if (!behavioralData.data) return;

    switch (currentAnalysis) {
        case 'time-budget':
            renderTimeBudgetChart();
            break;
        case 'seasonal':
            renderSeasonalPatterns();
            break;
        case 'temporal':
            renderTemporalPattern();
            break;
        case 'comparison':
            renderPeriodComparison();
            break;
    }
}

// Render time budget pie chart
function renderTimeBudgetChart() {
    const ctx = document.getElementById('time-budget-chart').getContext('2d');

    // Destroy existing chart
    if (charts.timeBudget) {
        charts.timeBudget.destroy();
    }

    // Use filtered data based on year selection
    const filteredData = getFilteredData();

    // Recalculate summary for filtered data
    const behaviorCounts = {
        Sleeping: 0,
        'Low-energy': 0,
        Foraging: 0,
        Movement: 0,
        Bounce: 0
    };

    filteredData.forEach(row => {
        let behavior = row.behavior || row.Behavior || row.state;
        if (behavior === 'Resting') behavior = 'Low-energy';

        if (behaviorCounts[behavior] !== undefined) {
            behaviorCounts[behavior]++;
        }
    });

    charts.timeBudget = new Chart(ctx, {
        type: 'doughnut',
        data: {
            labels: ['Sleeping', 'Low-energy', 'Foraging', 'Movement', 'Bounce'],
            datasets: [{
                data: [
                    behaviorCounts.Sleeping,
                    behaviorCounts['Low-energy'],
                    behaviorCounts.Foraging,
                    behaviorCounts.Movement,
                    behaviorCounts.Bounce
                ],
                backgroundColor: [
                    'rgba(153, 153, 153, 0.8)', // Sleeping - grey
                    'rgba(230, 159, 0, 0.8)',   // Low-energy - orange
                    'rgba(16, 185, 129, 0.8)',   // Foraging - green
                    'rgba(86, 180, 233, 0.8)',   // Movement - light blue
                    'rgba(228, 26, 28, 0.8)'    // Bounce - red
                ],
                borderColor: [
                    'rgba(153, 153, 153, 1)',
                    'rgba(230, 159, 0, 1)',
                    'rgba(16, 185, 129, 1)',
                    'rgba(86, 180, 233, 1)',
                    'rgba(228, 26, 28, 1)'
                ],
                borderWidth: 2
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    position: 'bottom',
                    labels: {
                        color: '#e2e8f0',
                        font: {
                            size: 16,
                            family: 'Inter',
                            weight: '500'
                        },
                        padding: 30
                    }
                },
                title: {
                    display: true,
                    text: `${behavioralData.elephant} - ${behavioralData.period} Period`,
                    color: '#f1f5f9',
                    font: {
                        size: 20,
                        family: 'Inter',
                        weight: 'bold'
                    },
                    padding: 20
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            const label = context.label || '';
                            const value = context.parsed || 0;
                            const total = context.dataset.data.reduce((a, b) => a + b, 0);
                            const percentage = ((value / total) * 100).toFixed(1);
                            return `${label}: ${value.toLocaleString()} points (${percentage}%)`;
                        }
                    }
                }
            },
            cutout: '40%'
        }
    });
}

// Render seasonal patterns
function renderSeasonalPatterns() {
    const ctx = document.getElementById('seasonal-chart').getContext('2d');

    // Destroy existing chart
    if (charts.seasonal) {
        charts.seasonal.destroy();
    }

    // Get selected year and month
    const selectedYear = document.getElementById('year-select').value;
    const selectedMonth = document.getElementById('month-select').value;

    // ALWAYS repopulate year selector based on current elephant's data
    const yearSelect = document.getElementById('year-select');
    const currentYearValue = yearSelect.value; // Save current selection

    // Clear all options except "All Years"
    yearSelect.innerHTML = '<option value="all">All Years</option>';

    // Get unique years from current elephant's data in SAST
    const years = new Set();
    behavioralData.data.forEach(row => {
        const date = new Date(row.date || row.Date);
        if (!isNaN(date)) {
            years.add(getSASTComponents(date).year);
        }
    });

    // Add years in sorted order
    const sortedYears = Array.from(years).sort();

    // DEBUG: Log year population
    console.log(`[Seasonal Patterns] Elephant: ${behavioralData.elephant}, Period: ${behavioralData.period}`);
    console.log(`[Seasonal Patterns] Total data points: ${behavioralData.data.length}`);
    console.log(`[Seasonal Patterns] Years found in data: ${sortedYears.join(', ')}`);

    sortedYears.forEach(year => {
        const option = document.createElement('option');
        option.value = year;
        option.textContent = year;
        yearSelect.appendChild(option);
    });

    // Restore selection if it still exists, otherwise reset to "all"
    if (sortedYears.includes(parseInt(currentYearValue))) {
        yearSelect.value = currentYearValue;
    } else {
        yearSelect.value = 'all';
    }

    console.log(`[Seasonal Patterns] Year selector populated with: ${sortedYears.length} years`);

    // Filter data by year and month
    let filteredData = behavioralData.data;

    if (yearSelect.value !== 'all') {
        filteredData = filteredData.filter(row => {
            const date = new Date(row.date || row.Date);
            return !isNaN(date) && getSASTComponents(date).year === parseInt(yearSelect.value);
        });
    }

    if (selectedMonth !== 'all') {
        filteredData = filteredData.filter(row => {
            const date = new Date(row.date || row.Date);
            return !isNaN(date) && (getSASTComponents(date).month + 1) === parseInt(selectedMonth);
        });
    }

    // Group data by month
    const monthlyData = Array(12).fill(0).map(() => ({
        Sleeping: 0,
        'Low-energy': 0,
        Foraging: 0,
        Movement: 0,
        Bounce: 0
    }));

    filteredData.forEach(row => {
        const date = new Date(row.date || row.Date);
        if (!isNaN(date)) {
            const month = getSASTComponents(date).month;
            let behavior = row.behavior || row.Behavior || row.state;
            if (behavior === 'Resting') behavior = 'Low-energy';

            if (monthlyData[month][behavior] !== undefined) {
                monthlyData[month][behavior]++;
            }
        }
    });

    // Calculate summary statistics
    const behaviorCounts = {
        Sleeping: filteredData.filter(r => (r.behavior || r.Behavior || r.state) === 'Sleeping').length,
        'Low-energy': filteredData.filter(r => {
            const b = r.behavior || r.Behavior || r.state;
            return b === 'Resting' || b === 'Low-energy';
        }).length,
        Foraging: filteredData.filter(r => (r.behavior || r.Behavior || r.state) === 'Foraging').length,
        Movement: filteredData.filter(r => (r.behavior || r.Behavior || r.state) === 'Movement').length,
        Bounce: filteredData.filter(r => (r.behavior || r.Behavior || r.state) === 'Bounce').length
    };

    const dominantBehavior = Object.keys(behaviorCounts).reduce((a, b) =>
        behaviorCounts[a] > behaviorCounts[b] ? a : b
    );

    // Update summary
    const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
    let periodText = 'All Data';
    if (selectedYear !== 'all' && selectedMonth !== 'all') {
        periodText = `${monthNames[parseInt(selectedMonth) - 1]} ${selectedYear}`;
    } else if (selectedYear !== 'all') {
        periodText = selectedYear;
    } else if (selectedMonth !== 'all') {
        periodText = monthNames[parseInt(selectedMonth) - 1];
    }

    document.getElementById('seasonal-period-text').textContent = periodText;
    document.getElementById('seasonal-obs').textContent = filteredData.length.toLocaleString();
    document.getElementById('seasonal-dominant').textContent = dominantBehavior;

    // Render chart
    charts.seasonal = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: monthNames,
            datasets: [
                {
                    label: 'Sleeping',
                    data: monthlyData.map(m => m.Sleeping),
                    backgroundColor: 'rgba(153, 153, 153, 0.8)',
                    borderColor: 'rgba(153, 153, 153, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Low-energy',
                    data: monthlyData.map(m => m['Low-energy']),
                    backgroundColor: 'rgba(230, 159, 0, 0.8)',
                    borderColor: 'rgba(230, 159, 0, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Foraging',
                    data: monthlyData.map(m => m.Foraging),
                    backgroundColor: 'rgba(16, 185, 129, 0.8)',
                    borderColor: 'rgba(16, 185, 129, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Movement',
                    data: monthlyData.map(m => m.Movement),
                    backgroundColor: 'rgba(86, 180, 233, 0.8)',
                    borderColor: 'rgba(86, 180, 233, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Bounce',
                    data: monthlyData.map(m => m.Bounce),
                    backgroundColor: 'rgba(228, 26, 28, 0.8)',
                    borderColor: 'rgba(228, 26, 28, 1)',
                    borderWidth: 1
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            scales: {
                x: {
                    stacked: true,
                    grid: { color: 'rgba(255, 255, 255, 0.1)' },
                    title: {
                        display: true,
                        text: 'Month',
                        color: '#94a3b8'
                    }
                },
                x2: {
                    type: 'category',
                    labels: [
                        '', 'SUMMER', '',
                        '', 'AUTUMN', '',
                        '', 'WINTER', '',
                        '', 'SPRING', ''
                    ],
                    grid: { display: false },
                    ticks: {
                        color: '#94a3b8',
                        font: {
                            size: 13,
                            weight: '700',
                            family: 'Inter'
                        },
                        padding: 5
                    },
                    position: 'bottom'
                },
                y: {
                    stacked: true,
                    grid: { color: 'rgba(255, 255, 255, 0.1)' },
                    beginAtZero: true,
                    max: currentSeasonalMode === 'percent' ? 100 : undefined,
                    title: {
                        display: true,
                        text: currentSeasonalMode === 'percent' ? 'Proportion (%)' : 'Number of Observations',
                        color: '#e2e8f0',
                        font: { size: 14, weight: '600' }
                    },
                    ticks: {
                        color: '#e2e8f0',
                        font: { size: 12 },
                        callback: function (value) {
                            return currentSeasonalMode === 'percent' ? value + '%' : value.toLocaleString();
                        }
                    }
                }
            },
            plugins: {
                legend: {
                    labels: {
                        color: '#e2e8f0',
                        font: {
                            size: 16,
                            family: 'Inter',
                            weight: '500'
                        },
                        padding: 20
                    }
                },
                title: {
                    display: true,
                    text: `Monthly Behavioral Distribution - ${behavioralData.elephant} (${behavioralData.period})`,
                    color: '#f1f5f9',
                    font: {
                        size: 20,
                        family: 'Inter',
                        weight: 'bold'
                    },
                    padding: 20
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            const label = context.dataset.label || '';
                            const value = context.parsed.y || 0;
                            if (currentSeasonalMode === 'percent') {
                                return `${label}: ${value.toFixed(1)}%`;
                            }
                            return `${label}: ${value.toLocaleString()} observations`;
                        }
                    }
                }
            }
        }
    });

    // If in percent mode, we need to transform the data
    if (currentSeasonalMode === 'percent') {
        const datasets = charts.seasonal.data.datasets;
        const labels = charts.seasonal.data.labels;

        for (let i = 0; i < labels.length; i++) {
            let total = 0;
            datasets.forEach(dataset => {
                total += dataset.data[i];
            });

            if (total > 0) {
                datasets.forEach(dataset => {
                    dataset.data[i] = (dataset.data[i] / total) * 100;
                });
            }
        }
        charts.seasonal.update();
    }
}

// Render temporal pattern (24-hour activity)
function renderTemporalPattern() {
    const canvas = document.getElementById('temporal-chart');
    if (!canvas) return;

    // Ensure container is visible before rendering to prevent dimension issues
    const container = document.getElementById('temporal-container');
    if (container.classList.contains('hidden')) {
        container.classList.remove('hidden');
    }

    const ctx = canvas.getContext('2d');

    // Destroy existing chart and clear property
    if (charts.temporal) {
        charts.temporal.destroy();
        charts.temporal = null;
    }

    // Use filtered data based on year selection
    const filteredData = getFilteredData();

    // Group data by hour
    const hourlyData = Array(24).fill(0).map(() => ({
        Sleeping: 0,
        'Low-energy': 0,
        Foraging: 0,
        Movement: 0,
        Bounce: 0
    }));

    filteredData.forEach(row => {
        const date = new Date(row.date || row.Date);
        if (!isNaN(date)) {
            const hour = getSASTComponents(date).hour;
            // Handle both 'behavior' and 'Behavior' columns
            let behavior = row.behavior || row.Behavior || row.state;
            if (behavior === 'Resting') behavior = 'Low-energy';

            if (hourlyData[hour][behavior] !== undefined) {
                hourlyData[hour][behavior]++;
            }
        }
    });

    // Also update currentAnalysis check if it exists (for safety)
    if (typeof currentAnalysis === 'undefined') currentAnalysis = 'time-budget';

    charts.temporal = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: Array.from({ length: 24 }, (_, i) => `${i}:00`),
            datasets: [
                {
                    label: 'Sleeping',
                    data: hourlyData.map(h => h.Sleeping),
                    backgroundColor: 'rgba(153, 153, 153, 0.8)',
                    borderColor: 'rgba(153, 153, 153, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Low-energy',
                    data: hourlyData.map(h => h['Low-energy']),
                    backgroundColor: 'rgba(230, 159, 0, 0.8)',
                    borderColor: 'rgba(230, 159, 0, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Foraging',
                    data: hourlyData.map(h => h.Foraging),
                    backgroundColor: 'rgba(16, 185, 129, 0.8)',
                    borderColor: 'rgba(16, 185, 129, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Movement',
                    data: hourlyData.map(h => h.Movement),
                    backgroundColor: 'rgba(86, 180, 233, 0.8)',
                    borderColor: 'rgba(86, 180, 233, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Bounce',
                    data: hourlyData.map(h => h.Bounce),
                    backgroundColor: 'rgba(228, 26, 28, 0.8)',
                    borderColor: 'rgba(228, 26, 28, 1)',
                    borderWidth: 1
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            scales: {
                x: {
                    stacked: true,
                    grid: { color: 'rgba(255, 255, 255, 0.1)' },
                    ticks: {
                        color: '#e2e8f0',
                        font: { size: 12 }
                    },
                    title: {
                        display: true,
                        text: 'Daily Hours (24h format)',
                        color: '#e2e8f0',
                        font: { size: 14, weight: '600' }
                    }
                },
                y: {
                    stacked: true,
                    grid: { color: 'rgba(255, 255, 255, 0.1)' },
                    beginAtZero: true,
                    max: currentTemporalMode === 'percent' ? 100 : undefined,
                    ticks: {
                        color: '#e2e8f0',
                        font: { size: 12 },
                        callback: function (value) {
                            return currentTemporalMode === 'percent' ? value + '%' : value.toLocaleString();
                        }
                    },
                    title: {
                        display: true,
                        text: currentTemporalMode === 'percent' ? 'Proportion (%)' : 'Number of Observations',
                        color: '#e2e8f0',
                        font: { size: 14, weight: '600' }
                    }
                }
            },
            plugins: {
                legend: {
                    labels: {
                        color: '#e2e8f0',
                        font: {
                            size: 16,
                            family: 'Inter',
                            weight: '500'
                        },
                        padding: 20
                    }
                },
                title: {
                    display: true,
                    text: '24-Hour Activity Pattern',
                    color: '#f1f5f9',
                    font: {
                        size: 20,
                        family: 'Inter',
                        weight: 'bold'
                    },
                    padding: 20
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            const label = context.dataset.label || '';
                            const value = context.parsed.y || 0;
                            if (currentTemporalMode === 'percent') {
                                return `${label}: ${value.toFixed(1)}%`;
                            }
                            return `${label}: ${value.toLocaleString()} observations`;
                        }
                    }
                }
            }
        }
    });

    // If in percent mode, transform temporal data
    if (currentTemporalMode === 'percent') {
        const datasets = charts.temporal.data.datasets;
        for (let i = 0; i < 24; i++) {
            let total = 0;
            datasets.forEach(dataset => {
                total += dataset.data[i];
            });

            if (total > 0) {
                datasets.forEach(dataset => {
                    dataset.data[i] = (dataset.data[i] / total) * 100;
                });
            }
        }
        charts.temporal.update();
    }
}

// Render period comparison
async function renderPeriodComparison() {
    const ctx = document.getElementById('comparison-chart').getContext('2d');

    // Destroy existing chart
    if (charts.comparison) {
        charts.comparison.destroy();
    }

    showLoading(true);

    // Get selected year for filtering
    const selectedYear = document.getElementById('global-year-select').value;

    // Load data for all periods
    const periods = ['PRE', 'INTERIM', 'POST'];
    const periodData = { 'PRE': null, 'INTERIM': null, 'POST': null };
    const periodAggregates = {
        'PRE': [],
        'INTERIM': [],
        'POST': []
    };

    // PERFORMANCE: Use populationCache if available for faster aggregate views
    const isAggMode = currentElephant === 'ALL' || currentElephant === 'MALES' || currentElephant === 'FEMALES';
    if (isAggMode && populationCache) {
        console.log('Using population cache for BACI Comparison');
        periods.forEach(period => {
            let filtered = populationCache;

            // Filter by sex subset
            if (currentElephant === 'MALES') {
                filtered = filtered.filter(row => ['E1', 'E2', 'E6'].includes(row.elephant_id));
            } else if (currentElephant === 'FEMALES') {
                filtered = filtered.filter(row => ['E3', 'E4', 'E5'].includes(row.elephant_id));
            }

            // Filter by period
            filtered = filtered.filter(row => {
                const rowStage = (row.Stage || row.stage || '').trim().toUpperCase();
                return rowStage === period;
            });

            if (selectedYear !== 'all') {
                filtered = filtered.filter(row => {
                    const date = new Date(row.date || row.Date);
                    return !isNaN(date) && getSASTComponents(date).year === parseInt(selectedYear);
                });
            }
            periodAggregates[period] = filtered;
        });

        // Calculate summaries for cached data
        periods.forEach(period => {
            const summary = calculateSummary(periodAggregates[period]);
            // Only include period if it has data
            if (summary.total > 0) {
                periodData[period] = summary;
            }
        });

        // If no data found at all
        if (Object.keys(periodData).length === 0) {
            console.warn('No data found for any period in BACI Comparison');
            showLoading(false);
            return;
        }

        // Render the actual Chart.js instance (rest of the function's logic)
        renderBACIComparisonUI(ctx, periodData, currentElephant, currentPeriod);
        showLoading(false);
        return;
    }

    const elephantsToLoad = currentElephant === 'ALL' ? ['E1', 'E2', 'E3', 'E4', 'E5', 'E6'] :
        currentElephant === 'MALES' ? ['E1', 'E2', 'E6'] :
            currentElephant === 'FEMALES' ? ['E3', 'E4', 'E5'] :
                [currentElephant];

    const loadPromises = elephantsToLoad.map(elephant => {
        const csvPath = `data/behavioral_points/${elephant}_behavioral_points.csv`;
        return new Promise((resolve) => {
            Papa.parse(csvPath, {
                download: true,
                header: true,
                dynamicTyping: true,
                complete: (results) => {
                    const validData = results.data.filter(row => row.x_m && row.y_m);
                    const localPeriodData = { 'PRE': [], 'INTERIM': [], 'POST': [] };

                    periods.forEach(period => {
                        let filtered = validData.filter(row => {
                            const rowStage = (row.Stage || row.stage || row.STAGE || '').trim().toUpperCase();
                            return rowStage === period.toUpperCase();
                        });

                        if (selectedYear !== 'all') {
                            filtered = filtered.filter(row => {
                                const date = new Date(row.date || row.Date);
                                return !isNaN(date) && getSASTComponents(date).year === parseInt(selectedYear);
                            });
                        }
                        localPeriodData[period] = filtered;
                    });
                    resolve(localPeriodData);
                },
                error: (error) => {
                    console.error(`Error loading data for ${elephant}:`, error);
                    resolve({ 'PRE': [], 'INTERIM': [], 'POST': [] }); // Resolve empty on error
                }
            });
        });
    });

    const results = await Promise.all(loadPromises);

    // Aggregate results from all elephants
    results.forEach(localData => {
        periods.forEach(period => {
            periodAggregates[period] = periodAggregates[period].concat(localData[period]);
        });
    });

    // Calculate summaries for each period
    periods.forEach(period => {
        const summary = calculateSummary(periodAggregates[period]);
        if (summary.total > 0) {
            periodData[period] = summary;
        }
    });

    if (Object.keys(periodData).length === 0) {
        console.warn('No data found for any period in BACI Comparison');
        showLoading(false);
        return;
    }

    renderBACIComparisonUI(ctx, periodData, currentElephant, currentPeriod);
    showLoading(false);
}

// Dedicated function to render the BACI comparison chart UI
function renderBACIComparisonUI(ctx, periodData, elephantId, currentPeriod) {
    if (charts.comparison) {
        charts.comparison.destroy();
    }

    const isDelta = currentBaciMode === 'delta';
    const datasets = [];
    const behaviors = ['sleeping', 'resting', 'foraging', 'movement', 'bounce'];

    if (periodData.PRE) {
        datasets.push({
            label: isDelta ? 'PRE (Baseline)' : 'PRE - HOME RANGE',
            data: behaviors.map(b => isDelta ? 0 : periodData.PRE.percentages[b]),
            backgroundColor: currentPeriod === 'PRE' ? 'rgba(59, 130, 246, 0.9)' : 'rgba(59, 130, 246, 0.4)',
            borderColor: 'rgb(59, 130, 246)',
            borderWidth: currentPeriod === 'PRE' ? 2 : 1
        });
    }

    if (periodData.INTERIM) {
        datasets.push({
            label: isDelta ? 'Interim (Δ %)' : 'Interim',
            data: behaviors.map(b => isDelta ? (periodData.INTERIM.percentages[b] - (periodData.PRE ? periodData.PRE.percentages[b] : 0)) : periodData.INTERIM.percentages[b]),
            backgroundColor: currentPeriod === 'INTERIM' ? 'rgba(245, 158, 11, 0.9)' : 'rgba(245, 158, 11, 0.4)',
            borderColor: 'rgb(245, 158, 11)',
            borderWidth: currentPeriod === 'INTERIM' ? 2 : 1
        });
    }

    if (periodData.POST) {
        datasets.push({
            label: isDelta ? 'POST (Δ %)' : 'POST - NOVEL RANGE',
            data: behaviors.map(b => isDelta ? (periodData.POST.percentages[b] - (periodData.PRE ? periodData.PRE.percentages[b] : 0)) : periodData.POST.percentages[b]),
            backgroundColor: currentPeriod === 'POST' ? 'rgba(16, 185, 129, 0.9)' : 'rgba(16, 185, 129, 0.4)',
            borderColor: 'rgb(16, 185, 129)',
            borderWidth: currentPeriod === 'POST' ? 2 : 1
        });
    }

    charts.comparison = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: ['Sleeping', 'Low-energy', 'Foraging', 'Movement', 'Bounce'],
            datasets: datasets
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            scales: {
                x: {
                    grid: { color: 'rgba(255, 255, 255, 0.1)' },
                    ticks: { color: '#e2e8f0' }
                },
                y: {
                    grid: {
                        color: 'rgba(255, 255, 255, 0.1)',
                        zeroLineColor: '#f1f5f9',
                        zeroLineWidth: 2
                    },
                    ticks: { color: '#e2e8f0' },
                    title: {
                        display: true,
                        text: isDelta ? 'Change relative to Pre (%)' : 'Percentage (%)',
                        color: '#e2e8f0'
                    }
                }
            },
            plugins: {
                legend: {
                    labels: {
                        color: '#e2e8f0',
                        font: {
                            size: 16,
                            family: 'Inter',
                            weight: '500'
                        },
                        padding: 20
                    }
                },
                title: {
                    display: true,
                    text: `${isDelta ? 'BACI Behavioral Shift' : 'Time Budget Comparison'} - ${elephantId === 'ALL' ? 'All Elephants' : (elephantId === 'MALES' ? 'All Males' : (elephantId === 'FEMALES' ? 'All Females' : elephantId))} ${currentPeriod !== 'ALL' ? `(Focal: ${currentPeriod})` : ''}`,
                    color: '#f1f5f9',
                    font: {
                        size: 20,
                        family: 'Inter',
                        weight: 'bold'
                    },
                    padding: 20
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            let label = context.dataset.label || '';
                            if (label) {
                                label += ': ';
                            }
                            if (context.parsed.y !== null) {
                                label += context.parsed.y.toFixed(1) + '%';
                            }
                            return label;
                        }
                    }
                }
            }
        }
    });
}

// Download current chart
function downloadCurrentChart() {
    const originalCanvas = document.querySelector('.viz-container:not(.hidden) canvas');
    if (!originalCanvas) {
        console.error('No visible canvas found to download');
        return;
    }

    const dateStr = new Date().toISOString().split('T')[0];
    let filename;

    switch (currentAnalysis) {
        case 'time-budget':
            filename = `time_budget_${currentElephant}_${currentPeriod}_${dateStr}.png`;
            break;
        case 'seasonal':
            filename = `seasonal_pattern_${currentElephant}_${currentPeriod}_${dateStr}.png`;
            break;
        case 'temporal':
            filename = `temporal_activity_${currentElephant}_${currentPeriod}_${dateStr}.png`;
            break;
        case 'comparison':
            filename = `period_comparison_${currentElephant}_${dateStr}.png`;
            break;
        default:
            filename = `elephant_chart_${dateStr}.png`;
    }

    // Create a temporary canvas to add the background
    const tempCanvas = document.createElement('canvas');
    const ctx = tempCanvas.getContext('2d');

    // Match dimensions
    tempCanvas.width = originalCanvas.width;
    tempCanvas.height = originalCanvas.height;

    // Fill with the dark theme background color (#111827)
    ctx.fillStyle = '#111827';
    ctx.fillRect(0, 0, tempCanvas.width, tempCanvas.height);

    // Draw the original chart on top
    ctx.drawImage(originalCanvas, 0, 0);

    // Download the result
    const url = tempCanvas.toDataURL('image/png', 1.0);
    const link = document.createElement('a');
    link.download = filename;
    link.href = url;
    link.click();
}

// Show/hide loading overlay
function showLoading(show) {
    const overlay = document.getElementById('loading-overlay');
    if (show) {
        overlay.classList.add('active');
    } else {
        overlay.classList.remove('active');
    }
}

// Export for use in other modules
window.behavioralAnalysis = {
    loadBehavioralData,
    switchAnalysisView,
    downloadCurrentChart
};
