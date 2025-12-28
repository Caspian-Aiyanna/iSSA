// Behavioral Analysis JavaScript
// Handles data loading, processing, and visualization for elephant behavioral states

// Global state
let behavioralData = {};
let currentElephant = 'E1';
let currentPeriod = 'PRE';
let currentAnalysis = 'time-budget';
let charts = {};
let heatmapInstance = null;
let dataCache = {}; // Cache for CSV data

// Initialize on page load
document.addEventListener('DOMContentLoaded', () => {
    initializeEventListeners();
    loadBehavioralData(currentElephant, currentPeriod);
});

// Event Listeners
function initializeEventListeners() {
    // Elephant selection
    document.querySelectorAll('.elephant-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.elephant-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentElephant = e.target.dataset.elephant;
            loadBehavioralData(currentElephant, currentPeriod);
        });
    });

    // Period selection
    document.querySelectorAll('.period-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.period-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentPeriod = e.target.dataset.period;
            loadBehavioralData(currentElephant, currentPeriod);
        });
    });

    // Analysis type selection
    document.querySelectorAll('.analysis-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            document.querySelectorAll('.analysis-btn').forEach(b => b.classList.remove('active'));
            e.target.classList.add('active');
            currentAnalysis = e.target.dataset.analysis;
            switchAnalysisView(currentAnalysis);
        });
    });

    // Download chart
    document.getElementById('download-chart').addEventListener('click', downloadCurrentChart);

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
}

// Load behavioral data
async function loadBehavioralData(elephant, period) {
    showLoading(true);

    try {
        if (elephant === 'ALL') {
            await loadAllElephants(period);
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

    const csvPath = `../results/RSF/behavioral_points/${elephant}_behavioral_points.csv`;

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
                        const rowStage = (row.Stage || row.stage || '').toUpperCase();
                        return rowStage === period.toUpperCase();
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

// Load all elephants data
async function loadAllElephants(period) {
    const cacheKey = `ALL_${period}`;

    // Check cache first
    if (dataCache[cacheKey]) {
        console.log(`Using cached data for ALL elephants (${period})`);
        behavioralData = {
            elephant: 'ALL',
            period: period,
            data: dataCache[cacheKey].data,
            summary: dataCache[cacheKey].summary
        };
        updateStatistics(behavioralData.summary);
        updateVisualization();
        return;
    }

    const elephants = ['E1', 'E2', 'E3', 'E4', 'E5', 'E6'];
    const allData = [];

    for (const elephant of elephants) {
        const csvPath = `../results/RSF/behavioral_points/${elephant}_behavioral_points.csv`;

        await new Promise((resolve, reject) => {
            Papa.parse(csvPath, {
                download: true,
                header: true,
                dynamicTyping: true,
                complete: (results) => {
                    let data = results.data.filter(row => row.x_m && row.y_m);

                    if (period !== 'ALL') {
                        data = data.filter(row => {
                            const rowStage = (row.Stage || row.stage || '').toUpperCase();
                            return rowStage === period.toUpperCase();
                        });
                    }

                    // Add elephant ID
                    data.forEach(row => row.elephant_id = elephant);
                    allData = allData.concat(data); // Safest way to merge large arrays
                    resolve();
                },
                error: (error) => reject(error)
            });
        });
    }

    const summary = calculateSummary(allData);

    // Save to cache
    dataCache[cacheKey] = {
        data: allData,
        summary: summary
    };

    behavioralData = {
        elephant: 'ALL',
        period: period,
        data: allData,
        summary: summary
    };

    updateStatistics(behavioralData.summary);
    updateVisualization();
}

// Calculate summary statistics
function calculateSummary(data) {
    const total = data.length;
    const behaviors = {
        'Resting': 0,
        'Foraging': 0,
        'Movement': 0
    };

    data.forEach(row => {
        // Handle both 'behavior' and 'Behavior' columns
        const behavior = row.behavior || row.Behavior || row.state;
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

    return {
        total,
        behaviors,
        percentages: {
            resting: total > 0 ? ((behaviors.Resting / total) * 100).toFixed(1) : 0,
            foraging: total > 0 ? ((behaviors.Foraging / total) * 100).toFixed(1) : 0,
            movement: total > 0 ? ((behaviors.Movement / total) * 100).toFixed(1) : 0
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
    document.getElementById('resting-pct').textContent = `${summary.percentages.resting}%`;
    document.getElementById('foraging-pct').textContent = `${summary.percentages.foraging}%`;
    document.getElementById('movement-pct').textContent = `${summary.percentages.movement}%`;

    if (summary.dateRange.min && summary.dateRange.max) {
        const dateStr = `${summary.dateRange.min.toLocaleDateString()} - ${summary.dateRange.max.toLocaleDateString()}`;
        document.getElementById('date-range').textContent = dateStr;
    } else {
        document.getElementById('date-range').textContent = '-';
    }

    document.getElementById('duration-days').textContent = summary.duration > 0 ? summary.duration : '-';

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
            years.add(date.getFullYear());
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
        return !isNaN(date) && date.getFullYear() === parseInt(selectedYear);
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
        'ALL': { name: 'All Elephants', image: 'elephant_main.jpg', preRange: 'Both Ranges', badge: 'Combined Data' }
    };

    const elephant = elephantData[currentElephant];
    if (elephant) {
        document.getElementById('profile-img').src = `elephants/${elephant.image}`;
        document.getElementById('profile-name').textContent = elephant.name;
        document.getElementById('profile-badge').textContent = elephant.badge;

        // Determine home range based on period
        let homeRange;
        if (currentPeriod === 'PRE') {
            homeRange = elephant.preRange;
        } else if (currentPeriod === 'INTERIM' || currentPeriod === 'POST') {
            homeRange = currentElephant === 'ALL' ? 'Both Ranges' : 'Kariega Game Reserve';
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

    // Show selected container
    const containerMap = {
        'time-budget': 'time-budget-container',
        'seasonal': 'seasonal-container',
        'temporal': 'temporal-container',
        'comparison': 'comparison-container'
    };
    document.getElementById(containerMap[analysisType]).classList.remove('hidden');

    // Update visualization
    updateVisualization();
}

// Update visualization based on current analysis type
function updateVisualization() {
    if (!behavioralData.data || behavioralData.data.length === 0) return;

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
        Resting: 0,
        Foraging: 0,
        Movement: 0
    };

    filteredData.forEach(row => {
        const behavior = row.behavior || row.Behavior || row.state;
        if (behaviorCounts[behavior] !== undefined) {
            behaviorCounts[behavior]++;
        }
    });

    charts.timeBudget = new Chart(ctx, {
        type: 'doughnut',
        data: {
            labels: ['Resting', 'Foraging', 'Movement'],
            datasets: [{
                data: [
                    behaviorCounts.Resting,
                    behaviorCounts.Foraging,
                    behaviorCounts.Movement
                ],
                backgroundColor: [
                    'rgba(59, 130, 246, 0.8)',  // Resting - blue
                    'rgba(16, 185, 129, 0.8)',  // Foraging - green
                    'rgba(245, 158, 11, 0.8)'   // Movement - orange
                ],
                borderColor: [
                    'rgba(59, 130, 246, 1)',
                    'rgba(16, 185, 129, 1)',
                    'rgba(245, 158, 11, 1)'
                ],
                borderWidth: 2
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: true,
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

    // Get unique years from current elephant's data
    const years = new Set();
    behavioralData.data.forEach(row => {
        const date = new Date(row.date || row.Date);
        if (!isNaN(date)) {
            years.add(date.getFullYear());
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
            return !isNaN(date) && date.getFullYear() === parseInt(yearSelect.value);
        });
    }

    if (selectedMonth !== 'all') {
        filteredData = filteredData.filter(row => {
            const date = new Date(row.date || row.Date);
            return !isNaN(date) && (date.getMonth() + 1) === parseInt(selectedMonth);
        });
    }

    // Group data by month
    const monthlyData = Array(12).fill(0).map(() => ({
        Resting: 0,
        Foraging: 0,
        Movement: 0
    }));

    filteredData.forEach(row => {
        const date = new Date(row.date || row.Date);
        if (!isNaN(date)) {
            const month = date.getMonth();
            const behavior = row.behavior || row.Behavior || row.state;
            if (monthlyData[month][behavior] !== undefined) {
                monthlyData[month][behavior]++;
            }
        }
    });

    // Calculate summary statistics
    const totalObs = filteredData.length;
    const behaviorCounts = {
        Resting: filteredData.filter(r => (r.behavior || r.Behavior || r.state) === 'Resting').length,
        Foraging: filteredData.filter(r => (r.behavior || r.Behavior || r.state) === 'Foraging').length,
        Movement: filteredData.filter(r => (r.behavior || r.Behavior || r.state) === 'Movement').length
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
    document.getElementById('seasonal-obs').textContent = totalObs.toLocaleString();
    document.getElementById('seasonal-dominant').textContent = dominantBehavior;

    // Render chart
    charts.seasonal = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: monthNames,
            datasets: [
                {
                    label: 'Resting',
                    data: monthlyData.map(m => m.Resting),
                    backgroundColor: 'rgba(59, 130, 246, 0.8)',
                    borderColor: 'rgba(59, 130, 246, 1)',
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
                    backgroundColor: 'rgba(245, 158, 11, 0.8)',
                    borderColor: 'rgba(245, 158, 11, 1)',
                    borderWidth: 1
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: true,
            scales: {
                x: {
                    stacked: true,
                    grid: { color: 'rgba(255, 255, 255, 0.1)' },
                    ticks: {
                        color: '#e2e8f0',
                        font: { size: 12 }
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
                    ticks: {
                        color: '#e2e8f0',
                        font: { size: 12 }
                    },
                    title: {
                        display: true,
                        text: 'Number of Observations',
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
                    text: `Monthly Behavioral Distribution - ${behavioralData.elephant} (${behavioralData.period})`,
                    color: '#f1f5f9',
                    font: {
                        size: 20,
                        family: 'Inter',
                        weight: 'bold'
                    },
                    padding: 20
                }
            }
        }
    });
}

// Render temporal pattern (24-hour activity)
function renderTemporalPattern() {
    const ctx = document.getElementById('temporal-chart').getContext('2d');

    // Destroy existing chart
    if (charts.temporal) {
        charts.temporal.destroy();
    }

    // Use filtered data based on year selection
    const filteredData = getFilteredData();

    // Group data by hour
    const hourlyData = Array(24).fill(0).map(() => ({
        Resting: 0,
        Foraging: 0,
        Movement: 0
    }));

    filteredData.forEach(row => {
        const date = new Date(row.date || row.Date);
        if (!isNaN(date)) {
            const hour = date.getHours();
            // Handle both 'behavior' and 'Behavior' columns
            const behavior = row.behavior || row.Behavior || row.state;
            if (hourlyData[hour][behavior] !== undefined) {
                hourlyData[hour][behavior]++;
            }
        }
    });

    charts.temporal = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: Array.from({ length: 24 }, (_, i) => `${i}:00`),
            datasets: [
                {
                    label: 'Resting',
                    data: hourlyData.map(h => h.Resting),
                    backgroundColor: 'rgba(59, 130, 246, 0.8)',
                    borderColor: 'rgba(59, 130, 246, 1)',
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
                    backgroundColor: 'rgba(245, 158, 11, 0.8)',
                    borderColor: 'rgba(245, 158, 11, 1)',
                    borderWidth: 1
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: true,
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
                    ticks: {
                        color: '#e2e8f0',
                        font: { size: 12 }
                    },
                    title: {
                        display: true,
                        text: 'Number of Observations',
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
                }
            }
        }
    });
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
    const periodData = {};

    for (const period of periods) {
        const csvPath = `../results/RSF/behavioral_points/${currentElephant}_behavioral_points.csv`;

        await new Promise((resolve) => {
            Papa.parse(csvPath, {
                download: true,
                header: true,
                dynamicTyping: true,
                complete: (results) => {
                    let data = results.data.filter(row => {
                        if (!row.x_m || !row.y_m) return false;
                        const rowStage = (row.Stage || row.stage || '').toUpperCase();
                        return rowStage === period.toUpperCase();
                    });

                    // Apply year filter if not "all"
                    if (selectedYear !== 'all') {
                        data = data.filter(row => {
                            const date = new Date(row.date || row.Date);
                            return !isNaN(date) && date.getFullYear() === parseInt(selectedYear);
                        });
                    }

                    periodData[period] = calculateSummary(data);
                    resolve();
                }
            });
        });
    }

    charts.comparison = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: ['Resting', 'Foraging', 'Movement'],
            datasets: [
                {
                    label: 'Pre',
                    data: [
                        periodData.PRE.percentages.resting,
                        periodData.PRE.percentages.foraging,
                        periodData.PRE.percentages.movement
                    ],
                    backgroundColor: 'rgba(59, 130, 246, 0.8)',
                    borderColor: 'rgba(59, 130, 246, 1)',
                    borderWidth: 2
                },
                {
                    label: 'Interim',
                    data: [
                        periodData.INTERIM.percentages.resting,
                        periodData.INTERIM.percentages.foraging,
                        periodData.INTERIM.percentages.movement
                    ],
                    backgroundColor: 'rgba(245, 158, 11, 0.8)',
                    borderColor: 'rgba(245, 158, 11, 1)',
                    borderWidth: 2
                },
                {
                    label: 'Post',
                    data: [
                        periodData.POST.percentages.resting,
                        periodData.POST.percentages.foraging,
                        periodData.POST.percentages.movement
                    ],
                    backgroundColor: 'rgba(16, 185, 129, 0.8)',
                    borderColor: 'rgba(16, 185, 129, 1)',
                    borderWidth: 2
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: true,
            scales: {
                x: {
                    grid: { color: 'rgba(255, 255, 255, 0.1)' },
                    ticks: { color: '#e2e8f0' }
                },
                y: {
                    grid: { color: 'rgba(255, 255, 255, 0.1)' },
                    ticks: { color: '#e2e8f0' },
                    title: {
                        display: true,
                        text: 'Percentage (%)',
                        color: '#e2e8f0'
                    },
                    max: 100
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
                    text: `Time Budget Comparison - ${currentElephant}`,
                    color: '#f1f5f9',
                    font: {
                        size: 20,
                        family: 'Inter',
                        weight: 'bold'
                    },
                    padding: 20
                }
            }
        }
    });

    showLoading(false);
}

// Download current chart
function downloadCurrentChart() {
    let canvas;
    let filename;

    switch (currentAnalysis) {
        case 'time-budget':
            canvas = document.getElementById('time-budget-chart');
            filename = `time_budget_${currentElephant}_${currentPeriod}.png`;
            break;
        case 'temporal':
            canvas = document.getElementById('temporal-chart');
            filename = `temporal_pattern_${currentElephant}_${currentPeriod}.png`;
            break;
        case 'comparison':
            canvas = document.getElementById('comparison-chart');
            filename = `period_comparison_${currentElephant}.png`;
            break;
        default:
            alert('Download not available for this visualization type');
            return;
    }

    if (canvas) {
        const url = canvas.toDataURL('image/png');
        const link = document.createElement('a');
        link.download = filename;
        link.href = url;
        link.click();
    }
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
