# Simple HTTP Server for Elephant Movement Visualization Platform
# Run this script to start a local web server

Write-Host "🐘 Elephant Movement Analysis Platform" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""

# Check if Python is available
$pythonCmd = Get-Command python -ErrorAction SilentlyContinue

if ($pythonCmd) {
    Write-Host "✓ Python found - Starting server on http://localhost:8000" -ForegroundColor Green
    Write-Host ""
    Write-Host "Press Ctrl+C to stop the server" -ForegroundColor Yellow
    Write-Host ""
    
    # Start Python HTTP server
    python -m http.server 8000
} else {
    Write-Host "✗ Python not found" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please install Python or use an alternative:" -ForegroundColor Yellow
    Write-Host "  1. Install Python from https://www.python.org/" -ForegroundColor White
    Write-Host "  2. Or use Node.js: npx http-server -p 8000" -ForegroundColor White
    Write-Host "  3. Or use VS Code Live Server extension" -ForegroundColor White
    Write-Host ""
    
    # Keep window open
    Read-Host "Press Enter to exit"
}
