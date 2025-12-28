"""
Convert RSF GeoTIFF rasters to web-compatible PNG images with georeferencing metadata.
This script processes all rasters in results/RSF/rasters/{pre,interim,post}/
and generates PNG images with accompanying JSON metadata for web visualization.
"""

import os
import json
from pathlib import Path
import numpy as np
from PIL import Image
import rasterio
from rasterio.enums import Resampling

# Paths
BASE_DIR = Path(__file__).parent.parent
RASTER_DIR = BASE_DIR / "results" / "RSF" / "rasters"
OUTPUT_DIR = Path(__file__).parent / "data" / "rsf_maps"

# Create output directory
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

def normalize_raster(data, vmin=None, vmax=None):
    """Normalize raster data to 0-1 range, handling NaN values."""
    # Mask NaN and infinite values
    valid_mask = np.isfinite(data)
    
    if not valid_mask.any():
        return np.zeros_like(data)
    
    valid_data = data[valid_mask]
    
    # Use provided min/max or calculate from data
    if vmin is None:
        vmin = np.percentile(valid_data, 2)  # Use 2nd percentile to avoid outliers
    if vmax is None:
        vmax = np.percentile(valid_data, 98)  # Use 98th percentile
    
    # Normalize
    normalized = np.zeros_like(data)
    normalized[valid_mask] = np.clip((data[valid_mask] - vmin) / (vmax - vmin), 0, 1)
    
    return normalized

def apply_colormap(normalized_data, colormap='green_red'):
    """Apply a colormap to normalized data (0-1 range)."""
    # Green (low) to Red (high) gradient - standard for RSF maps
    if colormap == 'green_red':
        # Create RGB image
        rgb = np.zeros((*normalized_data.shape, 3), dtype=np.uint8)
        
        # Green (low) to Yellow (medium) to Red (high) gradient
        # Red channel: increases from 0 to 255
        rgb[:, :, 0] = (normalized_data * 255).astype(np.uint8)
        # Green channel: high at low values, decreases at high values
        rgb[:, :, 1] = ((1 - normalized_data * 0.7) * 255).astype(np.uint8)
        # Blue channel: stays low
        rgb[:, :, 2] = 0
        
        return rgb
    
    # Default: grayscale
    gray = (normalized_data * 255).astype(np.uint8)
    return np.stack([gray, gray, gray], axis=-1)

def convert_raster_to_png(raster_path, output_path, metadata_path):
    """Convert a GeoTIFF raster to PNG with metadata."""
    try:
        with rasterio.open(raster_path) as src:
            # Read the raster data
            data = src.read(1)
            
            # Get bounds and transform
            bounds = src.bounds
            transform = src.transform
            crs = src.crs
            
            # Normalize the data
            normalized = normalize_raster(data)
            
            # Apply colormap
            rgb = apply_colormap(normalized, colormap='green_red')
            
            # Handle NaN values (make them transparent)
            alpha = np.where(np.isfinite(data), 255, 0).astype(np.uint8)
            rgba = np.dstack([rgb, alpha])
            
            # Create PIL Image
            img = Image.fromarray(rgba, mode='RGBA')
            
            # Save PNG
            img.save(output_path, 'PNG', optimize=True)
            
            # Save metadata
            metadata = {
                'bounds': {
                    'west': bounds.left,
                    'south': bounds.bottom,
                    'east': bounds.right,
                    'north': bounds.top
                },
                'crs': str(crs),
                'width': src.width,
                'height': src.height,
                'transform': list(transform)[:6],
                'nodata': float(src.nodata) if src.nodata is not None and not np.isnan(src.nodata) else None
            }
            
            with open(metadata_path, 'w') as f:
                json.dump(metadata, f, indent=2)
            
            print(f"[+] Converted: {raster_path.name} -> {output_path.name}")
            return True
            
    except Exception as e:
        print(f"[-] Error converting {raster_path.name}: {e}")
        return False

def main():
    """Process all rasters in the directory structure."""
    periods = ['pre', 'interim', 'post']
    converted_count = 0
    
    # Create index of all converted files
    index = {
        'elephants': ['E1', 'E2', 'E3', 'E4', 'E5', 'E6'],
        'behaviors': ['Resting', 'Foraging', 'Movement'],
        'periods': periods,
        'maps': []
    }
    
    for period in periods:
        period_dir = RASTER_DIR / period
        
        if not period_dir.exists():
            print(f"[!] Directory not found: {period_dir}")
            continue
        
        print(f"\n[*] Processing {period.upper()} period...")
        
        # Find all .tif files
        for raster_file in period_dir.glob("*.tif"):
            # Parse filename: Realized_E1_pre_Foraging.tif or Usage_E3_pre_Resting.tif
            parts = raster_file.stem.split('_')
            
            if len(parts) < 4:
                continue
            
            map_type = parts[0]  # Realized or Usage
            elephant = parts[1]  # E1, E2, etc.
            period_name = parts[2]  # pre, interim, post
            behavior = parts[3]  # Resting, Foraging, Movement
            
            # Create output filename
            output_name = f"{elephant}_{period_name}_{behavior}_{map_type}.png"
            metadata_name = f"{elephant}_{period_name}_{behavior}_{map_type}.json"
            
            output_path = OUTPUT_DIR / output_name
            metadata_path = OUTPUT_DIR / metadata_name
            
            # Convert raster
            if convert_raster_to_png(raster_file, output_path, metadata_path):
                converted_count += 1
                
                # Add to index
                index['maps'].append({
                    'elephant': elephant,
                    'period': period_name,
                    'behavior': behavior,
                    'type': map_type,
                    'png': output_name,
                    'metadata': metadata_name
                })
    
    # Save index
    index_path = OUTPUT_DIR / 'rsf_index.json'
    with open(index_path, 'w') as f:
        json.dump(index, f, indent=2)
    
    print(f"\n[DONE] Conversion complete!")
    print(f"   Converted: {converted_count} rasters")
    print(f"   Output directory: {OUTPUT_DIR}")
    print(f"   Index file: {index_path}")

if __name__ == "__main__":
    main()
