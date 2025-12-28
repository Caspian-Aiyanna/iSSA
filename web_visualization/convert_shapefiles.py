"""
Convert shapefiles to GeoJSON for web visualization - Simple version
"""
import json
import os
try:
    import shapefile
    from pyproj import Transformer
except ImportError:
    print("Installing required packages...")
    import subprocess
    subprocess.check_call(['pip', 'install', 'pyshp', 'pyproj'])
    import shapefile
    from pyproj import Transformer

# Define paths
shp_dir = "d:/PhD/Projects/WUR_projects/iSSA/data/shp"
output_dir = "d:/PhD/Projects/WUR_projects/iSSA/web_visualization/data"

# Create output directory if it doesn't exist
os.makedirs(output_dir, exist_ok=True)

def shapefile_to_geojson(shp_path, output_path, transform=False):
    """Convert shapefile to GeoJSON (shapefiles are already in WGS84)"""
    
    # Read shapefile
    sf = shapefile.Reader(shp_path)
    
    # Build GeoJSON structure
    features = []
    
    for shape_rec in sf.shapeRecords():
        shape = shape_rec.shape
        record = shape_rec.record
        
        # Get coordinates (already in WGS84 - no transformation needed)
        if shape.shapeType == 5:  # Polygon
            coordinates = []
            for part_idx in range(len(shape.parts)):
                start = shape.parts[part_idx]
                end = shape.parts[part_idx + 1] if part_idx + 1 < len(shape.parts) else len(shape.points)
                
                ring = []
                for point in shape.points[start:end]:
                    ring.append([point[0], point[1]])  # [lng, lat]
                coordinates.append(ring)
            
            geometry = {
                "type": "Polygon",
                "coordinates": coordinates
            }
        elif shape.shapeType in [3, 13]:  # Polyline or PolylineZ
            coordinates = []
            for point in shape.points:
                coordinates.append([point[0], point[1]])  # [lng, lat]
            
            geometry = {
                "type": "LineString",
                "coordinates": coordinates
            }
        else:
            continue
        
        # Build feature
        feature = {
            "type": "Feature",
            "geometry": geometry,
            "properties": dict(zip([f[0] for f in sf.fields[1:]], record))
        }
        features.append(feature)
    
    # Build GeoJSON
    geojson = {
        "type": "FeatureCollection",
        "features": features
    }
    
    # Save
    with open(output_path, 'w') as f:
        json.dump(geojson, f)
    
    # Calculate bounds
    all_coords = []
    for feature in features:
        geom = feature['geometry']
        if geom['type'] == 'Polygon':
            for ring in geom['coordinates']:
                all_coords.extend(ring)
        elif geom['type'] == 'LineString':
            all_coords.extend(geom['coordinates'])
    
    if all_coords:
        lngs = [c[0] for c in all_coords]
        lats = [c[1] for c in all_coords]
        bounds = [min(lngs), min(lats), max(lngs), max(lats)]
        return bounds
    return None

# Convert files
print("Converting study area boundaries...")
kw_bounds = shapefile_to_geojson(
    f"{shp_dir}/KW/KW.shp",
    f"{output_dir}/kw_boundary.geojson"
)
print(f"KW bounds: {kw_bounds}")

hv_bounds = shapefile_to_geojson(
    f"{shp_dir}/HV/HV.shp",
    f"{output_dir}/hv_boundary.geojson"
)
print(f"HV bounds: {hv_bounds}")

print("\nConverting fence line...")
fence_bounds = shapefile_to_geojson(
    f"{shp_dir}/Fence/fence_2024.shp",
    f"{output_dir}/fence_line.geojson"
)
print(f"Fence bounds: {fence_bounds}")

# Calculate combined bounds
all_bounds = [kw_bounds, hv_bounds, fence_bounds]
all_bounds = [b for b in all_bounds if b is not None]

if all_bounds:
    combined_bounds = [
        min(b[0] for b in all_bounds),  # west
        min(b[1] for b in all_bounds),  # south
        max(b[2] for b in all_bounds),  # east
        max(b[3] for b in all_bounds)   # north
    ]
    
    center_lng = (combined_bounds[0] + combined_bounds[2]) / 2
    center_lat = (combined_bounds[1] + combined_bounds[3]) / 2
    
    print(f"\nCombined study area bounds (WGS84):")
    print(f"  West:  {combined_bounds[0]:.6f}")
    print(f"  South: {combined_bounds[1]:.6f}")
    print(f"  East:  {combined_bounds[2]:.6f}")
    print(f"  North: {combined_bounds[3]:.6f}")
    
    print(f"\nMap center point:")
    print(f"  Latitude:  {center_lat:.6f}")
    print(f"  Longitude: {center_lng:.6f}")
    
    # Save bounds info as JSON for JavaScript
    bounds_info = {
        "bounds": {
            "south": combined_bounds[1],
            "west": combined_bounds[0],
            "north": combined_bounds[3],
            "east": combined_bounds[2]
        },
        "center": {
            "lat": center_lat,
            "lng": center_lng
        },
        "zoom": 12
    }
    
    with open(f"{output_dir}/map_config.json", 'w') as f:
        json.dump(bounds_info, f, indent=2)
    
    print(f"\nGeoJSON files saved to: {output_dir}")
    print("Files created:")
    print("  - kw_boundary.geojson")
    print("  - hv_boundary.geojson")
    print("  - fence_line.geojson")
    print("  - map_config.json")
