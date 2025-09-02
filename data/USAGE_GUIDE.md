# Guide: Using Cooke 2020 Data in Your Morris Water Maze App

## Data Conversion Summary

Successfully converted 2 wide-format CSV files to long format:
- **31-2.csv** → 54 animals, 8,038 observations (Group_31_2)
- **prueba_37-3.csv** → 54 animals, 7,379 observations (Group_37_3)
- **Combined**: 108 animals, 15,417 total observations

## Data Structure

The converted files now have the required format for your app:
- `time`: Time points (0 to ~60 seconds)
- `x`: X coordinates (range: ~4-104)
- `y`: Y coordinates (range: ~3-103)
- `Individual`: Animal ID (0-53 for each group)
- `Group`: Group identifier (Group_31_2, Group_37_3)

## How to Load in Your App

1. **Single Group Analysis**: Use individual files
   - `cooke2020_31-2_converted.csv` (Group_31_2 only)
   - `cooke2020_37-3_converted.csv` (Group_37_3 only)

2. **Comparative Analysis**: Use combined file
   - `cooke2020_converted.csv` (both groups)

## Arena Parameter Estimation

Based on coordinate ranges (4-104), the arena appears to be ~100 units in diameter:

**Suggested Parameters:**
- **Center X**: 54 (approximate middle of 4-104 range)
- **Center Y**: 53 (approximate middle of 3-103 range)  
- **Radius**: 50 (half of ~100 unit diameter)

## Steps to Analyze

1. **Load Data**: Upload one of the converted CSV files in your app
2. **Set Arena Parameters**: Use suggested values above or adjust based on your knowledge
3. **Platform Location**: You'll need to determine or estimate platform coordinates for entropy calculations
4. **Run Analysis**: Use both normalized and non-normalized entropy for validation

## Validation Strategy

Since you want to validate the entropy metric:

1. **Start with normalized entropy** (recommended for cross-study comparisons)
2. **Compare groups** (Group_31_2 vs Group_37_3) 
3. **Analyze individual variations** within each group
4. **Statistical validation** can come after initial exploration

## Platform Location

The platform coordinates aren't provided in the data. You have options:
1. **Estimate** based on behavioral patterns (animals converging to a location)
2. **Use automatic detection** if your app has this feature
3. **Set manually** based on experimental knowledge

## Next Steps

1. Load `cooke2020_converted.csv` in your app
2. Use the suggested arena parameters
3. Explore the data with normalized entropy enabled
4. Once you're satisfied with the analysis, we can help with statistical validation

The data is now ready for entropy analysis in your app!
