# Minimal Example
```mermaid
---
  config:
    themeVariables:
        xyChart:
            plotColorPalette: "#ff3456, #00ff00, #0000ff, #ffff00, #ff00ff, #00ffff"
---
xychart-beta
  title "Replicate functions"
  x-axis "Input Size" [0, 20000, 40000, 60000, 80000]
  y-axis "Time (in ms)" 0 --> 1
  line [0.0061968620000000104, 0.14550031900000002, 0.28393252999999996, 0.3942912039999999, 0.6026073060000001]
  line [0.003172471000000087, 0.002654841000000033, 0.0022092909999998937, 0.003436104999999998, 0.0019074949999999262]
```
![ff3456](https://placehold.co/8x8/ff3456/ff3456.png) Array&nbsp;&nbsp;![00ff00](https://placehold.co/8x8/00ff00/00ff00.png) Lazy List