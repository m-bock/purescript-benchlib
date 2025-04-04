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
  line [0.002, 0.153, 0.342, 0.583, 0.002, 0.153, 0.342, 0.583]
  line [0.002, 0.001, 0, 0.001, 0.002, 0.001, 0, 0.001]
```
![ff3456](https://placehold.co/8x8/ff3456/ff3456.png) Array&nbsp;&nbsp;![00ff00](https://placehold.co/8x8/00ff00/00ff00.png) Lazy List