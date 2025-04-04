
```mermaid
---
  config:
    themeVariables:
        xyChart:
            plotColorPalette: "#ff3456, #00ff00, #0000ff, #ffff00, #ff00ff, #00ffff"
---
xychart-beta
  title "replicate functions"
  x-axis "Input Size" [20000, 40000, 60000, 80000]
  y-axis "Time (in ms)" 0 --> 1
  line [0.14, 0.235, 0.418, 0.14, 0.235, 0.418]
  line [0.003, 0.004, 0.002, 0.003, 0.004, 0.002]
```
![ff3456](https://placehold.co/8x8/ff3456/ff3456.png) array&nbsp;&nbsp;![00ff00](https://placehold.co/8x8/00ff00/00ff00.png) lazy list