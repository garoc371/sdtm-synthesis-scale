`viz-3.31.0.js` is the unmodified browser build from the official
`@viz-js/viz` 3.31.0 npm package. It includes Graphviz 16.1.0 and Expat 2.8.5.
The app loads this file locally; the graph makes no CDN requests.

Upstream sources:
- Viz.js: https://github.com/mdaines/viz-js/tree/v3
- Graphviz: https://gitlab.com/graphviz/graphviz/-/tree/16.1.0
- Expat: https://github.com/libexpat/libexpat/tree/R_2_8_5

The accompanying license files cover each library. Graphviz and its dependencies
are unchanged. `../graph.js` contains the app's selection and viewport controls.
