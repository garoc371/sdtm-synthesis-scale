// Draws a dataset graph with Graphviz (viz.js) and sends node clicks to Shiny.
// A graph is a div.dag with data-dot (Graphviz source), data-input (Shiny input id),
// data-selected (the selected node) and data-key (the dataset name; zoom and position
// are kept while it stays the same).
(() => {
  const renderer = Viz.instance();
  const views = {};
  const MIN_SCALE = 0.1, MAX_SCALE = 3;

  async function mount(host) {
    if (host.dataset.mounted) return;
    host.dataset.mounted = 'true';
    const canvas = host.querySelector('.dag-canvas');
    const label = host.querySelector('.dag-scale');
    const key = host.dataset.key;
    const selected = host.dataset.selected;
    try {
      const viz = await renderer;
      if (!host.isConnected) return;
      const svg = viz.renderSVGElement(host.dataset.dot, { engine: 'dot' });
      const [gx, gy, gw, gh] = svg.getAttribute('viewBox').split(/\s+/).map(Number);
      svg.removeAttribute('width');
      svg.removeAttribute('height');
      svg.setAttribute('role', 'group');
      svg.setAttribute('aria-label', 'Dataset graph');
      canvas.replaceChildren(svg);

      // The view is a scale (screen pixels per graph unit) and the graph point at the
      // canvas's top left corner.
      const size = () => [canvas.clientWidth || 1, canvas.clientHeight || 1];
      // The whole graph, below the toolbar. A fitted view refits when the canvas resizes,
      // until the user zooms or pans.
      const fit = () => {
        const [w, h] = size();
        const top = 48, side = 12;
        const scale = Math.max(MIN_SCALE, Math.min((w - 2 * side) / gw, (h - top - side) / gh, 1));
        return { scale, fitted: true, x: gx - (w / scale - gw) / 2, y: gy - top / scale - (h / scale - top / scale - gh) / 2 };
      };
      // The saved or fitted view, moved to show the selected node when it is outside.
      const start = () => {
        const v = !views[key] || views[key].fitted ? fit() : views[key];
        if (!nodes.has(selected)) return v;
        // Node boxes are in the coordinates of Graphviz's translated root group.
        const { e, f } = svg.querySelector('g.graph').transform.baseVal.consolidate().matrix;
        const b = nodes.get(selected).getBBox();
        const box = { x: b.x + e, y: b.y + f, width: b.width, height: b.height };
        const [w, h] = size();
        const inside = box.x >= v.x && box.y >= v.y && box.x + box.width <= v.x + w / v.scale &&
          box.y + box.height <= v.y + h / v.scale;
        return inside ? v : { scale: v.scale, x: box.x + box.width / 2 - w / v.scale / 2, y: box.y + box.height / 2 - h / v.scale / 2 };
      };
      // A hidden canvas has no size yet: the view is set once it is shown.
      let view = null;
      const draw = () => {
        if (!canvas.clientWidth || !canvas.clientHeight) return;
        view = !view ? start() : view.fitted ? fit() : view;
        const [w, h] = size();
        svg.setAttribute('viewBox', [view.x, view.y, w / view.scale, h / view.scale].join(' '));
        label.textContent = `${Math.round(view.scale * 100)}%`;
        views[key] = view;
      };
      const zoom = (factor, at) => {
        if (!view) return;
        const [w, h] = size();
        const [px, py] = at || [w / 2, h / 2];
        const scale = Math.min(MAX_SCALE, Math.max(MIN_SCALE, view.scale * factor));
        view = { scale, x: view.x + px / view.scale - px / scale, y: view.y + py / view.scale - py / scale };
        draw();
      };

      // Edges and neighbours of one node (the hovered or selected one) stand out.
      const nodes = new Map([...svg.querySelectorAll('g.node')].map(n => [n.querySelector('title').textContent, n]));
      const edges = [...svg.querySelectorAll('g.edge')].map(e => [e, ...e.querySelector('title').textContent.split('->')]);
      svg.classList.toggle('dense', edges.length > 2.5 * nodes.size);
      const focus = id => {
        const near = new Set([id]);
        for (const [edge, tail, head] of edges) {
          const linked = tail === id || head === id;
          edge.classList.toggle('linked', linked);
          if (linked) { near.add(tail); near.add(head); }
        }
        for (const [name, node] of nodes) node.classList.toggle('near', near.has(name));
        svg.classList.toggle('focusing', nodes.has(id));
      };
      focus(selected);

      // Clicking the selected node, the background or pressing Escape clears the selection.
      const select = id => Shiny.setInputValue(host.dataset.input, id === selected ? '' : id, { priority: 'event' });
      for (const [id, node] of nodes) {
        node.setAttribute('role', 'button');
        node.setAttribute('tabindex', '0');
        node.setAttribute('aria-label', id);
        node.addEventListener('click', () => select(id));
        node.addEventListener('keydown', event => {
          if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); select(id); }
        });
        node.addEventListener('mouseenter', () => focus(id));
        node.addEventListener('mouseleave', () => focus(selected));
      }

      new ResizeObserver(draw).observe(canvas);

      const find = host.querySelector('.dag-find');
      find.querySelector('datalist').replaceChildren(...[...nodes.keys()].sort().map(id => new Option(id)));
      find.querySelector('input').addEventListener('change', event => {
        if (nodes.has(event.target.value)) select(event.target.value);
      });

      for (const button of host.querySelectorAll('[data-action]')) {
        button.addEventListener('click', () => {
          const action = button.dataset.action;
          if (action === 'fit') { view = fit(); draw(); }
          else if (action === 'actual') zoom(1 / view.scale);
          else zoom(action === 'in' ? 1.25 : 0.8);
        });
      }
      const point = event => {
        const rect = canvas.getBoundingClientRect();
        return [event.clientX - rect.left, event.clientY - rect.top];
      };
      // The wheel zooms once the graph has been clicked (or on a pinch); before that it scrolls the page.
      canvas.addEventListener('wheel', event => {
        if (!event.ctrlKey && !canvas.contains(document.activeElement)) return;
        event.preventDefault();
        zoom(Math.exp(-event.deltaY * (event.ctrlKey ? 0.01 : 0.0015)), point(event));
      }, { passive: false });
      canvas.addEventListener('dblclick', event => {
        if (!event.target.closest('g.node')) zoom(1.6, point(event));
      });
      canvas.addEventListener('keydown', event => {
        const actions = { '+': 1.25, '=': 1.25, '-': 0.8 };
        if (actions[event.key]) zoom(actions[event.key]);
        if (event.key === '0') { view = fit(); draw(); }
        if (event.key === 'Escape' && selected) select(selected);
      });

      let drag = null, moved = 0;
      canvas.addEventListener('pointerdown', event => {
        if (event.button !== 0 || event.target.closest('g.node')) return;
        canvas.setPointerCapture(event.pointerId);
        drag = point(event);
        moved = 0;
        canvas.classList.add('panning');
      });
      canvas.addEventListener('pointermove', event => {
        if (!drag) return;
        const [px, py] = point(event);
        moved += Math.abs(px - drag[0]) + Math.abs(py - drag[1]);
        view = { scale: view.scale, x: view.x - (px - drag[0]) / view.scale, y: view.y - (py - drag[1]) / view.scale };
        drag = [px, py];
        draw();
      });
      const end = () => { drag = null; canvas.classList.remove('panning'); };
      canvas.addEventListener('pointerup', () => {
        if (drag && moved < 4 && selected) select(selected);
        end();
      });
      canvas.addEventListener('pointercancel', end);
    } catch (error) {
      canvas.textContent = `The graph could not be drawn: ${error.message}`;
    }
  }

  const mountAll = () => document.querySelectorAll('.dag').forEach(mount);
  new MutationObserver(mountAll).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener('DOMContentLoaded', mountAll);
})();
