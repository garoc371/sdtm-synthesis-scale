// A button with data-input and data-value sets that Shiny input when clicked
// (input_button() in R/ui_helpers.R).
document.addEventListener('click', event => {
  const button = event.target.closest('[data-input][data-value]');
  if (button) Shiny.setInputValue(button.dataset.input, button.dataset.value, { priority: 'event' });
});

// Draws a dataset graph with Graphviz (viz.js) and sends node clicks to Shiny.
// A graph is a div.dag with data-dot (Graphviz source), data-input (Shiny input id),
// data-selected (the selected node) and data-key (the dataset name; zoom, position and
// dragged nodes are kept while it stays the same).
(() => {
  const renderer = Viz.instance();
  const views = {};
  // Dragged nodes per dataset: the node's offset and its laid out centre, so an offset is
  // dropped once a new layout puts the node elsewhere.
  const moves = {};
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
      svg.removeAttribute('width');
      svg.removeAttribute('height');
      svg.setAttribute('role', 'group');
      svg.setAttribute('aria-label', 'Dataset graph');
      canvas.replaceChildren(svg);

      // The view is a scale (screen pixels per graph unit) and the graph point at the
      // canvas's top left corner.
      const size = () => [canvas.clientWidth || 1, canvas.clientHeight || 1];
      // The whole graph with its dragged nodes, below the toolbar. A fitted view refits
      // when the canvas resizes, until the user zooms or pans.
      const fit = () => {
        const [w, h] = size();
        const top = 48, side = 12;
        const b = root.getBBox();
        const [gx, gy, gw, gh] = [b.x + shift.e, b.y + shift.f, b.width, b.height];
        const scale = Math.max(MIN_SCALE, Math.min((w - 2 * side) / gw, (h - top - side) / gh, 1));
        return { scale, fitted: true, x: gx - (w / scale - gw) / 2, y: gy - top / scale - (h / scale - top / scale - gh) / 2 };
      };
      // The saved or fitted view, moved to show the selected node when it is outside.
      const start = () => {
        const v = !views[key] || views[key].fitted ? fit() : views[key];
        if (!nodes.has(selected)) return v;
        const b = nodes.get(selected).getBBox();
        const [dx, dy] = offset(selected);
        const box = { x: b.x + dx + shift.e, y: b.y + dy + shift.f, width: b.width, height: b.height };
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

      // Node and edge coordinates are those of Graphviz's translated root group.
      const root = svg.querySelector('g.graph');
      const shift = root.transform.baseVal.consolidate().matrix;
      // Edges and neighbours of one node (the hovered or selected one) stand out.
      const nodes = new Map([...svg.querySelectorAll('g.node')].map(n => [n.querySelector('title').textContent, n]));
      const edges = [...svg.querySelectorAll('g.edge')].map(e => [e, ...e.querySelector('title').textContent.split('->')]);

      // A dragged node is translated; its edges keep their laid out curve, each point moved
      // by a blend of the two ends' offsets, and the arrowhead moves with the head.
      const centre = node => { const b = node.getBBox(); return [b.x + b.width / 2, b.y + b.height / 2]; };
      const saved = moves[key] || {};
      moves[key] = {};
      for (const [id, node] of nodes) {
        const [cx, cy] = centre(node);
        const m = saved[id];
        if (m && Math.abs(m.cx - cx) < 0.5 && Math.abs(m.cy - cy) < 0.5) moves[key][id] = m;
      }
      const offset = id => moves[key][id] ? [moves[key][id].dx, moves[key][id].dy] : [0, 0];
      const numbers = text => (text.match(/-?\d*\.?\d+(?:e-?\d+)?/gi) || []).map(Number);
      const shapes = edges.map(([edge, tail, head]) => {
        const path = edge.querySelector('path'), arrows = [...edge.querySelectorAll('polygon')];
        return { tail, head, path, points: path ? numbers(path.getAttribute('d')) : [],
          arrows: arrows.map(a => [a, numbers(a.getAttribute('points'))]) };
      });
      const reshape = shape => {
        const [tx, ty] = offset(shape.tail), [hx, hy] = offset(shape.head);
        const n = shape.points.length / 2;
        if (shape.path && n > 1) {
          const p = [];
          for (let i = 0; i < n; i++) {
            const t = i / (n - 1);
            p.push(`${shape.points[2 * i] + (1 - t) * tx + t * hx},${shape.points[2 * i + 1] + (1 - t) * ty + t * hy}`);
          }
          shape.path.setAttribute('d', `M${p[0]}C${p.slice(1).join(' ')}`);
        }
        for (const [arrow, q] of shape.arrows) {
          const p = [];
          for (let i = 0; i < q.length; i += 2) p.push(`${q[i] + hx},${q[i + 1] + hy}`);
          arrow.setAttribute('points', p.join(' '));
        }
      };
      const place = id => {
        const [dx, dy] = offset(id);
        nodes.get(id).setAttribute('transform', dx || dy ? `translate(${dx} ${dy})` : '');
        for (const shape of shapes) if (shape.tail === id || shape.head === id) reshape(shape);
      };
      const reset = host.querySelector('[data-action="reset"]');
      const placed = () => { reset.disabled = !Object.keys(moves[key]).length; };
      const move = (id, dx, dy) => {
        if (!dx && !dy) delete moves[key][id];
        else {
          if (!moves[key][id]) { const [cx, cy] = centre(nodes.get(id)); moves[key][id] = { cx, cy }; }
          Object.assign(moves[key][id], { dx, dy });
        }
        place(id);
        placed();
      };
      for (const id of Object.keys(moves[key])) place(id);
      placed();
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
        // Pointer clicks are handled with dragging, below; this is a keyboard's click.
        node.addEventListener('click', event => { if (!event.detail) select(id); });
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
          else if (action === 'reset') { for (const id of Object.keys(moves[key])) move(id, 0, 0); }
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

      // Dragging a node moves it, dragging the background pans; either without moving
      // (under 4 pixels) is a click.
      let drag = null;
      canvas.addEventListener('pointerdown', event => {
        if (event.button !== 0 || !view) return;
        const node = event.target.closest('g.node');
        const id = node && node.querySelector('title').textContent;
        canvas.setPointerCapture(event.pointerId);
        drag = { id, from: point(event), at: point(event), moved: 0, start: id && offset(id) };
        canvas.classList.add(id ? 'moving' : 'panning');
      });
      canvas.addEventListener('pointermove', event => {
        if (!drag) return;
        const [px, py] = point(event);
        drag.moved += Math.abs(px - drag.at[0]) + Math.abs(py - drag.at[1]);
        if (drag.id) {
          if (drag.moved >= 4) move(drag.id, drag.start[0] + (px - drag.from[0]) / view.scale, drag.start[1] + (py - drag.from[1]) / view.scale);
        } else {
          view = { scale: view.scale, x: view.x - (px - drag.at[0]) / view.scale, y: view.y - (py - drag.at[1]) / view.scale };
          draw();
        }
        drag.at = [px, py];
      });
      const end = () => { drag = null; canvas.classList.remove('panning', 'moving'); };
      canvas.addEventListener('pointerup', () => {
        if (drag && drag.moved < 4) {
          if (drag.id) select(drag.id);
          else if (selected) select(selected);
        }
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
