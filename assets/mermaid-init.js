import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs';

function getMermaidTheme() {
  return document.documentElement.classList.contains('dark') ? 'dark' : 'default';
}

mermaid.initialize({ startOnLoad: true, theme: getMermaidTheme() });

// Re-render mermaid diagrams when theme changes
const observer = new MutationObserver(async (mutations) => {
  for (const mutation of mutations) {
    if (mutation.attributeName === 'class') {
      const diagrams = document.querySelectorAll('.mermaid');
      for (const diagram of diagrams) {
        const code = diagram.getAttribute('data-mermaid-src');
        if (code) {
          diagram.removeAttribute('data-processed');
          diagram.innerHTML = code;
        }
      }
      mermaid.initialize({ startOnLoad: false, theme: getMermaidTheme() });
      await mermaid.run();
    }
  }
});

// Store original source and observe for theme changes
document.addEventListener('DOMContentLoaded', () => {
  document.querySelectorAll('.mermaid').forEach(el => {
    el.setAttribute('data-mermaid-src', el.textContent.trim());
  });
  observer.observe(document.documentElement, { attributes: true });
});
