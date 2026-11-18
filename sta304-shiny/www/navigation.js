// Start each top-level page at its heading while retaining the selected filters.
document.addEventListener("DOMContentLoaded", function () {
  if (!window.jQuery) return;
  window.jQuery(document).on("shown.bs.tab", '#main_nav a[data-bs-toggle="tab"]', function () {
    document.querySelectorAll(".bslib-sidebar-layout > .main, .bslib-sidebar-layout > .sidebar")
      .forEach(function (panel) { panel.scrollTop = 0; });
  });
});
