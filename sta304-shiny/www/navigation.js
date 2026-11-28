// Let visitors dismiss plot styling without losing their chart settings.
document.addEventListener("DOMContentLoaded", function () {
  const styling = document.querySelector(".plot-styling");
  if (!styling) return;

  document.addEventListener("click", function (event) {
    if (styling.open && !styling.contains(event.target)) styling.open = false;
  });

  styling.addEventListener("keydown", function (event) {
    if (event.key === "Escape" && styling.open) {
      styling.open = false;
      styling.querySelector("summary").focus();
    }
  });

  document.getElementById("view").addEventListener("change", function () {
    styling.open = false;
  });
});
