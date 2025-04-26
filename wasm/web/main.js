import init, { execute } from "monkey";

init();

document.getElementById("btn").addEventListener("click", () => {
  const input = document.getElementById("input").value;
  const startTime = performance.now();
  const output = execute(input);
  const endTime = performance.now();
  document.getElementById("output").innerText = output;
  document.getElementById("executionTime").innerText =
    `Executed in ${endTime - startTime} millisecond${endTime - startTime > 1 ? "s" : ""}`;
});
