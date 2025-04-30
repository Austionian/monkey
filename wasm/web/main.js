const worker = new Worker(new URL("./worker.js", import.meta.url), {
  type: "module",
});

const btn_loading = document.getElementById("btn_loading");
const btn_text = document.getElementById("btn_text");

document.getElementById("btn").addEventListener("click", async () => {
  btn_loading.classList.remove("hidden");
  btn_text.classList.add("hidden");

  const input = document.getElementById("input").value;

  // Wait for the worker to finish
  const { output, startTime, endTime } = await new Promise((resolve) => {
    worker.onmessage = (e) => {
      resolve(e.data);
    };

    // Send input to worker
    worker.postMessage(input);
  });

  document.getElementById("output").innerText = output;
  document.getElementById("executionTime").innerText =
    `Executed in ${endTime - startTime} millisecond${endTime - startTime > 1 ? "s" : ""}`;
  document.getElementById("btn_text").innerText = "run";

  btn_loading.classList.add("hidden");
  btn_text.classList.remove("hidden");
});
