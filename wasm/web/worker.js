import init, { execute } from "monkey";

init();

self.onmessage = function (e) {
  const input = e.data;
  try {
    const startTime = performance.now();
    const output = execute(input);
    const endTime = performance.now();
    self.postMessage({ success: true, output, startTime, endTime });
  } catch (err) {
    self.postMessage({ success: false, error: err.toString() });
  }
};
