import init, { execute } from "monkey";

init();

document.getElementById("btn").addEventListener("click", () => {
  document.getElementById("output").innerText = execute(
    document.getElementById("i").value,
  );
});
