import init, { greet, execute } from "monkey";

init();

document.getElementById("btn").addEventListener("click", () => {
  greet(execute(document.getElementById("i").value));
});
