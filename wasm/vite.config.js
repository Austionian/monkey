import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  // set the base for github pages deploy,
  // https://vite.dev/guide/static-deploy.html#github-pages
  base: "/monkey/",
  plugins: [tailwindcss()],
});
