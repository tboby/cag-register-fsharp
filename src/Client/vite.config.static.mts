import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";


// https://vitejs.dev/config/
export default defineConfig({
    plugins: [react()],
    build: {
        outDir: "../../deploy/public",
    },
    server: {
        port: 8080,
        watch: {
            ignored: [ "**/*.fs" ]
        },
    }
});