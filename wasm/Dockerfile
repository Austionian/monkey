FROM rust:1-bullseye AS rust-builder

COPY Cargo.toml Cargo.lock ./

# Copy only the manifest files of all crates first so Docker can cache deps.
COPY ast/Cargo.toml          ast/Cargo.toml
COPY code/Cargo.toml         code/Cargo.toml
COPY compiler/Cargo.toml     compiler/Cargo.toml
COPY evaluator/Cargo.toml    evaluator/Cargo.toml
COPY lexer/Cargo.toml        lexer/Cargo.toml
COPY object/Cargo.toml       object/Cargo.toml
COPY parser/Cargo.toml       parser/Cargo.toml
COPY repl/Cargo.toml         repl/Cargo.toml
COPY token/Cargo.toml        token/Cargo.toml
COPY vm/Cargo.toml           vm/Cargo.toml
COPY wasm/Cargo.toml         wasm/Cargo.toml

RUN cargo install wasm-pack --locked

COPY ast/          ast/
COPY code/         code/
COPY compiler/     compiler/
COPY evaluator/    evaluator/
COPY lexer/        lexer/
COPY object/       object/
COPY parser/       parser/
COPY repl/         repl/
COPY token/        token/
COPY vm/           vm/
COPY wasm/         wasm/

RUN cargo build --workspace --release

WORKDIR /wasm

RUN wasm-pack build --release --target web

FROM node:24-alpine AS node-builder
WORKDIR /app
COPY wasm/package.json wasm/package-lock.json ./
RUN npm ci --omit=dev
COPY --from=rust-builder /wasm/pkg ./pkg
COPY wasm/web/ ./web/
COPY wasm/index.html ./
COPY wasm/vite.config.js ./

# Build static assets (your front‑end framework)
RUN npm run build

FROM nginx:alpine
COPY --from=node-builder /app/dist /usr/share/nginx/html
