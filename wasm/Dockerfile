FROM node:24 AS builder
COPY . .
WORKDIR /wasm
RUN npm run build

FROM nginx:alpine
COPY --from=builder /wasm/dist /usr/share/nginx/html
RUN apk add --no-cache bash
