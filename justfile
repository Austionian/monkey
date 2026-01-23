default:
  just -l

# Deploys the monkey playground to https://monkey.r00ks.io
deploy:
  #!/bin/bash
  cd wasm && just deploy
