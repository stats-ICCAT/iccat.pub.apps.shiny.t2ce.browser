docker build -t iccat/apps/ce_browser:0.1.0 \
             --build-arg GITHUB_AUTH_TOKEN=$GITHUB_AUTH_TOKEN \
             --progress=plain \
             .
