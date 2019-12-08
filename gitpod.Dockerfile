FROM gitpod/workspace-full

USER gitpod

RUN sudo apt-get update -y && \
    sudo apt-get install -y haskell-platform