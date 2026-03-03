FROM ubuntu:22.04

# Set environment to non-interactive
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies, NASM, and Node.js
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    ca-certificates \
    nasm \
    && curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

# Install Racket 9.1 from official installer
RUN curl -fsSL https://download.racket-lang.org/installers/9.1/racket-9.1-x86_64-linux-buster-cs.sh \
    -o racket-installer.sh \
    && sh racket-installer.sh --create-dir --unix-style --dest /usr/local \
    && rm racket-installer.sh

# Install Copilot CLI
RUN npm install -g @github/copilot

# Set working directory
WORKDIR /workspace

# Default shell
CMD ["/bin/bash"]
