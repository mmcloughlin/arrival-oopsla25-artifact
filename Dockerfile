FROM ubuntu:22.04

# Install system dependencies
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y \
        build-essential \
        curl \
        git \
        libgmp-dev \
        opam \
        pkg-config \
        python3-distutils \
        unzip \
        wget \
    ;

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain 1.85.0 -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Install Arrival
COPY arrival /root/arrival

# Install SymASLp.
RUN mkdir -p /root/symaslp
RUN /root/arrival/cranelift/isle/veri/veri/script/install/aslp.sh -i /root/symaslp
ENV PATH="/root/symaslp/bin:${PATH}"

# Install z3
RUN mkdir -p /root/z3/bin
RUN /root/arrival/cranelift/isle/veri/veri/script/install/z3.sh -b /root/z3/bin
ENV PATH="/root/z3/bin:${PATH}"

# Install CVC5
RUN mkdir -p /root/cvc5
RUN /root/arrival/cranelift/isle/veri/veri/script/install/cvc5.sh -i /root/cvc5
ENV PATH="/root/cvc5/bin:${PATH}"
