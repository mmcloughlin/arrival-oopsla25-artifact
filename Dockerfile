FROM ubuntu:22.04

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
        build-essential \
        git \
        unzip \
        wget \
    ;

# Install Arrival
COPY arrival /root/arrival

# Install z3
RUN mkdir -p /root/z3/bin
RUN /root/arrival/cranelift/isle/veri/veri/script/install/z3.sh -b /root/z3/bin
ENV PATH="/root/z3/bin:${PATH}"

# Install CVC5
RUN mkdir -p /root/cvc5
RUN /root/arrival/cranelift/isle/veri/veri/script/install/cvc5.sh -i /root/cvc5
ENV PATH="/root/cvc5/bin:${PATH}"
