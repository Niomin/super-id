# Build stage
FROM --platform=linux/arm64 haskell:9.12.2 AS builder

WORKDIR /app

# Install PostgreSQL development libraries
RUN apt-get update && \
    apt-get install -y \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy cabal file and download dependencies
COPY super-id.cabal ./
RUN cabal update && cabal build --only-dependencies

# Copy source code
COPY . .

# Build the application
RUN cabal build

# Install the executable to a known location
RUN cabal install --install-method=copy --installdir=/app

# Runtime stage
FROM --platform=linux/arm64 debian:bookworm-slim

# Install runtime dependencies (PostgreSQL client libs, espeak for TTS)
RUN apt-get update && \
    apt-get install -y \
    libpq5 \
    libgmp10 \
    espeak-ng \
    ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the executable from builder
COPY --from=builder /app/super-id .

# Expose port
EXPOSE 3000

# Run the application
CMD ["./super-id"]
