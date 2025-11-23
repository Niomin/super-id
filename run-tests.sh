#!/bin/bash
set -e

echo "=== Super-ID Test Runner ==="
echo ""

# Check if PostgreSQL is running
echo "Checking PostgreSQL connection..."
if ! pg_isready -h "${POSTGRES_HOST:-localhost}" -p "${POSTGRES_PORT:-5432}" -U "${POSTGRES_USER:-superid}" > /dev/null 2>&1; then
  echo "Error: PostgreSQL is not ready. Please ensure the database is running."
  echo "You can start it with: docker-compose up -d postgres"
  exit 1
fi
echo "✓ PostgreSQL is ready"
echo ""

# Set test environment variables
export POSTGRES_HOST="${POSTGRES_HOST:-localhost}"
export POSTGRES_PORT="${POSTGRES_PORT:-5432}"
export POSTGRES_USER="${POSTGRES_USER:-superid}"
export POSTGRES_PASSWORD="${POSTGRES_PASSWORD:-superid123}"
export POSTGRES_DB="${POSTGRES_DB:-superid}"
export IMAGE_SIZE="${IMAGE_SIZE:-24}"
export JPG_QUALITY="${JPG_QUALITY:-10}"

echo "Running tests with cabal..."
echo ""

# Run tests
cabal test --test-show-details=direct

echo ""
echo "=== Tests completed successfully! ==="
