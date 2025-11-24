# Super-ID - The Ultimate Identity Service

A distributed identity management system with custom HTTP methods, Dhall-based configuration, and comprehensive RFC2324 compliance.

**Features:**
- Custom HTTP methods: `ACQUIRE`, `VALIDATE`, `ABDICATE`, `HELP`
- Dhall-powered API with strong type safety
- Multiple response formats: PNG, JPEG, GIF, WAV
- RFC2324/HTCPCP compliant (Hyper Text Coffee Pot Control Protocol)
- Event Sourcing with full audit trail and time travel capabilities
- Production-ready Docker setup with minimal image size
- PostgreSQL backend with optimized indexing

## Prerequisites

- Docker and Docker Compose
- For local development without Docker: Haskell (GHC 9.12.2), Cabal, PostgreSQL

## Event Sourcing

**NEW!** Super-ID now supports event sourcing for complete audit trails and CQRS architecture.

See **[EVENTSOURCING.md](EVENTSOURCING.md)** for detailed setup and usage instructions.

Key features:
- Immutable event log of all state changes
- Event replay and time travel capabilities
- Optimized read models (projections)
- Snapshot support for performance
- PostgreSQL-backed event store

## Running with Docker Compose

### Development

The easiest way to run the application:

```bash
docker-compose up --build
```

This will:
1. Start PostgreSQL on port 5432 (using postgres:alpine image)
2. Build and start the Haskell application on port 3000
3. Initialize the database schema automatically

### Production

For optimized production builds with minimal image size:

```bash
docker-compose -f docker-compose.prod.yml up --build -d
```

Production features:
- **Alpine-based runtime** (minimal size)
- **Stripped binary** with UPX compression
- **Non-root user** for security
- **Resource limits** (CPU: 1 core, Memory: 512MB)
- **Auto-restart** on failure
- **Multi-stage build** (only executable in final image)

Check logs:
```bash
docker-compose -f docker-compose.prod.yml logs -f app
```

Stop:
```bash
docker-compose -f docker-compose.prod.yml down
```

**Image Size Comparison:**
- Development build (~200-400MB with debian:bookworm-slim)
- Production build (~50-100MB with Alpine + stripped binary + UPX)

## API Endpoints

### 1. HELP /

**HTTP Method:** `HELP` (custom method)
**Content-Type:** `text/markdown`

Returns the contents of this README.md file.

**Example:**
```bash
curl -X HELP http://localhost:3000/
```

### 2. ACQUIRE /

**HTTP Method:** `ACQUIRE` (custom method)
**Content-Type:** `application/dhall`
**Accepts:** `image/png`, `image/jpg`, `image/jpeg`, `image/gif`, `audio/x-wav`

**Request Example:**
```dhall
{ appId = "MY_APP_ID"
, payload = { teamId = +123 }
} : { appId : Text, payload : { teamId : Integer } }
```

**Response:**
- `image/png`: PNG image with UUID as text (height configurable via `IMAGE_SIZE` env var, default 24px)
- `image/jpg`: JPEG with UUID as text (height configurable via `IMAGE_SIZE`, quality configurable via `JPG_QUALITY`, defaults: 24px height, quality 1)
- `image/gif`: Animated GIF showing each UUID character (size configurable via `IMAGE_SIZE`, default 24x24px)
- `audio/x-wav`: WAV file with robotic voice reading the UUID

The UUID is persisted in the database along with the appId and securely base64-encoded payload.

### 3. VALIDATE /{uuid}

**HTTP Method:** `VALIDATE` (custom method)
**Content-Type:** `application/dhall`

**Request Example (with payload validation):**
```dhall
{ appId = "MY_APP_ID"
, payload = { teamId = +123 }
} : { appId : Text, payload : { teamId : Integer } }
```

**Request Example (without payload - just appId validation):**
```dhall
{ appId = "MY_APP_ID"
} : { appId : Text }
```

**Response:**
- `204`: Success - UUID exists with matching appId (and payload if provided)
- `417`: Failure - validation failed

### 4. ABDICATE /{uuid}

**HTTP Method:** `ABDICATE` (custom method)
**Content-Type:** `application/dhall`

**Request Example:**
```dhall
{ appId = "MY_APP_ID"
, iPromiseAppIdMY_APP_IDisMine = true
}
```

Note: The field `iPromiseAppIdMY_APP_IDisMine` must match the pattern `iPromiseAppId{appId}IsMine` where `{appId}` is replaced with the actual appId value.

**Response:**
- `204`: Success - entry deleted
- `403`: Promise field is false or missing/invalid
- `404`: Entry not found

### 5. RFC2324 Support (HTCPCP/1.0)

This API implements **RFC2324** - Hyper Text Coffee Pot Control Protocol.

**Any coffee-related request returns HTTP 418 "I'm a teapot"**

Triggers:
- HTTP method `BREW` or `WHEN`
- Header `Accept-Additions: Coffee`
- Header `Content-Type` containing "coffee"

**Response:**
- Status: `418 I'm a teapot`
- Body: Random ASCII art teapot (4 variations)

**Examples:**
```bash
# Using BREW method
curl -X BREW http://localhost:3000/

# Using Accept-Additions header
curl -H "Accept-Additions: Coffee" http://localhost:3000/

# Using Content-Type
curl -H "Content-Type: message/coffeepot" http://localhost:3000/
```

Sample response:
```
        ___
       {___}
     .-'   '-.
    /  Tea    \
   |   Only!   |
    \  .....  /
     '-._._.-'
       |_|_|
```

## Database Schema

```sql
CREATE TABLE identities (
    uuid UUID PRIMARY KEY,
    app_id TEXT NOT NULL,
    payload_base64 TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## Environment Variables

- `IMAGE_SIZE`: Height/width of generated images in pixels (default: 24). Must be divisible by 12 for best results.
- `JPG_QUALITY`: JPEG compression quality from 1 (lowest, highest compression) to 100 (highest quality, lowest compression). Default: 1.
- `SNAPSHOT_FREQUENCY`: Take aggregate snapshot every N events (default: 50).

## Local Development

Build locally:
```bash
cabal build
```

Run locally (requires PostgreSQL running):
```bash
# Set environment variables
export POSTGRES_HOST=localhost
export POSTGRES_PORT=5432
export POSTGRES_USER=superid
export POSTGRES_PASSWORD=superid123
export POSTGRES_DB=superid
export IMAGE_SIZE=24
export JPG_QUALITY=10

# Run the application
cabal run super-id
```

## Running Tests

Super-ID includes comprehensive API tests using **hspec** and **hspec-wai**.

### Prerequisites for Testing

Ensure PostgreSQL is running (you can use docker-compose):
```bash
docker-compose up -d postgres
```

### Run Tests Locally

Using the test runner script:
```bash
./run-tests.sh
```

Or directly with cabal:
```bash
# Set environment variables
export POSTGRES_HOST=localhost
export POSTGRES_PORT=5432
export POSTGRES_USER=superid
export POSTGRES_PASSWORD=superid123
export POSTGRES_DB=superid

# Run tests
cabal test --test-show-details=direct
```

### CI/CD Integration

A GitHub Actions workflow is provided in `.github/workflows/test.yml`. It will:
1. Set up PostgreSQL service
2. Install Haskell and system dependencies
3. Build the project
4. Run all tests

The tests cover:
- ✓ HELP endpoint (returns README.md)
- ✓ ACQUIRE endpoint (all formats: PNG, JPG, GIF, WAV)
- ✓ VALIDATE endpoint (with and without payload)
- ✓ ABDICATE endpoint (including promise validation)
- ✓ RFC2324 compliance (BREW, WHEN methods, coffee headers)
- ✓ Health check endpoint
- ✓ Integration workflows

## Testing with curl

**Note:** Replace `YOUR-UUID-HERE` with the actual UUID returned from ACQUIRE requests.

Example HELP request:
```bash
curl -X HELP http://localhost:3000/
```

Example ACQUIRE request (PNG):
```bash
curl -X ACQUIRE \
  -H "Content-Type: application/dhall" \
  -H "Accept: image/png" \
  -d '{ appId = "TEST_APP", payload = { teamId = +42 } } : { appId : Text, payload : { teamId : Integer } }' \
  http://localhost:3000/ --output uuid.png
```

Example ACQUIRE request (GIF):
```bash
curl -X ACQUIRE \
  -H "Content-Type: application/dhall" \
  -H "Accept: image/gif" \
  -d '{ appId = "TEST_APP", payload = { teamId = +42 } } : { appId : Text, payload : { teamId : Integer } }' \
  http://localhost:3000/ --output uuid.gif
```

Example ACQUIRE request (WAV audio):
```bash
curl -X ACQUIRE \
  -H "Content-Type: application/dhall" \
  -H "Accept: audio/x-wav" \
  -d '{ appId = "TEST_APP", payload = { teamId = +42 } } : { appId : Text, payload : { teamId : Integer } }' \
  http://localhost:3000/ --output uuid.wav
```

Example VALIDATE request (with payload):
```bash
curl -X VALIDATE \
  -H "Content-Type: application/dhall" \
  -d '{ appId = "TEST_APP", payload = { teamId = +42 } } : { appId : Text, payload : { teamId : Integer } }' \
  http://localhost:3000/YOUR-UUID-HERE -v
```

Example VALIDATE request (without payload):
```bash
curl -X VALIDATE \
  -H "Content-Type: application/dhall" \
  -d '{ appId = "TEST_APP" } : { appId : Text }' \
  http://localhost:3000/YOUR-UUID-HERE -v
```

Example ABDICATE request:
```bash
curl -X ABDICATE \
  -H "Content-Type: application/dhall" \
  -d '{ appId = "TEST_APP", iPromiseAppIdTEST_APPIsMine = True }' \
  http://localhost:3000/YOUR-UUID-HERE -v
```

Example RFC2324 coffee requests (returns 418):
```bash
# Try to brew coffee
curl -X BREW http://localhost:3000/ -v

# Request with coffee header
curl -H "Accept-Additions: Coffee" http://localhost:3000/ -v

# Coffee content type
curl -H "Content-Type: message/coffeepot" http://localhost:3000/ -v
```

## Project Structure

```
super-id/
├── app/
│   └── Main.hs              # Thin controllers, HTTP routing
├── src/
│   ├── Repository.hs        # Database access layer
│   ├── Logic.hs             # Business logic
│   ├── DhallTypes.hs        # Dhall parsing
│   ├── ImageGen.hs          # PNG/JPG/GIF generation
│   ├── AudioGen.hs          # WAV audio generation
│   ├── Events.hs            # Domain events and aggregates
│   └── EventStore.hs        # Event sourcing infrastructure
├── test/
│   ├── Spec.hs              # Test discovery
│   └── ApiSpec.hs           # API endpoint tests
├── db/
│   └── init.sql             # Database schema (with event tables)
├── .github/
│   └── workflows/
│       └── test.yml         # CI/CD pipeline
├── Dockerfile               # Multi-stage Haskell build
├── Dockerfile.prod          # Production optimized build
├── docker-compose.yml       # Docker orchestration
├── docker-compose.prod.yml  # Production compose
├── run-tests.sh             # Test runner script
├── .env                     # Environment variables
├── EVENTSOURCING.md         # Event sourcing documentation
└── super-id.cabal           # Haskell dependencies
```

## Technologies

- **Language:** Haskell (GHC 9.12.2)
- **Web Framework:** Scotty (WAI/Warp)
- **Database:** PostgreSQL (via postgresql-simple)
- **Event Sourcing:** Custom implementation with PostgreSQL JSONB storage
- **Config Format:** Dhall
- **Image Generation:** JuicyPixels
- **Audio Generation:** espeak-ng
- **Testing:** hspec, hspec-wai
- **CI/CD:** GitHub Actions
- **Containerization:** Docker, Docker Compose
