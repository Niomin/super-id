# Super-ID - The Ultimate Identity Service

## Prerequisites

- Docker and Docker Compose
- For local development without Docker: Haskell (GHC 9.12.2), Cabal, PostgreSQL

## Running with Docker Compose

The easiest way to run the application:

```bash
docker-compose up --build
```

This will:
1. Start PostgreSQL on port 5432 (using postgres:alpine image)
2. Build and start the Haskell application on port 3000
3. Initialize the database schema automatically

## API Endpoints

### 1. ACQUIRE /

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

### 2. VALIDATE /{uuid}

**HTTP Method:** `VALIDATE`
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

### 3. ABDICATE /{uuid}

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

## Testing with curl

**Note:** Replace `YOUR-UUID-HERE` with the actual UUID returned from ACQUIRE requests.

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
│   └── AudioGen.hs          # WAV audio generation
├── db/
│   └── init.sql             # Database schema
├── Dockerfile               # Multi-stage Haskell build
├── docker-compose.yml       # Docker orchestration
├── .env                     # Environment variables
└── super-id.cabal           # Haskell dependencies
```

## Technologies

- **Language:** Haskell
- **Web Framework:** Scotty
- **Database:** PostgreSQL (via postgresql-simple)
- **Config Format:** Dhall
- **Image Generation:** JuicyPixels
- **Audio Generation:** espeak-ng
- **Containerization:** Docker, Docker Compose
