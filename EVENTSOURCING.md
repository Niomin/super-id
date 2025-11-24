# Event Sourcing Setup Guide

This guide explains how to set up and use event sourcing in the Super-ID project.

## Overview

Super-ID now implements **Event Sourcing** using a custom, lightweight implementation with a PostgreSQL backend. Event sourcing provides:

- **Complete audit trail** - Every state change is recorded as an immutable event
- **Time travel** - Rebuild state at any point in time
- **Event replay** - Reprocess events for debugging or new projections
- **CQRS** - Separate read and write models for optimal performance

## Architecture

### Domain Events

Three event types are defined in `src/Events.hs`:

1. **IdentityAcquired** - Fired when a new identity is created
   - `acquiredAppId`: Application identifier
   - `acquiredPayload`: Base64-encoded payload
   - `acquiredFormat`: Response format (PNG, JPG, GIF, WAV)
   - `acquiredAt`: Timestamp

2. **IdentityValidated** - Fired when an identity is validated
   - `validatedAppId`: Application identifier
   - `validatedPayload`: Optional payload for validation
   - `validationSuccessful`: Result of validation
   - `validatedAt`: Timestamp

3. **IdentityAbdicated** - Fired when an identity is deleted
   - `abdicatedAppId`: Application identifier
   - `abdicatedAt`: Timestamp

### Event Store

The event store (`src/EventStore.hs`) provides a custom, lightweight implementation:

- **Event persistence** - Events stored in PostgreSQL using JSONB
- **Stream management** - Events grouped by aggregate ID (UUID)
- **Event replay** - Rebuild aggregates from event history
- **Snapshots** - Performance optimization for large event streams
- **No external dependencies** - Pure Haskell implementation on top of postgresql-simple

### Database Schema

#### Events Table
```sql
CREATE TABLE events (
    id BIGSERIAL PRIMARY KEY,
    stream_id TEXT NOT NULL,          -- UUID of the identity
    event_type TEXT NOT NULL,         -- Type of event
    event_data JSONB NOT NULL,        -- Event payload
    created_at TIMESTAMP,
    version INTEGER NOT NULL,         -- For optimistic concurrency
    metadata JSONB DEFAULT '{}'
);
```

#### Snapshots Table
```sql
CREATE TABLE snapshots (
    stream_id TEXT PRIMARY KEY,       -- UUID of the identity
    aggregate_data JSONB NOT NULL,    -- Serialized aggregate state
    version INTEGER NOT NULL,         -- Version at snapshot
    created_at TIMESTAMP
);
```

#### Read Model (Projection)
```sql
CREATE TABLE identity_read_model (
    uuid UUID PRIMARY KEY,
    app_id TEXT NOT NULL,
    payload_base64 TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    is_abdicated BOOLEAN DEFAULT FALSE,
    validation_count INTEGER DEFAULT 0,
    last_updated TIMESTAMP
);
```

## Development Environment Setup

### Prerequisites

- Docker and Docker Compose
- Haskell (GHC 9.12.2) and Cabal (for local development)
- PostgreSQL 14+ (or use Docker)

### Quick Start with Docker

1. **Clone the repository**
   ```bash
   cd /Users/aleksei/ScratchPad/super-id
   ```

2. **Configure environment variables**
   ```bash
   # .env file is already configured with defaults
   cat .env
   ```

3. **Start the services**
   ```bash
   docker-compose up --build
   ```

This will:
- Start PostgreSQL with event sourcing tables
- Build and start the Haskell application
- Initialize the database schema automatically

### Local Development (without Docker)

1. **Start PostgreSQL**
   ```bash
   docker-compose up -d postgres
   ```

2. **Install dependencies**
   ```bash
   cabal update
   cabal build --only-dependencies
   ```

3. **Build the project**
   ```bash
   cabal build
   ```

4. **Run the application**
   ```bash
   # Set environment variables
   export POSTGRES_HOST=localhost
   export POSTGRES_PORT=5432
   export POSTGRES_USER=superid
   export POSTGRES_PASSWORD=superid123
   export POSTGRES_DB=superid
   export IMAGE_SIZE=24
   export JPG_QUALITY=10
   export EVENT_SOURCING_ENABLED=true

   # Run
   cabal run super-id
   ```

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `SNAPSHOT_FREQUENCY` | Take snapshot every N events | `50` |
| `POSTGRES_HOST` | PostgreSQL host | `localhost` |
| `POSTGRES_PORT` | PostgreSQL port | `5432` |
| `POSTGRES_USER` | Database user | `superid` |
| `POSTGRES_PASSWORD` | Database password | `superid123` |
| `POSTGRES_DB` | Database name | `superid` |

## How It Works

### Write Path (Commands)

1. **Command received** (e.g., ACQUIRE request)
2. **Event created** (e.g., IdentityAcquired)
3. **Event stored** in events table
4. **Projection updated** (read model)
5. **Response returned** to client

### Read Path (Queries)

1. **Query received** (e.g., VALIDATE request)
2. **Read from projection** (identity_read_model table)
3. **Response returned** to client

### Event Replay

To rebuild state from events:

```haskell
-- Load all events for an aggregate
events <- loadEvents eventStore aggregateId

-- Rebuild aggregate state
let aggregate = rebuildAggregate events
```

## Testing

Run tests with event sourcing enabled:

```bash
export EVENT_SOURCING_ENABLED=true
./run-tests.sh
```

Or with cabal:
```bash
cabal test --test-show-details=direct
```

## Querying the Event Store

### View all events for an identity

```sql
SELECT
    event_type,
    event_data,
    created_at,
    version
FROM events
WHERE stream_id = 'YOUR-UUID-HERE'
ORDER BY created_at ASC;
```

### View current state (read model)

```sql
SELECT * FROM identity_read_model
WHERE uuid = 'YOUR-UUID-HERE';
```

### View event statistics

```sql
SELECT
    event_type,
    COUNT(*) as count,
    MIN(created_at) as first_event,
    MAX(created_at) as last_event
FROM events
GROUP BY event_type;
```

## Architecture

Super-ID uses **full event sourcing** with CQRS:

- All state changes stored as immutable events in `events` table
- Read models updated synchronously for fast queries
- Legacy `identities` table maintained for backward compatibility
- No flag needed - event sourcing is always active (YOLO!)

## Performance Considerations

### Snapshots

For aggregates with many events, snapshots improve performance:

- Snapshot taken every `SNAPSHOT_FREQUENCY` events (default: 50)
- Stored in `snapshots` table
- Rebuild starts from latest snapshot + newer events

### Projections

Read models are optimized for queries:

- Denormalized for fast reads
- Updated asynchronously (eventual consistency)
- Indexed on common query patterns

### Connection Pooling

Event store uses connection pooling:

```haskell
-- Pool configuration
createPool (connect connInfo) close
  1    -- stripes
  60   -- timeout (seconds)
  10   -- max connections per stripe
```

## Troubleshooting

### Events not appearing

Check event store logs:
```bash
docker-compose logs -f app
```

Verify database connection:
```bash
docker-compose exec postgres psql -U superid -d superid -c "SELECT COUNT(*) FROM events;"
```

### Read model out of sync

Rebuild projections from events:
```bash
# Connect to database
docker-compose exec postgres psql -U superid -d superid

# Clear read model
TRUNCATE identity_read_model;

# Projections will rebuild automatically from events
```

### Performance issues

Check event counts:
```sql
SELECT stream_id, COUNT(*) as event_count
FROM events
GROUP BY stream_id
ORDER BY event_count DESC
LIMIT 10;
```

Consider lowering `SNAPSHOT_FREQUENCY` for high-volume streams.

## Implementation Notes

This is a **custom, lightweight event sourcing implementation** built directly on PostgreSQL, not using external event sourcing libraries. This approach provides:

- **Simplicity** - Easy to understand and maintain
- **Flexibility** - Full control over event schema and storage
- **PostgreSQL-native** - Leverages JSONB for efficient event storage
- **No external dependencies** - Uses only postgresql-simple

## Additional Resources

- [Event Sourcing pattern](https://martinfowler.com/eaaDev/EventSourcing.html)
- [CQRS pattern](https://martinfowler.com/bliki/CQRS.html)
- [PostgreSQL JSON support](https://www.postgresql.org/docs/current/datatype-json.html)

## Next Steps

1. Implement event versioning and migrations
2. Add event streaming to external systems
3. Build additional projections for analytics
4. Implement event-driven workflows
5. Add event sourcing tests
