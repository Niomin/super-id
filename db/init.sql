CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Legacy table for CRUD operations (kept for backward compatibility)
CREATE TABLE IF NOT EXISTS identities (
    uuid UUID PRIMARY KEY,
    app_id TEXT NOT NULL,
    payload_base64 TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_identities_uuid ON identities(uuid);
CREATE INDEX IF NOT EXISTS idx_identities_app_id ON identities(app_id);

-- Event Sourcing tables
-- Events table: stores all domain events
CREATE TABLE IF NOT EXISTS events (
    id BIGSERIAL PRIMARY KEY,
    stream_id TEXT NOT NULL,
    event_type TEXT NOT NULL,
    event_data JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    version INTEGER NOT NULL DEFAULT 1,
    metadata JSONB DEFAULT '{}'::jsonb
);

CREATE INDEX IF NOT EXISTS idx_events_stream_id ON events(stream_id);
CREATE INDEX IF NOT EXISTS idx_events_created_at ON events(created_at);
CREATE INDEX IF NOT EXISTS idx_events_event_type ON events(event_type);

-- Snapshots table: stores aggregate snapshots for performance
CREATE TABLE IF NOT EXISTS snapshots (
    stream_id TEXT PRIMARY KEY,
    aggregate_data JSONB NOT NULL,
    version INTEGER NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Projections table: materialized read models
CREATE TABLE IF NOT EXISTS identity_read_model (
    uuid UUID PRIMARY KEY,
    app_id TEXT NOT NULL,
    payload_base64 TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    is_abdicated BOOLEAN DEFAULT FALSE,
    validation_count INTEGER DEFAULT 0,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_read_model_app_id ON identity_read_model(app_id);
CREATE INDEX IF NOT EXISTS idx_read_model_abdicated ON identity_read_model(is_abdicated);
