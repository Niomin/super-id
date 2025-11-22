CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS identities (
    uuid UUID PRIMARY KEY,
    app_id TEXT NOT NULL,
    payload_base64 TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_identities_uuid ON identities(uuid);
CREATE INDEX IF NOT EXISTS idx_identities_app_id ON identities(app_id);
