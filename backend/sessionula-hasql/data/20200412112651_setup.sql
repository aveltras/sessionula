-- migrate:up
CREATE TABLE IF NOT EXISTS sessionula (
   session_id text NOT NULL PRIMARY KEY,
   session_data bytea NOT NULL,
   created_at timestamptz,
   accessed_at timestamptz
);

-- migrate:down
DROP TABLE IF EXISTS sessionula;
