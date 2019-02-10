ALTER TABLE fruit
    ADD COLUMN color TEXT NOT NULL DEFAULT 'green';
INSERT INTO fruit (name, color) VALUES (
    'lemon', 'yellow'
);