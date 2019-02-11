ALTER TABLE fruit
    ADD COLUMN color TEXT NOT NULL;
INSERT INTO fruit (name, color) VALUES (
    'lemon', 'yellow'
);