# minivi-scaffold

a simple terminal-based text editor

## Requirements

- unix-like os (i.e. `macOS`, `linux` or WSL on Windows)
- `ghc 9.4.8` (not tested with other `ghc` vesions)
- `cabal 3.12` (any 3.0+ version should work)

## Building and Running

To build the project:

```sh
$ cabal build
```

To edit a file (e.g. `test.txt`):

``` sh
$ cabal exec minivi -- test.txt
```

You can also start the editor without specifying a file:

```sh
$ cabal exec minivi
```

You can use `cabal repl` to start ghci.

To run tests:

```sh
$ cabal test
```

## Project Structure

```
minivi/
├── README.md           # This file
├── minivi.cabal        # Cabal configuration file
├── app/
│   └── Main.hs         # Entry point with main loop
├── c/
│   └── terminal.c      # C code for terminal operations
├── src/
│   ├── App.hs          # Data models and state definitions
│   ├── Update.hs       # Logic for updating editor state
│   ├── Util.hs         # Utility functions and helpers
│   └── View.hs         # Terminal rendering functions
└── test/
    └── Test.hs         # Test suite
```

## Usage Instructions

### Basic Usage
1. **Opening a file**: `cabal exec minivi -- filename.txt`
2. **Creating a new file**: Start without parameters `cabal exec minivi` then save with `:w filename.txt`
3. **Switching modes**: Press `i` for Insert mode, `Esc` to return to Normal mode
4. **Saving a file**: Press `:` then type `w` and press Enter
5. **Quitting**: Press `:` then type `q` and press Enter (or `:q!` to force quit)

### Key Bindings

#### Modes
- `Esc` - Return to Normal mode from any other mode
- `i` - Enter Insert mode (for typing text)
- `:` - Enter Command mode (for commands like saving and quitting)
- To exit the program, press `:` to enter Command mode, then type `q` and press Enter

#### Navigation (Normal Mode)
- Arrow keys - Move cursor
- `h`, `j`, `k`, `l` - Move left, down, up, right
- `w` - Move to next word
- `b` - Move to previous word
- `0` - Move to beginning of line
- `$` - Move to end of line

#### Editing (Insert Mode)
- Any character keys - Insert text
- `Backspace` - Delete character before cursor
- `Enter` - Create a new line

#### Search
- `/` - Start search (type query and press Enter)
- `n` - Go to next match
- `N` - Go to previous match

#### History
- `u` - Undo last change
- `U` - Redo last undone change

#### Commands (Enter with `:`)
- `:w` - Save to current file
- `:w filename.txt` - Save to specified file
- `:q` - Quit (will warn if unsaved changes)
- `:q!` - Force quit without saving

## Features

### Basic Features

- **Modes**: Normal, Insert, Command, and Message modes (vim-like)
- **Navigation**: Arrow keys and h, j, k, l for movement
- **Content rendering** with proper handling of tabs and terminal size
- **Cursor handling** with proper viewport scrolling
- **Editing capabilities**:
  - Insert characters in Insert mode
  - Delete characters (backspace)
  - Create new lines with Enter

### Commands

- `:w` - Write the current buffer to the current file
- `:w filename.txt` - Write the current buffer to the specified file
- `:q` - Quit the editor (will warn if buffer is modified)
- `:q!` - Force quit without saving

### Advanced Navigation

- `w` - Move to the start of the next word
- `b` - Move to the start of the previous word
- `0` - Jump to the beginning of the current line
- `$` - Jump to the end of the current line

### Text Search

- `/` - Start a text search (type your query and press Enter)
- `n` - Find the next occurrence of the search pattern
- `N` - Find the previous occurrence of the search pattern

### History and Undo/Redo

- `u` - Undo the last change (up to 4 steps)
- `U` - Redo the last undone change

## Implementation Details

The editor is built with a model-view-update architecture:

- **App.hs**: Contains the data types and model definitions
- **View.hs**: Handles the rendering of the editor state
- **Update.hs**: Contains the logic for updating the state based on user input
- **Util.hs**: Provides utility functions and low-level terminal operations
- **Main.hs**: Entry point that sets up the editor and main loop

## Acknowledgments

This project was extended with the assistance of Claude 3.7 Sonnet.# minivi-scaffold