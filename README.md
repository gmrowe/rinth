# rinth

A clojure maze generation project based on [Mazes for Programmers](https://pragprog.com/titles/jbmaze/mazes-for-programmers/) by Jamis Buck

## Requirements

- [Java runtime](https://openjdk.org/)
- [Clojure]()

## Usage

The project can be run using the ./scripts/run script. Executing `./scripts/run --help` shows a list of accepted commands.

To generate a 32x32 maze using the sidewinder alogorithm and show the maze onscreen, the following command may be executed:

   ```sh
   ./scripts/run -c 32 -r 32 sidewinder
   ```
