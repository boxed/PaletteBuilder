# PaletteBuilder

An Elm application for building and managing theme color palettes for projects.

Based on the approach from [Building Your Color Palette](https://www.refactoringui.com/previews/building-your-color-palette) by Refactoring UI.

## Features

- Create and edit color palettes
- Generate harmonious color schemes
- Export palettes in various formats
- Preview colors in context

## Development

### Prerequisites

- [Elm](https://elm-lang.org/) 0.19.1

### Running locally

```bash
elm reactor
```

Then open http://localhost:8000

### Building

```bash
elm make src/Main.elm --output=main.js
```

For optimized production build:

```bash
elm make src/Main.elm --optimize --output=main.js
```

## License

MIT
