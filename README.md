# Tangram editor in Elm

Example Elm SVG application with drag-and-drop.

# Installation

This program requires the LocalStorage effects module which is not yet published, so installation is a bit complicated.

```shell
cd workdir
git clone https://github.com/fredcy/elm-tangram-svg.git
git clone https://github.com/fredcy/localstorage.git
git clone https://github.com/fredcy/elm-tools.git

cd elm-tangram-svg

# Create a localized copy of the localstorage package.

make prepare PYTHON=/usr/local/bin/python27

# Build elm-tangram-svg

make
```

# Running

Open scene.html to view.

To run with automatic reloading for development, run `modd`.

