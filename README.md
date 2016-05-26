# elm-svg-example
Example Elm SVG application with drag-and-drop.

# Installation

This program requires the LocalStorage effects module which is not yet published, so installation is a bit complicated.

```shell
cd workdir
git clone https://github.com/fredcy/elm-tangram-svg.git
git clone https://github.com/fredcy/localstorage.git
git clone https://github.com/NoRedInk/elm-ops-tooling.git

cd elm-tangram-svg

# Do initial build. It will fail with an error about 'LocalStorage'
make

# Install the module providing LocalStorage as if it were a package.
# This modifies elm-package.json and elm-stuff.
../elm-ops-tooling/elm_self_publish.py ../localstorage .

# Build again; this should now work.
make
```

# Running

Open scene.html to view.

To run with automatic reloading for development, run `modd`.

