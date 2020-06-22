if elm make src/Main.elm --output=../static/main.js --optimize; then
    elm-format --yes src/*.elm
    elm-analyse
fi
