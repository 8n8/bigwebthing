#!/bin/bash

set -e

elm make src/Main.elm --optimize

cp index.html ..
