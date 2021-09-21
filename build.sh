#!/bin/bash

set -e

flake8
pytest
black *.py
