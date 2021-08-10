#!/bin/bash

set -e

go install
gofmt -w *.go
golangci-lint run
