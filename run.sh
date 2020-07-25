if go install client; then
    gofmt -w go/src/client/*
    go/bin/deadcode go/src/client
    echo "Build succeeded "`date`
fi
