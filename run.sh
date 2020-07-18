if go install nacl; then
    gofmt -w go/src/nacl/*
    go/bin/deadcode go/src/nacl
    echo "Build succeeded "`date`
fi
