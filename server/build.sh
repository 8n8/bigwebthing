go test &&
go install &&
gofmt -w *.go &&
golangci-lint run
