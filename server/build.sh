go test &&
go install &&
golangci-lint run &&
gofmt -w *.go
