package main

import (
	"bytes"
	"errors"
	"fmt"
	"io/ioutil"
	"os/exec"
	"time"
)

func main() {
	fmt.Printf("Starting build at %s\n", time.Now().Format("Mon Jan 2 15:04:05"))
	err := build()
	if err != nil {
		fmt.Println(err.Error())
	}
}

func build() error {
	ok, err := run("go", "test")
	fmt.Println(string(ok))
	if err != nil {
		return err
	}

	ok, err = run("go", "install")
	fmt.Println(string(ok))
	if err != nil {
		return err
	}

	ok, err = run("golangci-lint", "run")
	fmt.Println(string(ok))
	if err != nil {
		return err
	}

	_, err = run("gofmt", "-w", "*.go")
	return err
}

func run(name string, args ...string) ([]byte, error) {
	cmd := exec.Command(name, args...)

	var stderr bytes.Buffer
	var stdout bytes.Buffer

	cmd.Stderr = &stderr
	cmd.Stdout = &stdout
	cmd.Dir = "go"

	errRun := cmd.Run()

	ok, errOut := ioutil.ReadAll(&stdout)
	if errOut != nil {
		panic(errOut)
	}

	if errRun != nil {
		errBytes, err := ioutil.ReadAll(&stderr)
		if err != nil {
			panic(err)
		}

		return ok, errors.New(string(errBytes))
	}

	return ok, nil
}
