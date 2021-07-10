package main

import (
	"github.com/webview/webview"
)

func main() {
	debug := true
	w := webview.New(debug)
	defer w.Destroy()

	w.SetTitle("Does it bind any ports?")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate("https://bbc.co.uk")
	w.Run()
}
