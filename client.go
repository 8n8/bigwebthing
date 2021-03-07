package main

import (
	"github.com/webview/webview"
)

func main() {
	w := webview.New(false)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate("https://bbc.co.uk")
	w.Run()
}
