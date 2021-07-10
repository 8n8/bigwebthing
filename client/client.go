package main

import (
	"github.com/gotk3/gotk3/gtk"
)

func main() {
	gtk.Init(nil)
	win, err := gtk.WindowNew(gtk.WINDOW_TOPLEVEL)
	if err != nil {
		panic(err)
	}

	win.SetTitle("Hello True")
	win.Connect("destroy", func() {
		gtk.MainQuit()
	})

	l, err := gtk.LabelNew("Hello there True!")
	if err != nil {
		panic(err)
	}

	win.Add(l)

	win.SetDefaultSize(800, 600)

	win.ShowAll()

	gtk.Main()
}
