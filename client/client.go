package main

import (
	"github.com/webview/webview"
	"encoding/base64"
	"fmt"
)

const indexHtml = `
<!DOCTYPE html>
<html lang="en">
<body>
Hi
</body>
</html>
`

func main() {
	debug := true
	w := webview.New(debug)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	url := "data:text/html;base64," + base64.StdEncoding.EncodeToString([]byte(indexHtml))
	fmt.Println(url)
	w.Navigate(url)
	w.Run()
}
