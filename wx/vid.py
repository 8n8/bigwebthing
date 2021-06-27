import wx
import wx.media

class TestPanel(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, title='Media Player')
        self.testMedia = wx.media.MediaCtrl(self, style=wx.SIMPLE_BORDER)
        self.media = 'vid.mp4'
        self.testMedia.Bind(wx.media.EVT_MEDIA_LOADED, self.play)
        self.testMedia.Bind(wx.media.EVT_MEDIA_FINISHED, self.quit)
        self.Bind(wx.media.EVT_MEDIA_STOP, self.OnMediaStop, self.testMedia)
        self.Bind(wx.EVT_CLOSE, self.quit)
        if self.testMedia.Load(self.media):
            pass
        else:
            print("Media not found")
            self.quit(None)
        self.Show()

    def play(self, event):
        self.testMedia.Play()

    def quit(self, event):
        self.testMedia.Stop()
        self.Destroy()

    # Sets the mp4 file in a loop for testing only
    def OnMediaStop(self, event):
        self.testMedia.Seek(0)
        event.Veto()

if __name__ == '__main__':
    app = wx.App()
    Frame = TestPanel()
    app.MainLoop()
