from tkinter import *
from tkinter import ttk

def calculate(*args):
    try:
        value = float(feet.get())
        meters.set(int(0.3048 * value * 10000.0 + 0.5)/10000.0)
    except ValueError:
        pass

def social_click(*args):
    pass

def contacts_click(*args):
    pass

def sent_click(*args):
    pass

def inbox_click(*args):
    pass

def money_click(*args):
    pass

def main():
    root = Tk()
    root.title("Feet to Meters")
    
    main_frame = ttk.Frame(root)
    main_frame.grid(column=0, row=0, sticky=(N, W, E, S))
    root.columnconfigure(0, weight=1)
    root.rowconfigure(0, weight=1)

    button_frame = ttk.Frame(main_frame)
    button_frame.grid(column=0, row=0)

    ttk.Button(
        button_frame, text="Social", command=social_click).grid(
        column=0, row=0)
    ttk.Button(
        button_frame, text="Contacts", command=contacts_click).grid(
        column=1, row=0)
    ttk.Button(
        button_frame, text="Sent", command=sent_click).grid(
        column=2, row=0)
    ttk.Button(
        button_frame, text="Inbox", command=inbox_click).grid(
        column=3, row=0)
    ttk.Button(
        button_frame, text="Money", command=money_click).grid(
        column=4, row=0)

    page_frame = ttk.Frame(main_frame)
    page_frame.grid(column=0, row=1)

    ttk.Label(page_frame, text="the page goes here We need to do two things: create the widget itself, and then place it onscreen").grid(
        column=0, row=0)
    
    root.mainloop()

main()
