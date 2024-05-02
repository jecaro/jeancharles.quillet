---

title: Columns - a new XMonad layout
lastmod: 2024-05-02

---

Today I would like to share the [XMonad][xmonad] layout I have written sometime 
ago. It has been very stable for a while and I think it is now a good time to 
share it.

It is a layout that organizes windows in columns while offering great 
flexibility when it comes to arrange the windows. I use it along a [tabbed 
layout][tabbed-layout] and with those two layouts, I do not need anything else 
really.

Let's first start with a quick demo of the layout in action:

![](/images/columns.mkv)

As one can see in the video, the layout organizes the windows in columns. You 
can have as many columns as you like. The windows can be freely moved from one 
column to another, left and right, and, in a single column, up and down. They 
can also be resized both horizontally and vertically. Adding new column is done 
by moving a window to the spot you want the column to appear. Simple, easy.

I also use [this trick][sublayout] to dock windows together in tabs as 
showcased in the video.

I can change the focus of the windows using [WindowNavigation]. Every other 
command is handled by sending custom messages to the layout. See the [source 
code][gist] for details.

With this layout, I can arrange the windows really in any way I want while 
keeping them always tiled.

I could not find anything like this in the [xmonad-contrib] repository. The 
closest I found was [ResizableTall] that I used for a while before developping 
my own. However, it is limited to two columns. Another interesting one is 
[rowOfColumns]. It is pretty similar to mine, but it wasn't working very well 
with the tabbed sub-layout if I remember correctly.

That's it. The source of the layout is available in this [gist].

I hope it can be useful for other people. I am usually more than happy to 
contribute to OSS but in this very case I do not want to send a PR to 
[xmonad-contribu]. I find this repository cluttered enough with hacky modules 
that I do not want to add another one (pretty hacky too).

But feel free to use it as you will and let me know if you like it.

[xmonad]: https://xmonad.org/
[xmonad-contrib]: https://github.com/xmonad/xmonad-contrib
[sublayout]: 
https://hackage.haskell.org/package/xmonad-contrib-0.18.0/docs/XMonad-Layout-SubLayouts.html#v:subLayout
[ResizableTall]: 
https://hackage.haskell.org/package/xmonad-contrib-0.18.0/docs/XMonad-Layout-ResizableTile.html 
[rowOfColumns]: 
https://hackage.haskell.org/package/xmonad-contrib-0.18.0/docs/XMonad-Layout-Groups-Examples.html#g:2 
[tabbed-layout]: 
https://hackage.haskell.org/package/xmonad-contrib-0.18.0/docs/XMonad-Layout-Tabbed.html
[WindowNavigation]: 
https://hackage.haskell.org/package/xmonad-contrib-0.18.0/docs/XMonad-Layout-WindowNavigation.html
[gist]: https://gist.github.com/jecaro/a6684da4f6e5891211f19d2a7c959b44

