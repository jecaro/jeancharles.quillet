---

title: The home-manager function that changes everything

---

I have been a happy user of [NixOS] for a while now. I have it running on two 
different computers at home. And for work, I use [nixpkgs] on top of Ubuntu. I 
share the configuration between the two using a file named `./packages.nix`. It 
basically contains the list of packages to be installed on [NixOS] when running 
it or it can be used with `nix-env -i` when running Ubuntu. This is simple 
straight forward approach (as simple as [nixpkgs] can be) and it's been working 
fine so far.

Something I was annoyed tho, was keeping my dotfiles on a separate git 
repository (deployed using the venerable [stow]). Indeed, it would be great to 
have the dotfiles along the packages to install. Such setup would garantee that 
the dotfiles are always in sync with the packages versions.

This kind of concern is usually addressed by the use of [home-manager], a nix 
tool designed to handled dotfiles in a declarative way. But I wasn't very eager 
to try it out, as, for what I understand, [home-manager] creates links to 
dotfiles in your home directory. These dotfiles are created in the nix store, 
either using [configuration 
options](https://rycee.gitlab.io/home-manager/options.html), either by copying 
a specified file. So they are read only just like anything in the nix store. 
What it means in pratice is that every single edit of a dotfile means 
rebuilding and reinstalling the [home-manager] configuration for it to update 
those links.

And for me, I find it cumbersome, that cannot work. I fiddle quite a bit with my 
dotfiles. I need to edit them very often to reflect or change my current 
workflow. I try some changes for a bit of time to see if they work. For 
example, I might want to try a [neovim] plugin or hack a bit on my [xmonad] 
configuration. Sometimes I'm happy with the changes and I keep them, sometimes 
I'm not and I revert them.

But at some point, I found a function defined in [home-manager] that solves 
this issue: [mkOutOfStoreSymlink]. So instead of creating a link to a read only 
file in the nix store, that function creates a link to a file located just 
anywhere, like in the very same repository as the [home-manager] configuration 
for example.

Using this function gives us the best of both worlds: dotfiles and packages in 
the same repository handled by [home-manager] and the ability to edit the 
dotfiles as much as you want. Note that we loose a bit of safety here as the 
dotfiles [home-manager] will link might not be there in the first place and 
this will lead to broken links. But I think it's a fair trade off.

In the end, I now have a single repository with a flake which defines:

- the [NixOS] configuration for my home computers
- my [home-manager] configuration for my work computer
- another lightweight [home-manager] configuration with command line tools only 
  intended to be used on servers

With everything as DRY as possible thanks to the nix programming language.

It took me a while to move to [home-manager] but now I'm glad I did. This setup 
is all in one place and super easy to maintain. It feels great to deploy 
confidently the packages along the configuration files and be very sure that 
everything will work the exact the same way on every single machine.

[NixOS]: https://nixos.org/
[home-manager]: https://github.com/nix-community/home-manager
[mkOutOfStoreSymlink]: 
https://github.com/nix-community/home-manager/commit/91551c09d48583230b36cf759ad703b5f1d83d9a
[neovim]: https://neovim.io/
[nixpkgs]: https://github.com/NixOS/nixpkgs
[stow]: https://www.gnu.org/software/stow/
[xmonad]: https://xmonad.org/
