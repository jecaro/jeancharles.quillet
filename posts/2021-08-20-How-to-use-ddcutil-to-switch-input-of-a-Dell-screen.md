---

title: How to use ddcutil to switch input of a Dell screen

---

I have recently acquired a Dell UltraSharp 34, a very nice screen able to drive
up to four different inputs. It also has an USB hub.

I use it with two laptops:

- one laptop for working, plugged in USB-C

- one laptop for testing, plugged in DisplayPort and USB upstream

As for the USB hub, I plug in it my keyboard, mouse and headphones. Now, when I
toggle the input the USB hub automatically switches to the laptop which
currently uses the screen. This is cool but not that easy. To effectively
switch the input source, one needs to click on a button behind the screen,
choose the function `input source` with a kind of joystick, then select the
desired input and click again. This is quite complicated for such a simple and
common task.

Fortunately the screen provides an I2C interface allowing to programmatically
change the screen settings from a computer connected to it. On Linux these
settings can be changed using the software [ddcutil](https://www.ddcutil.com/).

Firstly, in order to use this tool, the kernel module `i2c-dev` must be loaded.
Then rights must be set to access the device as a non-root user. For
simplicity's sake, I decided to give the rights to the `users` group. Here is
the relevant part of my `configuration.nix` for [NixOS](https://nixos.org)
users:

``` nix
  boot.kernelModules = [ "i2c-dev" ];
  services.udev.extraRules = "KERNEL==\"i2c-[0-9]*\", GROUP+=\"users\"";
```

Now to make sure `ddcutil` is working correctly, call the commands `detect`:

```
$ ddcutil detect
Invalid display
   I2C bus:  /dev/i2c-4
   EDID synopsis:
      Mfg id:               AUO
      Model:
      Product code:         22333
      Serial number:
      Binary serial number: 0 (0x00000000)
      Manufacture year:     2018,  Week: 0
   DDC communication failed
   This is an eDP laptop display. Laptop displays do not support DDC/CI.

Display 1
   I2C bus:  /dev/i2c-5
   EDID synopsis:
      Mfg id:               DEL
      Model:                DELL U3421WE
      Product code:         41349
      Serial number:        9C6P653
      Binary serial number: 810566732 (0x3050444c)
      Manufacture year:     2020,  Week: 52
   VCP version:         2.1
```

My Dell screen is correctly detected. Then the command `capabilities`:

```
$ ddcutil capabilities
...
  Feature: 60 (Input Source)
      Values:
         1b: Unrecognized value
         0f: DisplayPort-1
         11: HDMI-1
         12: HDMI-2
...
```

This last command will dump a bunch of things that can be set using `ddcutil`
and in particular the command to switch inputs.

Now if I want to switch the screen input to `DisplayPort-1`, all I have to do
is calling the command `setvcp` for the feature `Input Source` with the value
given by the `capabilities` command:

``` bash
$ ddcutil setvcp 60 0x0f
```

And if I want to switch it back to USB-C:

``` bash
$ ddcutil setvcp 60 0x1b
```

I only use these two inputs so I made a script to quickly toggle between the
two:

``` bash
#!/usr/bin/env bash

set -o nounset
set -o errexit

# Get current input
current=$(ddcutil getvcp 60 | sed -n "s/.*(sl=\(.*\))/\1/p")

# Get the other input
case $current in

    # Usb
    0x1b)
        output=0x0f
        ;;

    # Display port
    0x0f)
        output=0x1b
        ;;

    *)
        echo "Unknown input"
        exit 1
        ;;
esac

# Set new input
ddcutil setvcp 60 $output
```

I'd love to be able to switch only the USB hub as well but unfortunately it
doesn't seem possible with this screen. If you know a way to do it, [let me
know](/pages/contact.html).

