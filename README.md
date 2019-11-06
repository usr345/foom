Force Operations Ordnance Maintenance
=====================================

A small story about trust and control.

Running
-------

Requires system OpenGL libraries installed and `freeglut3` package.

* **Ubuntu:** `sudo apt install freeglut3`
* **Others:** ??

Building / Running from source
------------------------------

* `stack build`
* `stack run`

Packaging
---------

#### Linux / AppImage

Requirements:

* hsinstall: `stack build hsinstall`
* linuxdeploy: https://github.com/linuxdeploy/linuxdeploy
  * appimage plugin: https://github.com/linuxdeploy/linuxdeploy-plugin-appimage
* butler: https://itch.io/docs/butler/installing.html

Package:

```shell
./appimage.sh
```

Ship to [Itch.io](https://itch.io):

```shell
./butle.sh
```
