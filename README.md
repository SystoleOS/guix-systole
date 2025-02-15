Systole OS GNU Guix Channel for Medical Image Computing
=======================================================

Welcome! This repository contains package recipes and extensions of the
[GNU Guix package manager](https://guix.gnu.org) tailored for medical image computing. It offers a collection of packages developed at Oslo University Hospital, NTNU, and other institutions.

The primary aim is to contribute most packages upstream. This repository serves as a staging area for work in progress or packages that are not yet ready for public use.

For inquiries or further information, please feel free to reach out directly via email to our contributors.

## Getting Started

To utilize this repository, you must have [Guix](https://guix.gnu.org) installed on your system. Refer to the
[installation instructions](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html).

## How Does It Work?

The package definitions in this repository _extend_ [the default packages provided by Guix](https://hpc.guix.info/browse). To integrate them with the `guix` command-line tools, create the `~/.config/guix/channels.scm` file using the following snippet to include the `systole` _channel_:

```scheme
(cons (channel
        (name 'systole)
        (url "https://github.com/SystoleOS/guix-systole.git"))
      %default-channels)
```

This configuration ensures that `guix pull` will fetch both Guix and the Systole channel.

## Package Installation

As this repository does not currently provide pre-built substitutes or binaries, users are encouraged to build packages from source. You can clone the Systole repository and compile the packages you need:

```bash
cd src
git clone https://github.com/SystoleOS/guix-systole.git
```

After cloning, you may build packages using:

```bash
guix build -L ~/src/systole <package-name>
```

When you are satisfied with your changes to the packages, you can push them to the repository for others to access.

## Guidelines for Modifications

New modules defining packages should be stored in `systole/packages`, belonging to the Guile `(systole packages)` namespace. When adding new packages, please place them in the relevant module or create a new module if necessary.

New modules defining services should be stored in `systole/services`, belonging to the Guile `(systole services)` namespace. When adding new services, please place them in the appropriate module or create a new module if needed.

Note that we are in the process of migrating older modules to this new namespace, so some packages may not adhere to these guidelines yet.

## More Information

The Guix manual provides valuable resources, including:

- Information on [getting started](https://guix.gnu.org/manual/en/html_node/Getting-Started.html),
- Details on the [channels mechanism](https://www.gnu.org/software/guix/manual/en/html_node/Channels.html) that allows integration of Systole packages with Guix,
- Guidance on the [GUIX_PACKAGE_PATH environment variable](https://guix.gnu.org/manual/en/html_node/Package-Modules.html#index-GUIX_005fPACKAGE_005fPATH), which enables you to extend the visible package set in Guix.

