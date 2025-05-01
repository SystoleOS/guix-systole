#!/bin/sh

# export GUIX_PROFILE=~/.config/guix/current
# . $GUIX_PROFILE/etc/profile

# Build the iso image
printf 'Building the image...\n\n'
image=$(guix time-machine -C './guix-systole/systoleos/channels.scm' \
--substitute-urls='https://ci.guix.gnu.org https://bordeaux.guix.gnu.org https://substitutes.nonguix.org' \
-- system image -t iso9660 './guix-systole/systoleos/systoleos.scm')

if [ $? -ne 0 ]; then
  printf 'Could not create image.\n' >&2
  exit 1
fi

release_tag=$(date + "%Y%m%d%H%M")
cp "${image}" "./systoleos-${release_tag}.iso"

printf 'Image was successfully built> %s\n' "${image}"
