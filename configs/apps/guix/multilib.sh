#! /bin/sh
# Add multilib support for Guix system

# may be something should be done in manifest or system config
# by doing the inheritance and appending the target
# system as it is done in wine.scm:
#
#    https://github.com/lfam/guix/blob/master/gnu/packages/wine.scm
guix package -i `guix build -s i686-linux gcc-toolchain`
