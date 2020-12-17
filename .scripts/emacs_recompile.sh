## Install Emacs source on Arch
# Clean last build
cd ~/Templates/src
# Discard old build
git reset --hard
# Delete old build contents
git clean -xdf
# Get latest update
git clone -b master git://git.sv.gnu.org/emacs.git
# Prep system
sudo pacman -Syyu --noconfirm
# sudo apt-get install build-essential automake texinfo libjpeg-dev libncurses5-dev
# sudo apt-get install libtiff5-dev libgif-dev libpng-dev libxpm-dev libgtk-3-dev libgnutls28-dev
cd emacs/
# read INSTALL.REPO
./autogen.sh 
# configure recommended I add --with-mailutils
./configure --with-mailutils  --with-modules --with-xwidgets
make
# check it's working
# src/emacs --version
# run it
src/emacs &
# install it globally
sudo make install
