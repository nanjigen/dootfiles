## Remove files
yay -Rs migemo-git --noconfirm
yay -Rs emacs-pdf-tools-git --noconfirm
yay -Rs emacs-exwm-git --noconfirm
sudo pacman -Rs cask --noconfirm
# yay -Rs emacs-git --noconfirm
sudo pacman -Rs emacs --noconfirm
yes | sudo rm -rf ~/.emacs.d

# sudo pacman -S emacs --noconfirm

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

yay -S migemo-git --noconfirm
# yay -S emacs-git

cd
# The following is from
# https://github.com/smile13241324/spacemacs-InstallerAndConfig/blob/master/spacemacsAutoInstall_manjaro.sh
spacemacsInstallationDir="${HOME}/.emacs.d"
[[ ! -d "${spacemacsInstallationDir}" ]] && git clone https://github.com/syl20bnr/spacemacs "${spacemacsInstallationDir}"
cd "${spacemacsInstallationDir}" || exit
git checkout origin/develop --track
git checkout develop
git pull
cd ..

yay -S emacs-pdf-tools-git --noconfirm
# yay -S emacs-exwm-git

# pacman -S migemo-git
sudo rm -rf ~/.config/emacs

emacs -nw --insecure
# cd "${DIR}" || exit
