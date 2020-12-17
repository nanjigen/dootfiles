# vpngate.py

This script allows user to connect to the vpngate server easily. The user just needs to provide the desidered output country, and the script automatically chooses the best server. It provide the user the easy way to select the desired country.


This project is inspired by Andrea Lazzarotto. Thanks to him, I able to make this project successful.


# Usage

Run the script by typing command given below:

    ./vpngate.py
But before running the script make sure you make it executable. just right click on it and select properties menu there you will find  the option to make it executable.

After running the script wait for few seconds and let it download the requied file. Then it will present you with the option of selecting the country, select the appropiate country form the available option and press enter. 

Moreover, in this script you need not to type the name of the country just select the country by the option dropdown in the commandline. 

After running the script it will ask you for the password, enter it and wait for the succesfull connection.
After the succesfull connection it will show you the message 'INITALIZATION COMPLET'. Do not close the terminal window!!! just minimize it and that's all. You are connected to the vpn server. 
To terminate the program just open the terminal and type ctrl+c


# Requirements

OpenVPN needs to be installed.

The script should run on any Linux distribution with the Python3 [Requests](python-requests.org) module installed. The user running the script must be able to run `sudo` commands in order to start `openvpn`.
To install the required library just type the following command in the terminal

pip3 install bullet;

pip3 install requests;

pip3 install pyfiglet;

pip3 install base64;

pip3 install subprocess;