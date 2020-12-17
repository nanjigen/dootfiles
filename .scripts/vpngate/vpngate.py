#!/usr/bin/env python3

"""
following script will help you to connect the vpngate server (http://www.vpngate.net/en/) and the select the desired 
vpn server location . 

"""

__author__ = " Chetan Dhongade and Andrea Lazzarotto"
__copyright__ = "Copyright 2014+, Chetan Dhongade and Andrea Lazzarotto"
__license__ = "GPLv3"
__version__ = "1.0"
__maintainer__ = "Chetan Dhongade"
__email__ = "cumbolites@gmail.com"

#importing the required library

try:
    import requests, os, sys, tempfile, subprocess, base64, time, bullet
    from bullet import colors
    from pyfiglet import Figlet

except(ModuleNotFoundError):
    print("Please install required library")
    print("use the command : pip3 install <name of the library>")
    exit(1)


#I provided seperate function for each action in the code so it is easy to maintain 

#function for selecting the server it provide the drop down selection option 
def select_server(serv_list):
    cli = bullet.Bullet(
        prompt = "\nPlease select server location : ",
        choices = serv_list, 
        indent = 0,
        align = 5, 
        margin = 2,
        shift = 0,
        bullet = "",
        pad_right = 5)
    result = cli.launch()
    return result
#function  pick_server select the server according to there score 
def pick_server(supported):
    winner = sorted(supported, key=lambda s: float(s[2].replace(',','.')), reverse=True)[0]
    return winner
#after selecting the server we need to decode the base64 code in the python3 it return the 'utf-8' which rises
# the error to avoid it, I decode the utf-8 code, which can be written to the file  
def con_serv(winner):
    _, path = tempfile.mkstemp()
    f = open(path, 'w')
    f.write(base64.b64decode(winner[-1]).decode('utf-8'))
    f.write('\nscript-security 2\nup /etc/openvpn/update-resolv-conf\ndown /etc/openvpn/update-resolv-conf')
    f.close()
    x = subprocess.Popen(['sudo', 'openvpn', '--config', path])
    try:
        while True:
            time.sleep(6)
        # termination with Ctrl+C
    except:
        try:
            x.kill()
        except:
            pass
        while x.poll() != 0:
            time.sleep(1)
        print('\nVPN terminated')



if __name__=='__main__':
    #here is the simple welcome title to make the program look good 
    #you can remove it if not required.
    f = Figlet(font='slant')
    print(f.renderText('VPN-GATE'))
    try:
        print("welcome ....")
        #Here I am using the request module to download the server list and there respective certificate 
        vpn_data = requests.get('http://www.vpngate.net/api/iphone/').text.replace('\r','')
        #after that we need to split the information to make it easy to process, it makes list in list
        servers = [line.split(',') for line in vpn_data.split('\n')]
        servers = [s for s in servers[2:] if len(s) > 1]
    except:
        print('Cannot get VPN servers data')
        exit(1)  
    #collecting the name of all country server list in the list
    serv_list = []
    for i in servers:
        serv_list.append(i[5])
    #here I am removing the repeted name of the server 
    serv_list = list(set(serv_list))
    #here I make the seperate function to select the server 
    desired = select_server(serv_list)
    #after selecting the server, we need to determine how many server are ther in that lication 
    num_serv =  [s for s in servers if desired.lower() in s[5].lower()]
    #showing how many server support open vpn by observing the last entry in the list's[-1]'
    supported = [s for s in num_serv if len(s[-1]) > 0]
    print('{0}  of  {1}  servers support OpenVPN'.format(str(len(supported)), desired ))
    #selecting the best server 
    winner = pick_server(supported)
    print("\n\n The speed of the selected server is : {} MBps\n\n".format(str(float(winner[4]) / 10**6)))
    print("please wait connecting to the vpn server.......")
    con_serv(winner)
    
    print("Thank you for using VPNgate...")
    #exiting the program 