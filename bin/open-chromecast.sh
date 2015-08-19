#!/usr/bin/env sh
sudo iptables -A INPUT -p udp --sport 1900 -j ACCEPT
sudo iptables -I INPUT -p udp -m udp --dport 32768:61000 -j ACCEPT
