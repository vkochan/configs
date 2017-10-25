#!/bin/sh

DEV_GW=$(ip route | grep default | awk '{ print $5; }')
FW=iptables

echo 1 > /proc/sys/net/ipv4/ip_forward

$FW -t nat -A POSTROUTING -o $DEV_GW -j MASQUERADE
