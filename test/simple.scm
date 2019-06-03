(loopback)
(wireless "wlan0")
(ether "eth0")
(bundle "net-wired"
	"eth0"
	"lo")
(bundle "net-wireless"
	"wlan0"
	"lo")
