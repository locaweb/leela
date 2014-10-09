package leela

// #cgo CFLAGS: -I ../c/src
// #cgo LDFLAGS: -lleela -L/tmp/leela/src/libs/c/
// #include <leela/lql.h>
// #include <leela/status.h>
// #include <leela/endpoint.h>
import "C"
import (
	"strings"
	"unsafe"
)

func endpointLoad(endpoint string) (EndpointType, error) {
	initialized_endpoint = initializeEndpoint(endpoint)
	initialized_endpoint{host: getEndpointHost(endpoint),
		port: getEndpointPort(endpoint), path: getEndpointPath(endpoint)}

	return initialized_endpoint
}

func initializeEndpoint(endpoint string) (EndpointType, error) {
	new_endpoint = EndpointType

	if strings.Contains(endpoint, "tcp://") == true {
		new_endpoint{protocol: C.PROTO_TCP}
	} else if strings.Contains(endpoint, "udp://") == true {
		new_endpoint{protocol: C.PROTO_UDP}
	} else {
		return errors.New("The endpoint must have a protocol specified.")
	}

	return new_endpoint
}

func getEndpointHost(endpoint string) string {
	raw = strings.Split(endpoint, ":")
	host = strings.Split(raw[1], "/")
	return host[1]
}

func getEndpointPort(endpoint string) string {
}

func getEndpointPath(endpoint string) string {
}
