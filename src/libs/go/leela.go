package leela

// #cgo CFLAGS: -I ../c/src
// #cgo LDFLAGS: -lleela -L/tmp/leela/src/libs/c/
// #include <leela/lql.h>
// #include <leela/status.h>
// #include <leela/endpoint.h>
import "C"
import (
	"regexp"
	"strings"
	"unsafe"
)

type EndpointType struct {
	protocol C.enum_leela_protocol
	host     string
	port     int
	path     string
}

func endpointLoad(endpoint string) (EndpointType, error) {
	initialized_endpoint := initializeEndpoint(endpoint)
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
	r, _ := regexp.Compile(`\/(\S+)\:`)
	found_str := r.FindString(endpoint)
	s := strings.Trim(found_str, "/")
	s = strings.Trim(s, ":")

	return s
}

func getEndpointPort(endpoint string) int {
	r, _ := regexp.Compile(`\:(\d+)\/`)
	found_str := r.FindString(endpoint)
	s := strings.Trim(found_str, ":")
	s = strings.Trim(s, "/")

	return int(s)
}

func getEndpointPath(endpoint string) string {
	r, _ := regexp.Compile(`\d\/(\S+)`)
	found_str := r.FindString(endpoint)
	s, _ := regexp.Compile(`\/(\S+)`)

	return s.FindString(found_str)
}
