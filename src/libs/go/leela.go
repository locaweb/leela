package leela

import "errors"

// #cgo CFLAGS: -I ../c/src
// #cgo LDFLAGS: -lleela -L/tmp/leela/src/libs/c/
// #include <leela/lql.h>
// #include <leela/status.h>
// #include <leela/endpoint.h>
import "C"
import "unsafe"

//type Lql_context C.struct_lql_context_t
//type Lql_cursor C.struct_lql_cursor_t
//type leela_status C.enum_leela_status

/*type lql_row_type C.enum_lql_row_type*/
type lql_name C.struct_lql_name_t
type lql_path C.struct_lql_path_t
type lql_nattr C.struct_lql_nattr_t
type lql_tattr C.struct_lql_tattr_t
type lql_kattr C.struct_lql_kattr_t
type lql_fail C.struct_lql_fail_t

type LeelaRow struct {
	rowtype  string
	rowvalue string
}

type Leela_name struct {
	user string
	tree string
	kind string
	name string
	guid string
}

type Leela_nattr struct {
	guid  string
	names []string
}

type Leela_kattr struct {
	guid string
	name string
}

type Leela_tattr struct {
	guid   string
	name   string
	series [][]Leela_timeserie
}

type Leela_timeserie struct {
	timestamp float64
	entry     C.struct_lql_value_t
}

type LeelaStatus interface {
}

func EndpointLoad(endpoint string) *C.struct_leela_endpoint_t {
	end := C.CString(endpoint)
	defer C.free(unsafe.Pointer(end))
	return (*C.struct_leela_endpoint_t)(unsafe.Pointer(C.leela_endpoint_load(end)))
}

//func contextInit(endpoint C.struct_leela_endpoint_t, username string, secret string, timeout int) *C.struct_leela_lql_context_t {
//	usr := C.CString(username)
//	sec := C.CString(secret)
//	defer C.free(unsafe.Pointer(usr))
//	defer C.free(unsafe.Pointer(sec))
//	return (*C.struct_leela_lql_context_t)(unsafe.Pointer(C.leela_lql_context_init(endpoint, usr, sec, C.int(timeout))))
//}

func ContextClose(context *C.struct_lql_context_t) (int, error) {
	status := int(C.leela_lql_context_close(context))
	return checkStatus(status)
}

func checkStatus(status int) (int, error) {
	statuses := map[string]int{
		"LEELA_OK":      0,
		"LEELA_EOF":     1,
		"LEELA_BADARGS": 2,
		"LEELA_TIMEOUT": 3,
		"LEELA_ERROR":   -1,
	}

	for k, v := range statuses {
		if status == v {
			switch k {
			case "LEELA_BADARGS":
				return v, errors.New("Bad Arguments")
			case "LEELA_TIMEOUT":
				return v, errors.New("Timeout")
			case "LEELA_ERROR":
				return v, errors.New("Fatal Error")
			}

			return v, nil
		}
	}

	return -1, errors.New("Bad Status Code")
}

//
//func Leela_lql_cursor_init_default(ctx *C.struct_lql_context_t) *C.struct_lql_cursor_t {
//	return (*C.struct_lql_cursor_t)(unsafe.Pointer(C.leela_lql_cursor_init_default(ctx)))
//}

//
//func leela_lql_cursor_execute(cursor *C.struct_lql_cursor_t, query string) C.enum_leela_status {
//    qry := C.CString(query)
//    defer C.free(unsafe.Pointer(qry))
//    return C.leela_lql_cursor_execute(cursor, qry)
//}
//
//func leela_lql_cursor_close(cursor *C.struct_lql_cursor_t) C.enum_leela_status {
//    return C.leela_lql_cursor_close(cursor)
//}
//
//func (cursor Lql_cursor) Leela_fetch() {
//    row := leela_lql_fetch_type(cursor)
//    rowtype  := nil
//    rowvalue := nil
//    if row == nil {
//        return nil
//    }
//    switch row {
//        case row == C.LQL_NAME_MSG:
//            name := C.leela_lql_fetch_name(cursor.cursor)
//            if name != nil {
//                rowtype  := "name"
//                rowvalue := make_name_msg(name)
//                C.leela_lql_name_free(name)
//            }
//            break
//        case row == C.LQL_PATH_MSG:
//            path := C.leela_lql_fetch_path(cursor.cursor)
//            if path != nil {
//                rowtype := "path"
//                C.leela_lql_path_free(path)
//            }
//            break
//        case row == C.LQL_STAT_MSG:
//            stat := C.leela_lql_fetch_stat(cursor.cursor)
//            if stat != nil {
//                rowtype := "stat"
//                C.leela_lql_stat_free(stat)
//            }
//            break
//        case row == C.LQL_NATTR_MSG:
//            nattr := C.leela_lql_fetch_nattr(cursor.cursor)
//            if nattr != nil {
//                rowtype := "n-attr"
//                C.leela_lql_nattr_free(nattr)
//            }
//            break
//        case row == C.LQL_KATTR_MSG:
//            kattr := C.leela_lql_fetch_kattr(cursor.cursor)
//            if kattr != nil {
//                rowtype := "k-attr"
//                C.leela_lql_kattr_free(kattr)
//            }
//            break
//        case row == C.LQL_TATTR_MSG:
//            tattr := C.leela_lql_fetch_tattr(cursor.cursor)
//            if tattr != nil {
//                rowtype := "t-attr"
//                C.leela_lql_tattr_free(tattr)
//            }
//            break
//        case row == C.LQL_FAIL_MSG:
//            fail := C.leela_lql_fetch_fail(cursor.cursor)
//            if fail != nil {
//                rowtype := "fail"
//                C.leela_lql_fail_free(fail)
//            }
//            break
//        default:
//            panic("Unknown Leela Row Type")
//            break
//    }
//    return LeelaRow {
//            rowtype,
//            rowvalue,
//    }
//}
//
//func make_name_msg(name C.struct_lql_name_t) {
//    return Leela_name{
//                name.user,
//                name.tree,
//                name.kind,
//                name.name,
//                name.guid,
//            }
//}
//
//func make_nattr_msg(nattr C.struct_lql_nattr_t) {
//    names := make([]string, nattr.size)
//    guid  := C.GoString(nattr.guid)
//    for k := 0; k < nattr.size; k += 1 {
//        names[k] = C.GoString(nattr.names[k])
//    }
//    return Leela_nattr{
//            guid,
//            names,
//            }
//}
//
//func make_govalue(value C.struct_lql_value_t) {
//    vtype   := value.vtype
//    switch vtype {
//        case vtype == C.LQL_BOOL_TYPE:
//            if value.data.v_bool {
//                pyvalue = True
//            } else {
//                pyvalue = False
//            }
//            break
//        case vtype == C.LQL_TEXT_TYPE:
//            pyvalue := C.GoString(value.data.v_str)
//            break
//        case vtype == C.LQL_INT32_TYPE:
//            pyvalue := int32(value.data.v_i32)
//            break
//        case vtype == C.LQL_UINT32_TYPE:
//            pyvalue := uint32(value.data.v_u32)
//            break
//        case vtype == C.LQL_INT64_TYPE:
//            pyvalue := int64(value.data.v_i64)
//            break
//        case vtype == C.LQL_UINT64_TYPE:
//            pyvalue := uint64(value.data.v_u64)
//            break
//        case vtype == C.LQL_DOUBLE_TYPE:
//            pyvalue := float64(value.data.v_double)
//            break
//        case vtype == C.LQL_NIL_TYPE:
//            pyvalue := nil
//            break
//    }
//    return pyvalue
//}
//
//func make_kattr_msg(kattr C.struct_lql_kattr_t) {
//    return Leela_kattr{
//                kattr.guid,
//                kattr.name,
//                make_govalue(kattr.value),
//            }
//}
//
//func make_tattr_msg(tattr C.struct_lql_tattr_t) {
//    return Leela_tattr{
//            tattr.guid,
//            tattr.name,
//            make_timeseries(tattr.size, tattr.series),
//    }
//
//}
//
//func make_timeseries(size C.int, series C.struct_lql_tuple2_t) {
//    result := make([][]Leela_timeserie, size)
//    for k := 0;k < size; k += 1 {
//        result[k] = make([]Leela_timeserie, 2)
//        result[k][0] = series[k].fst
//        result[k][1] = make_govalue(series[k].snd)
//    }
//}
//
//func (cursor Lql_cursor) leela_lql_fetch_type() lql_row_type {
//    return C.leela_lql_fetch_type(cursor)
//}
//
//func (cursor Lql_cursor) leela_lql_fetch_fail() lql_fail {
//}
