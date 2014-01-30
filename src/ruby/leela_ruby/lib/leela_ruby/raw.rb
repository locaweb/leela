module Leela
  module Raw
    extend FFI::Library
    ffi_lib "leela"

    enum :status, [
      :leela_ok,       0,
      :leela_eof,      1,
      :leela_badargs,  2,
      :leela_timeout,  3,
      :leela_error,   -1
    ]

    enum :lql_row_type, [
      :lql_name_msg,
      :lql_path_msg,
      :lql_stat_msg,
      :lql_fail_msg
    ]

    attach_function :leela_endpoint_load, [:string], :pointer
    attach_function :leela_endpoint_free, [:pointer], :void

    attach_function :leela_lql_context_init, [:pointer], :pointer
    attach_function :leela_lql_context_close, [:pointer], :status

    attach_function :leela_lql_cursor_init, [:pointer, :string, :string, :int], :pointer
    attach_function :leela_lql_cursor_execute, [:pointer, :string], :status

    attach_function :leela_lql_fetch_type, [:pointer], :lql_row_type

    attach_function :leela_lql_fetch_name, [:pointer], :pointer
    attach_function :leela_lql_name_free,  [:pointer], :void

    attach_function :leela_lql_fetch_stat, [:pointer], :pointer
    attach_function :leela_lql_stat_free,  [:pointer], :void

    attach_function :leela_lql_fetch_path, [:pointer], :pointer
    attach_function :leela_lql_path_free,  [:pointer], :void

    attach_function :leela_lql_fetch_fail, [:pointer], :pointer
    attach_function :leela_lql_fail_free,  [:pointer], :void

    attach_function :leela_lql_cursor_next, [:pointer], :status
    attach_function :leela_lql_cursor_close, [:pointer], :status

    class LqlName < FFI::Struct
      layout :guid, :string,
             :user, :string,
             :tree, :string,
             :name, :string
    end

    class LqlPath < FFI::Struct
      layout :size,    :int,
             :entries, :pointer
    end

    class LqlStat < FFI::Struct
      layout :size,  :int,
             :attrs, :pointer
    end

    class LqlFail < FFI::Struct
      layout :code,    :uint32,
             :message, :string
    end

    class LqlAttrs < FFI::Struct
      layout :first,  :string,
             :second, :string
    end

    class LqlNAttr < FFI::Struct
      layout :size,  :int,
             :guid, :string,
             :names, :pointer
    end
  end
end
