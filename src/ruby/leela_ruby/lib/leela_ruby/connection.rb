module Leela
  class Connection
    def initialize(endpoints, user, pass, timeout=6000)
      endpoints  = [endpoints].flatten

      @endpoints = endpoints
      @user      = user
      @pass      = pass
      @timeout   = timeout

      mendpoint  = FFI::MemoryPointer.new(:pointer, endpoints.size+1)

      endpoints.each_with_index do |endp, index|
        endpoint = Leela::Raw.leela_endpoint_load(endp)
        mendpoint[index].put_pointer(0, endpoint)
      end

      mendpoint[endpoints.size].put_pointer(0, nil)

      @context = Leela::Raw.leela_lql_context_init(mendpoint)
    end

    def execute(query)
      @cursor  = Leela::Raw.leela_lql_cursor_init(@context, @user, @pass, @timeout)
      Leela::Raw.leela_lql_cursor_execute(@cursor, query)

      while Leela::Raw.leela_lql_cursor_next(@cursor) == :leela_ok
        stat = Leela::Raw::LqlStat.new(Leela::Raw.leela_lql_fetch_stat(@cursor))

        i = 0
        attrs = []

        while i < stat[:size]
          attr = Leela::Raw::LqlAttrs.new(stat[:attrs]+Leela::Raw::LqlAttrs.size*i)
          attrs << [attr[:first], attr[:second]]

          i += 1
        end
      end

      attrs
    end

    def close
      Leela::Raw.leela_lql_cursor_close(@cursor)   if @cursor
      Leela::Raw.leela_lql_context_close(@context) if @context
    end
  end
end
