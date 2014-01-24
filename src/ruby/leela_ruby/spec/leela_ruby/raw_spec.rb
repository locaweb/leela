require "spec_helper"

describe Leela::Raw do
  it "calls the whole raw stack and execute a query" do
    endpoint = Leela::Raw.leela_endpoint_load("tcp://warp0013.locaweb.com.br:4080")
    mendpoint = FFI::MemoryPointer.new(:pointer, 2)
    mendpoint[0].put_pointer(0, endpoint)
    mendpoint[1].put_pointer(0, nil)

    context = Leela::Raw.leela_lql_context_init(mendpoint)
    cursor  = Leela::Raw.leela_lql_cursor_init(context, "pothix", "V1fR0sTo", 1000)

    Leela::Raw.leela_lql_cursor_execute(cursor, "using (locaweb) stat;")

    while Leela::Raw.leela_lql_cursor_next(cursor) == :leela_ok
      i, attrs  = 0, []
      stat      = Leela::Raw::LqlStat.new(Leela::Raw.leela_lql_fetch_stat(cursor))

      while i < stat[:size]
        attr = Leela::Raw::LqlAttrs.new(stat[:attrs]+Leela::Raw::LqlAttrs.size*i)
        attrs << [attr[:first], attr[:second]]

        i += 1
      end
    end

    Leela::Raw.leela_lql_cursor_close(cursor)
    Leela::Raw.leela_lql_context_close(context)
  end
end
