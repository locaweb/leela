module Leela
  class Cursor
    attr_accessor :lql_cursor

    def initialize(conn)
      @conn       = conn
    end

    def execute(query)
      lql_cursor = Leela::Raw.leela_lql_cursor_init(
        @conn.context, @conn.user, @conn.pass, @conn.timeout
      )

      ret_code = Leela::Raw.leela_lql_cursor_execute(lql_cursor, query)
      raise Leela::Badargs unless ret_code == :leela_ok

      messages = []

      until (cnext = Leela::Raw.leela_lql_cursor_next(lql_cursor)) == :leela_eof
        raise Leela::Badargs if cnext == :leela_badargs

        messages << fetch(lql_cursor)
      end

      Leela::Raw.leela_lql_cursor_close(lql_cursor)

      messages
    end

    def fetch cursor
      case type_of(cursor)
      when :lql_name_msg
        msg = Leela::Raw::LqlName.new(Leela::Raw.leela_lql_fetch_name(cursor))
        [:name, [msg[:guid], msg[:user], msg[:tree], msg[:name]]]

      when :lql_path_msg
        msg = Leela::Raw::LqlPath.new(Leela::Raw.leela_lql_fetch_path(cursor))
        [:path, build_attrs_for(msg[:entries], msg[:size]) ]

      when :lql_stat_msg
        msg = Leela::Raw::LqlStat.new(Leela::Raw.leela_lql_fetch_stat(cursor))
        [:stat, build_attrs_for(msg[:attrs], msg[:size]) ]

      when :lql_fail_msg
        msg = Leela::Raw::LqlFail.new(Leela::Raw.leela_lql_fetch_fail(cursor))
        [:fail, [msg[:message], msg[:code]] ]
      end
    end

    def type_of cursor
      Leela::Raw.leela_lql_fetch_type(cursor)
    end

    private

    def build_attrs_for entries, size
      i = 0
      attrs = []

      while i < size
        attr = Leela::Raw::LqlAttrs.new(entries+Leela::Raw::LqlAttrs.size*i)
        attrs << [attr[:first], attr[:second]]

        i += 1
      end

      attrs
    end
  end
end

