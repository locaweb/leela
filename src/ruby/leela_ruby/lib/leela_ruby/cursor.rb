module Leela
  class Cursor
    attr_accessor :lql_cursor

    def initialize(conn)
      @conn = conn
    end

    def execute(query, &block)
      raise Leela::BadargsError.new("Username not given") unless @conn.user
      raise Leela::BadargsError.new("Password not given") unless @conn.pass

      lql_cursor = Leela::Raw.leela_lql_cursor_init(
        @conn.context, @conn.user, @conn.pass, @conn.timeout
      )

      ret_code = Leela::Raw.leela_lql_cursor_execute(lql_cursor, query)
      raise Leela::BadargsError.new(code = 0) unless ret_code == :leela_ok

      messages = []

      until (cnext = Leela::Raw.leela_lql_cursor_next(lql_cursor)) == :leela_eof
        raise Leela::BadargsError.new(code = 0) if cnext == :leela_badargs

        if block_given?
          last_value = block.call(fetch(lql_cursor))
        else
          messages << fetch(lql_cursor)
        end

        break if last_value == :break
      end

      Leela::Raw.leela_lql_cursor_close(lql_cursor)

      block_given? ? nil : messages
    end

    def fetch cursor
      case type_of(cursor)
      when :lql_name_msg
        begin
          msg = Leela::Raw::LqlName.new(Leela::Raw.leela_lql_fetch_name(cursor))
          [:name, [msg[:guid], msg[:user], msg[:tree], msg[:name]]]
        ensure
          Leela::Raw::leela_lql_name_free(msg.pointer)
        end

      when :lql_path_msg
        begin
          msg = Leela::Raw::LqlPath.new(Leela::Raw.leela_lql_fetch_path(cursor))
          [:path, build_attrs_for(msg[:entries], msg[:size]) ]
        ensure
          Leela::Raw::leela_lql_path_free(msg.pointer)
        end

      when :lql_stat_msg
        begin
          msg = Leela::Raw::LqlStat.new(Leela::Raw.leela_lql_fetch_stat(cursor))
          [:stat, build_attrs_for(msg[:attrs], msg[:size]) ]
        ensure
          Leela::Raw::leela_lql_stat_free(msg.pointer)
        end

      when :lql_fail_msg
        begin
          msg = Leela::Raw::LqlFail.new(Leela::Raw.leela_lql_fetch_fail(cursor))
          Leela::Raw.leela_lql_cursor_close(cursor)
          throw_exception msg[:message], msg[:code]
        ensure
          Leela::Raw::leela_lql_fail_free(msg.pointer)
        end
      end
    end

    def type_of cursor
      Leela::Raw.leela_lql_fetch_type(cursor)
    end

    private

    def build_attrs_for entries, size
      i, attrs = 0, []

      while i < size
        attr = Leela::Raw::LqlAttrs.new(entries + Leela::Raw::LqlAttrs.size*i)
        attrs << [attr[:first], attr[:second]]

        i += 1
      end

      attrs
    end

    def throw_exception msg, code
      raise Leela::BadRequestError.new(msg, code) if code == 400
      raise Leela::ForbiddenError.new(msg, code) if code == 403
      raise Leela::NotFoundError.new(msg, code) if code == 404
      raise Leela::InternalServerError.new(msg, code) if code == 500
      raise Leela::UserError.new(msg, code) if code > 400 && code < 500
      raise Leela::ServerError.new(msg, code) if code > 500 && code < 600
      raise Leela::LeelaError.new(msg, code)
    end
  end
end

