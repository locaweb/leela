module Leela
  class Cursor

    def initialize(conn, user, pass, timeout)
      raise Leela::LeelaError.new("no context") unless conn.context
      raise Leela::LeelaError.new("no username given") unless user
      raise Leela::LeelaError.new("no secret given") unless pass
      @conn    = conn
      @user    = user
      @pass    = pass
      @timeout = timeout
    end

    def execute(query)
      Leela::Raw.with_cursor(@conn.context, @user, @pass, @timeout) do |cursor|
        rc = Leela::Raw.leela_lql_cursor_execute(cursor, query)
        Leela::LeelaError.raise_from_leela_status(rc) unless rc == :leela_ok

        answer = []

        while (rc = Leela::Raw.leela_lql_cursor_next(cursor)) == :leela_ok
          row = fetch cursor
          if block_given?
            ctrl = yield row
            break if ctrl == :break
          else
            answer << row
          end
        end
        Leela::LeelaError.raise_from_leela_status(rc) unless rc == :leela_eof

        answer unless block_given?
      end
    end

    private

    def type_of cursor
      Leela::Raw.leela_lql_fetch_type(cursor)
    end

    def fetch cursor
      case type_of(cursor)
      when :lql_name_msg
        begin
          msg = Leela::Raw::LqlName.new(Leela::Raw.leela_lql_fetch_name(cursor))
          [:name, [msg[:user], msg[:tree], msg[:name], msg[:guid]]]
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

      when :lql_nattr_msg
        begin
          msg = Leela::Raw::LqlNAttr.new(Leela::Raw.leela_lql_fetch_nattr(cursor))
          [:'n-attr', make_nattr_msg(msg[:size], msg[:guid], msg[:names])]
        ensure
          Leela::Raw::leela_lql_nattr_free(msg.pointer)
        end

      when :lql_kattr_msg
        begin
          msg = Leela::Raw::LqlKAttr.new(Leela::Raw.leela_lql_fetch_kattr(cursor))
          val = make_kattr_msg(msg[:guid], msg[:name], msg[:value])
          [:'k-attr', val]
        ensure
          Leela::Raw::leela_lql_kattr_free(msg.pointer)
        end

      when :lql_tattr_msg
        begin
          msg = Leela::Raw::LqlTAttr.new(Leela::Raw.leela_lql_fetch_tattr(cursor))
          val = make_tattr_msg(msg[:guid], msg[:name], msg[:series], msg[:size])
          [:'t-attr', val]
        ensure
          Leela::Raw::leela_lql_tattr_free(msg.pointer)
        end

      when :lql_fail_msg
        begin
          msg = Leela::Raw::LqlFail.new(Leela::Raw.leela_lql_fetch_fail(cursor))
          Leela::LeelaError.raise_from_fail msg
        ensure
          Leela::Raw::leela_lql_fail_free(msg.pointer)
        end
      end
    end

    def build_attrs_for(entries, size)
      attrs = []
      0.upto(size-1) do |i|
        attr = Leela::Raw::LqlAttrs.new(entries + Leela::Raw::LqlAttrs.size*i)
        attrs << [attr[:first].read_string, attr[:second].read_string]
      end
      attrs
    end

    def build_tattrs_for(entries, size)
      attrs = []
      0.upto(size-1) do |i|
        attr = Leela::Raw::LqlAttrs.new(entries + Leela::Raw::LqlAttrs.size*i)
        attrs << [attr[:first].read_double, make_attr_msg(attr[:second])]
      end
      attrs
    end

    def make_nattr_msg(size, guid, names)
      attr = names.get_array_of_string(0, size)
      [guid, attr]
    end

    def make_attr_msg(value)
      attr = Leela::Raw::LqlValueT.new(value)

      case attr[:vtype]
      when :lql_bool_type
        attr[:data][:v_bool]
      when :lql_text_type
        attr[:data][:v_str]
      when :lql_int32_type
        attr[:data][:v_i32]
      when :lql_int64_type
        attr[:data][:v_i64]
      when :lql_uint32_type
        attr[:data][:v_u32]
      when :lql_uint64_type
        attr[:data][:v_u64]
      when :lql_double_type
        attr[:data][:v_double]
      when :lql_nil_type
        nil
      end
    end

    def make_kattr_msg(guid, name, value)
      val = make_attr_msg(value)
      [guid, name, val]
    end

    def make_tattr_msg(guid, name, series, size)
      val = build_tattrs_for(series, size)
      [guid, name, val]
    end
  end
end
