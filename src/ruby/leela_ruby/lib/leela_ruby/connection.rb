module Leela
  class Connection
    DEFAULT_TIMEOUT = 6000

    attr_reader :user, :pass, :timeout, :context

    def initialize(endpoints, timeout=DEFAULT_TIMEOUT, user=nil, pass=nil)
      init_context(endpoints, user, pass, timeout)
    end

    def self.open(endpoints, timeout=DEFAULT_TIMEOUT, user, pass, &block)
      conn = self.new(endpoints, user, pass, timeout)

      if block_given?
        block.call(conn)
        conn.close
      else
        conn
      end
    end

    def execute(query, user=nil, pass=nil, &block)
      @user = user if user
      @pass = pass if pass

      cursor = Leela::Cursor.new(self)

      if block_given?
        cursor.execute(query, &block)
      else
        cursor.execute(query)
      end
    end

    def close
      Leela::Raw.leela_lql_context_close(@context) if @context
    end

    private

    def init_context(endpoints, user, pass, timeout, &block)
      ends      = [endpoints].flatten
      ends_null = []

      @user     = user
      @pass     = pass
      @timeout  = timeout

      mendpoint  = FFI::MemoryPointer.new(:pointer, endpoints.size+1)

      ends.each_with_index do |endp, index|
        endpoint = Leela::Raw.leela_endpoint_load(endp)
        mendpoint[index].put_pointer(0, endpoint)

        ends_null << (endpoint.null? || nil)
      end

      mendpoint[endpoints.size].put_pointer(0, nil)

      raise Leela::BadRequestError.new("invalid endpoint found") if ends_null.compact.any?

      @context = Leela::Raw.leela_lql_context_init(mendpoint)
      raise Leela::LeelaError.new() if @context.null?

    ensure
      endpoints.size.times do |index|
        Leela::Raw.leela_endpoint_free(mendpoint[index].get_pointer(0))
      end
      mendpoint.free
    end
  end
end
