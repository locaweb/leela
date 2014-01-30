module Leela
  class Connection
    DEFAULT_TIMEOUT = 6000

    attr_reader :user, :pass, :timeout, :context

    def initialize(endpoints, user, pass, timeout=DEFAULT_TIMEOUT)
      init_context(endpoints, user, pass, timeout=DEFAULT_TIMEOUT)
    end

    def self.open(endpoints, user, pass, timeout=DEFAULT_TIMEOUT, &block)
      raise "a block should be given to use open" unless block_given?

      conn = self.new(endpoints, user, pass, timeout=DEFAULT_TIMEOUT)
      block.call(conn)
      conn.close
    end

    def execute(query, &block)
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

    def connected?
      not @context.null?
    end

    private

    def init_context(endpoints, user, pass, timeout=DEFAULT_TIMEOUT, &block)
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

      raise Leela::LeelaError.new(code: 0) if @context.null?
    end
  end
end
