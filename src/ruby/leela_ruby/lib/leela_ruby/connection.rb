module Leela
  class Connection
    DEFAULT_TIMEOUT = 6000

    attr_reader :context

    def initialize(endpoints, options={})
      init_context(endpoints, options[:user], options[:pass])
    end

    def self.open(endpoints, options={})
      conn = self.new(endpoints, options)

      if block_given?
        begin
          yield conn
        ensure
          conn.close
        end
      else
        conn
      end
    end

    def execute(query, options={}, &block)
      cursor = Leela::Cursor.new(self, options[:user], options[:pass], options[:timeout] || 0)
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

    def init_context(endpoints, user, pass, &block)
      ends      = [endpoints].flatten
      null      = false
      mendpoint = FFI::MemoryPointer.new(:pointer, endpoints.size+1)

      ends.each_with_index do |endp, index|
        endpoint = Leela::Raw.leela_endpoint_load(endp)
        null     = null || endpoint.null?
        mendpoint[index].put_pointer(0, endpoint)
      end

      mendpoint[endpoints.size].put_pointer(0, nil)
      raise Leela::LeelaError.new("error parsing endpoint") if null

      @context = Leela::Raw.leela_lql_context_init(mendpoint, user, pass, 30000)
      raise Leela::LeelaError.new("error connecting to leela cluster") if @context.null?

    ensure
      endpoints.size.times do |index|
        Leela::Raw.leela_endpoint_free(mendpoint[index].get_pointer(0))
      end
      mendpoint.free
    end
  end
end
