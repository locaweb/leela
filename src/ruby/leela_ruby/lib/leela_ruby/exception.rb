module Leela
  class LeelaError < Exception
    attr_accessor :msg, :code

    def self.raise_from_leela_status(code)
      case code
      when :leela_badargs
        raise Leela::LeelaError.new "badargs: error using the library"
      when :leela_timeout
        raise Leela::TimeoutError.new "timeout: error reading from cursor"
      when :leela_error
        raise Leela::LeelaError.new "error: unexpected error has happened"
      end
    end

    def self.raise_from_fail(msg)
      code = msg[:code]
      msg  = msg[:message]
      if (code == 400)
        raise Leela::BadRequestError.new(msg, code)
      elsif (code == 403)
        raise Leela::ForbiddenError.new(msg, code)
      elsif (code == 404)
        raise Leela::NotFoundError.new(msg, code)
      elsif (code == 500)
        raise Leela::InternalServerError.new(msg, code)
      elsif (code > 400 && code < 500)
        raise Leela::UserError.new(msg, code)
      elsif (code > 500 && code < 600)
        raise Leela::ServerError.new(msg, code)
      else
        raise Leela::LeelaError.new(msg, code)
      end
    end

    def initialize msg, code = 0
      super msg
      @code = code
    end
  end

  class TimeoutError < LeelaError
    def initialize msg
      super msg, 0
    end
  end

  class UserError < LeelaError
    def initialize msg, code = 499
      super msg, code
    end
  end

  class ServerError < LeelaError
    def initialize msg, code = 599
      super msg, code
    end
  end

  class BadRequestError < UserError
    def initialize msg, code = 400
      super msg, code
    end
  end

  class ForbiddenError < UserError
    def initialize msg, code = 403
      super msg, code
    end
  end

  class NotFoundError < UserError
    def initialize msg, code = 404
      super msg, code
    end
  end

  class InternalServerError < ServerError
    def initialize msg, code = 500
      super msg, code
    end
  end

end
