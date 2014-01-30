module Leela
  class LeelaError < Exception
    attr_accessor :msg, :code
    def initialize msg = "LeelaError", code
      super msg
      @code = code
    end
  end

  class BadargsError < LeelaError
    def initialize msg = "BadargsError", code
      super msg, code
    end
  end

  class UserError < LeelaError
    def initialize msg = "UserError", code
      super msg, code
    end
  end

  class ServerError < LeelaError
    def initialize msg = "ServerError", code
      super msg, code
    end
  end

  class BadRequestError < UserError
    def initialize msg = "BadRequestError", code
      super msg, code
    end
  end

  class ForbiddenError < UserError
    def initialize msg = "ForbiddenError", code
      super msg, code
    end
  end

  class NotFoundError < UserError
    def initialize msg = "NotFoundError", code
      super msg, code
    end
  end

  class InternalServerError < ServerError
    def initialize msg = "InternalServerError", code
      super msg, code
    end
  end
end
