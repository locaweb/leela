module Leela
  class LeelaError < Exception
    attr_accessor :msg, :code
    def initialize msg = "LeelaError", code = 0
      super msg
      @code = code
    end
  end

  class BadargsError < LeelaError
    def initialize msg = "BadargsError", code = 0
      super msg, code
    end
  end

  class UserError < LeelaError
    def initialize msg = "UserError", code = 499
      super msg, code
    end
  end

  class ServerError < LeelaError
    def initialize msg = "ServerError", code = 599
      super msg, code
    end
  end

  class BadRequestError < UserError
    def initialize msg = "BadRequestError", code = 400
      super msg, code
    end
  end

  class ForbiddenError < UserError
    def initialize msg = "ForbiddenError", code = 403
      super msg, code
    end
  end

  class NotFoundError < UserError
    def initialize msg = "NotFoundError", code = 404
      super msg, code
    end
  end

  class InternalServerError < ServerError
    def initialize msg = "InternalServerError", code = 500
      super msg, code
    end
  end
end
