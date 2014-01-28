module Leela
  class Badargs < Exception
    def message; "Wrong leela API use, possible bug on leela-ruby, please report!"; end
  end
end
