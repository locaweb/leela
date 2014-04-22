require "metriks"
require "./lib/metriks/reporter/leela.rb"

agent = Metriks::Reporter::Leela.new(:user    => "test",
                                     :pass    => "",
                                     :tree    => "dgvncsz0f",
                                     :guid    => "application::nephelae",
                                     :debug   => proc {|msg| p msg},
                                     :onerror => proc {|e| raise e},
                                     :cluster => ["tcp://warp0013.locaweb.com.br:4080"])
worker = agent.start

meter = Metriks.meter("foobar")
100.times do
  meter.mark
end

agent.stop worker
