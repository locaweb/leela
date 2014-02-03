require "spec_helper"

describe Leela::Connection do
  it "raises an exception when an invalid machine was provided" do
    expect {
      Leela::Connection.new("tcp://warp9999.locaweb.com.br:4080", "pothix", "V1fR0sTo")
    }.to raise_error(Leela::LeelaError)
  end

  it "also run all the integration tests by using the spec/run scrips passing the endpoint as argument"
end
