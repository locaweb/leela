require "spec_helper"

describe Leela::Connection do
  it "raises an exception when an invalid machine was provided" do
    expect {
      Leela::Connection.new("tcp://warp9999.locaweb.com.br:4080", "pothix", "V1fR0sTo")
    }.to raise_error(Leela::LeelaError)
  end

  context "connection without user and pass" do
    it "works passing user for connection" do
      expect {
        conn = Leela::Connection.new("tcp://warp0013.locaweb.com.br:4080", 6000, "pothix", "V1fR0sTo")
        conn.execute("using (locaweb) stat;")
      }.to_not raise_error
    end

    it "works passing user for execute" do
      expect {
        conn = Leela::Connection.new("tcp://warp0013.locaweb.com.br:4080")
        conn.execute("using (locaweb) stat;", "pothix", "V1fR0sTo")
      }.to_not raise_error
    end

    it "raises when no user and pass are given" do
      expect {
        conn = Leela::Connection.new("tcp://warp0013.locaweb.com.br:4080")
        conn.execute("anything")
      }.to raise_error(Leela::BadargsError)
    end
  end

  it "also run all the integration tests by using the spec/run scrips passing the endpoint as argument"
end
