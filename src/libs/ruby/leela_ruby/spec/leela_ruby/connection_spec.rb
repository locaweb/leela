require "spec_helper"

describe Leela::Connection do
  it "raises an exception when cant connect" do
    expect {
      Leela::Connection.open(["tcp://localhost:1080", "tcp://localhost:1081"], "pothix", "V1fR0sTo") do |cursor|
      end
    }.to raise_error(Leela::LeelaError)
  end

  context "connection without user and pass" do
    it "works passing user for connection" do
      expect {
        Leela::Connection.open("tcp://warp0013.locaweb.com.br:4080", "pothix", "V1fR0sTo") do |conn|
          conn.execute("using (locaweb) stat;")
        end
      }.to_not raise_error
    end

    it "works passing user for execute" do
      expect {
        Leela::Connection.open("tcp://warp0013.locaweb.com.br:4080") do |conn|
          conn.execute("using (locaweb) stat;", 60000, "pothix", "V1fR0sTo")
        end
      }.to_not raise_error
    end

    it "raises when no user and pass are given" do
      expect {
        Leela::Connection.open("tcp://warp0013.locaweb.com.br:4080") do |conn|
          conn.execute("anything")
        end
      }.to raise_error(Leela::LeelaError)
    end
  end
end
