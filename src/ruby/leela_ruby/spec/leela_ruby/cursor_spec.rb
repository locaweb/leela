require "spec_helper"

describe Leela::Cursor do
  context "with a valid connection" do
    it "executes a simple query to return a fail structure" do
      expect {
        Leela::Connection.open("tcp://warp0013.locaweb.com.br:4080", "pothix", "V1fR0sTo") do |conn|
          conn.execute("foobar")
        end
      }.to raise_error(Leela::UserError)
    end
  end
end
