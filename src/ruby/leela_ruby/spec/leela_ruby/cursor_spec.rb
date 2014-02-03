require "spec_helper"

describe Leela::Cursor do
  context "with a valid connection" do
    before do
      @conn   = Leela::Connection.new("tcp://warp0013.locaweb.com.br:4080", "pothix", "V1fR0sTo")
      @cursor = Leela::Cursor.new(@conn)
    end

    after do
      @conn.close
    end

    it "executes a simple query to return a fail structure" do
      expect {
      @cursor.execute("fail")
      }.to raise_error(Leela::BadRequestError)
    end
  end
end
