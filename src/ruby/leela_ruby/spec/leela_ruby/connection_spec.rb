require "spec_helper"

describe Leela::Connection do
  it "initializes a connection with just one endpoint" do
    expect {
      conn = Leela::Connection.new("tcp://warp0013.locaweb.com.br:4080", "pothix", "V1fR0sTo")
      conn.close
    }.to_not raise_error
  end

  it "initializes a connection with more than one endpoint" do
    endpoints = [
      "tcp://warp0013.locaweb.com.br:4080",
      "tcp://warp0014.locaweb.com.br:4080"
    ]

    expect {
      conn = Leela::Connection.new(endpoints, "pothix", "V1fR0sTo")
      conn.close
    }.to_not raise_error
  end

  context "with a valid connection" do
    before do
      @conn = Leela::Connection.new("tcp://warp0013.locaweb.com.br:4080", "pothix", "V1fR0sTo")
    end

    after { @conn.close }

    it "executes a simple query to return stat structure" do
      result = @conn.execute("using (locaweb) stat;")
      expect(result).to_not be_nil
      expect(result).to be_a(Array)
      expect(result.size > 0).to be_true
    end

    it "executes a simple query to return name structure" do
      result = @conn.execute("using (leelaruby) make (test), make (test2);")
      expect(result).to_not be_nil
      expect(result).to be_a(Array)
      expect(result.size > 0).to be_true
    end

    it "executes a simple query to return path structure" do
      result = @conn.execute("using (leelaruby) make (test), make (test2);")
      firstguid, lastguid = result.first.last.first, result.last.last.first
      result = @conn.execute("using (leelaruby) make #{firstguid} -[test]> #{lastguid}, path #{firstguid};")

      expect(result).to_not be_nil
      expect(result).to be_a(Array)
      expect(result.size > 0).to be_true
    end
  end
end
