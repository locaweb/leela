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

  it "raises an exception when an invalid machine was provided" do
    expect {
      Leela::Connection.new("tcp://warp9999.locaweb.com.br:4080", "pothix", "V1fR0sTo")
    }.to raise_error(Leela::LeelaError)
  end

  it "executes a query with Leela.open" do
    result = []

    Leela::Connection.open("tcp://warp0013.locaweb.com.br:4080", "pothix", "V1fR0sTo") do |conn|
      result = conn.execute("using (locaweb) stat;")
    end

    expect(result).to_not be_nil
    expect(result).to be_a(Array)
    expect(result.size > 0).to be_true
  end

  context "with a valid connection" do
    before do
      @conn = Leela::Connection.new("tcp://warp0013.locaweb.com.br:4080", "pothix", "V1fR0sTo")
    end

    after { @conn.close if @conn.connected? }

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

    context "when creating new paths" do
      before do
        result = @conn.execute("using (leelaruby) make (test), make (test2), make (test3);")
        guids = result.map{|r| r.last.first}

        @query = "using (leelaruby) make #{guids.first} -[test]> #{guids.last}, make #{guids[1]} -[test]> #{guids.last}, path #{guids.first};"
      end

      it "executes a simple query to return path structure" do
        result = @conn.execute(@query)

        expect(result).to_not be_nil
        expect(result).to be_a(Array)
        expect(result.size > 0).to be_true
      end

      it "executes a simple query to return path structure by using a block" do
        result = []

        @conn.execute(@query) do |row|
          result << row
        end

        expect(result).to_not be_nil
        expect(result).to be_a(Array)
        expect(result.size).to eql(2)
      end
    end
  end
end
