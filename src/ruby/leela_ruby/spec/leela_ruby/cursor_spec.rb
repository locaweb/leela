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

    it "executes a simple query to return stat structure" do
      # example: [[:stat, [["endpoint/blackbox", "tcp://10.30.124.167:50023"]] ]]

      result = @cursor.execute("using (locaweb) stat;")

      message = result.first

      message_type   = message.first
      message_values = message.last

      attrkey = message_values.first.first
      attrval = message_values.first.last

      expect(result.size > 0).to be_true

      expect(result).to_not be_nil
      expect(result).to be_a(Array)

      expect(message_type).to be_a(Symbol)
      expect(message).to be_a(Array)

      expect(attrkey).to be_a(String)
      expect(attrval).to be_a(String)
    end

    it "executes a simple query to return a name structure" do
      result = @cursor.execute("using (leelaruby) make (test), make (test2);")

      message = result.first

      message_type   = message.first
      message_values = message.last

      expect(result.size > 0).to be_true

      expect(result).to_not be_nil
      expect(result).to be_a(Array)

      expect(message_type).to be_a(Symbol)
      expect(message).to be_a(Array)

      (0..3).each do |index|
        expect(message_values[index]).to be_a(String)
      end

      # the first element should be the guid
      expect(message_values.first).to match(/[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}/)
    end

    it "executes a simple query to return a path structure" do
      # example: [
      #   [:name, ["023b2b80-8845-11e3-9b07-0f48fe0268e8", "pothix", "leelaruby", "test" ]],
      #   [:name, ["027e00e0-8845-11e3-b7a3-51d291422a0e", "pothix", "leelaruby", "test2"]]
      # ]
      result = @cursor.execute("using (leelaruby) make (test), make (test2);")
      firstguid, lastguid = result.first.last.first, result.last.last.first

      result = @cursor.execute("using (leelaruby) make #{firstguid} -[test]> #{lastguid}, path #{firstguid};")

      message = result.first

      message_type   = message.first
      message_values = message.last

      attrkey = message_values.first.first
      attrval = message_values.first.last

      expect(result.size > 0).to be_true

      expect(result).to_not be_nil
      expect(result).to be_a(Array)

      expect(message_type).to be_a(Symbol)
      expect(message).to be_a(Array)

      expect(attrkey).to be_a(String)
      expect(attrval).to be_a(String)
    end

    it "returns more than one path" do
      result = @cursor.execute("using (leelaruby) make (test), make (test2), make (test3);")
      guids = result.map{|r| r.last.first}

      query = "using (leelaruby) make #{guids.first} -[test]> #{guids.last}, make #{guids[1]} -[test]> #{guids.last}, path #{guids.first};"

      result = @cursor.execute(query)
      expect(result.size).to eql(2)
    end

    it "returns more than one path when using a block" do
      result = @cursor.execute("using (leelaruby) make (test), make (test2), make (test3);")

      guids  = result.map{|r| r.last.first}
      query  = "using (leelaruby) make #{guids.first} -[test]> #{guids.last}, make #{guids[1]} -[test]> #{guids.last}, path #{guids.first};"

      result = []

      @cursor.execute(query) do |row|
        result << row
      end

      expect(result.size).to eql(2)
    end

    it "executes a simple query to return a fail structure" do
      result = @cursor.execute("fail")

      message = result.first

      message_type   = message.first
      message_values = message.last

      attrkey = message_values.first
      attrval = message_values.last

      expect(result.size > 0).to be_true

      expect(result).to_not be_nil
      expect(result).to be_a(Array)

      expect(message_type).to be_a(Symbol)
      expect(message).to be_a(Array)

      expect(attrkey).to be_a(String)
      expect(attrval).to be_a(Integer)
    end
  end
end
