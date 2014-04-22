require "leela_ruby"
require "metriks/time_tracker"

module Metriks::Reporter
  class Leela
    attr_reader :cluster,
                :user,
                :pass,
                :tree,
                :guid,
                :prefix,
                :timeout_in_ms

    def initialize (options)
      @user          = options[:user]
      @pass          = options[:pass]
      @tree          = options[:tree]
      @guid          = options[:guid]
      @prefix        = options[:prefix]
      @cluster       = options[:cluster]
      @registry      = options[:registry] || Metriks::Registry.default
      @interval      = options[:interval] || 60
      @onerror       = options[:on_error] || proc { |e| }
      @debug         = options[:debug] || proc { |m| }
      @timeout_in_ms = options[:timeout_in_ms] || 60000
    end

    def start
      agent    = [true, nil]
      agent[1] = Thread.new do
        _, interval = sleep_r(0)
        @debug.call "leela.start: %s" % [cluster.join(",")]
        ::Leela::Connection.open(@cluster, @user, @pass) do |ctx|
          guid = resolv ctx
          while agent[0] do
            more, interval = sleep_r interval
            next if more
            begin
              flush ctx, guid
            rescue => e
              @debug.call "leela.start[loop]: exception caught: %s" % e.to_s
              on_error.call e
            end
          end
          flush ctx, guid
        end
      end
      agent
    end

    def stop (agent)
      agent[0] = false
      agent[1].join
    end

    def flush (ctx, guid)
      @debug.call "[%s] leela.flush" % [guid]
      offset = 0
      buffer = write guid
      while (offset < buffer.size)
        send_metrics ctx, guid, buffer.slice(offset, 512)
        offset += 512
      end
    end

    def write (guid)
      buffer = []
      @registry.each do |name, metric|
        case metric
        when Metriks::Meter
          write_metric buffer, guid, name, metric, [
            :count, :one_minute_rate, :five_minute_rate,
            :fifteen_minute_rate, :mean_rate
          ]
        when Metriks::Counter
          write_metric buffer, guid, name, metric, [:count]
        when Metriks::Gauge
          write_metric buffer, guid, name, metric, [:value]
        when Metriks::UtilizationTimer
          write_metric buffer, guid, name, metric, [
            :count, :one_minute_rate, :five_minute_rate,
            :fifteen_minute_rate, :mean_rate,
            :min, :max, :mean, :stddev,
            :one_minute_utilization, :five_minute_utilization,
            :fifteen_minute_utilization, :mean_utilization
          ], [
            :median, :get_75th_percentile, :get_95th_percentile,
            :get_99th_percentile
          ]
        when Metriks::Timer
          write_metric buffer, guid, name, metric, [
            :count, :one_minute_rate, :five_minute_rate,
            :fifteen_minute_rate, :mean_rate,
            :min, :max, :mean, :stddev
          ], [
            :median, :get_75th_percentile, :get_95th_percentile,
            :get_99th_percentile
          ]
        when Metriks::Histogram
          write_metric buffer, guid, name, metric, [
            :count, :min, :max, :mean, :stddev
          ], [
            :median, :get_75th_percentile, :get_95th_percentile,
            :get_99th_percentile
          ]
        end
      end

      buffer
    end

    def write_metric (buffer, guid, name, metric, keys, snapshots = [])
      time  = Time.now.to_f
      name0 = unquote name
      if (@prefix && @prefix != "")
        name0 = "#{@prefix}/#{name0}/"
      else
        name0 = "#{name0}/"
      end

      keys.each do |key|
        name  = unquote key.to_s.gsub(/^get_/, "")
        value = metric.send key
        buffer << "attr put #{guid} \"#{name0}#{name}\" [#{time}] (double #{value})"
      end

      unless snapshots.empty?
        snapshot = metric.snapshot
        snapshots.each do |key|
          name  = unquote key.to_s.gsub(/^get_/, "")
          value = snapshot.send key
          buffer << "attr put #{guid} \"#{name0}#{name}\" [#{time}] (double #{value})"
        end
      end
    end

    def send_metrics (ctx, guid, buffer)
      @debug.call "[%s] leela.send_metrics: %d" % [guid, buffer.size]
      buffer = buffer.join "\n"
      ctx.execute("using (#{@tree}) #{buffer};")
    end

    def unquote (s)
      s.gsub(/\\"/, "").gsub(/"/, "")
    end

    def resolv (ctx)
      guid = ctx.execute("using (#{@tree}) guid (#{@guid});") {|row| row.last.last}
      @debug.call "leela.resolv: %s -> %s" % [@guid, guid]
      guid
    end

    def sleep_r (r)
      step = 0.100
      if (r < step)
        r = @interval + (@interval * rand * 0.25)
        [false, r]
      else
        sleep step
        [true, r - step]
      end
    end
  end
end
