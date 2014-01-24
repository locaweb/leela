# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'leela_ruby/version'

Gem::Specification.new do |spec|
  spec.name          = "leela_ruby"
  spec.version       = Leela::VERSION
  spec.authors       = ["PotHix"]
  spec.email         = ["pothix@pothix.com"]
  spec.description   = "Leela ruby client"
  spec.summary       = spec.description
  spec.homepage      = "http://github.com/locaweb/leela"
  spec.license       = "APACHE-2"

  spec.files         = Dir["./**/*"].reject {|file| file =~ /\.git|pkg/}
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_dependency "ffi"

  spec.add_development_dependency "bundler", "~> 1.3"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"
end
