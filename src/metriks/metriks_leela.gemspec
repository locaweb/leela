# coding: utf-8

lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "metriks/reporter/leela/version.rb"

Gem::Specification.new do |spec|
  spec.name          = "metriks_leela"
  spec.version       = Metriks::Reporter::Leela::Version::VERSION
  spec.authors       = ["Diego Souza"]
  spec.email         = ["dgvncsz0f@gmail.com"]
  spec.description   = "Metriks Reporter - Leela"
  spec.summary       = spec.description
  spec.homepage      = "http://github.com/locaweb/leela"
  spec.license       = "APACHE-2"

  spec.files         = Dir["./**/*"].reject {|file| file =~ /\.git|pkg/}
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_dependency "metriks"
  spec.add_dependency "leela_ruby", ">= 5.2.0"

  spec.add_development_dependency "bundler", "~> 1.3"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"
end
