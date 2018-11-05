begin
  require 'rspec'
rescue LoadError
  require 'rubygems' unless ENV['NO_RUBYGEMS']
  gem 'rspec'
  require 'rspec'
end

$:.unshift(File.dirname(__FILE__) + '/../lib')
require 'indentation'

def rdoc_examples(examples_header = /^Examples:/i)
  readme_rdoc = File.join(File.dirname(__FILE__), '../README.rdoc')
  rdoc_content = File.read(readme_rdoc)
  example_content = rdoc_content.split(/^== /).select{|p| p =~ examples_header}.first
  raise "Couldn't find Examples header matching: #{examples_header}" unless example_content
  examples_content = example_content.split('=== ')
  examples = {}
  # Skipping first 'example' since it is "Examples:\n"
  examples_content[1..-1].each do |excon|
    lines = excon.split("\n")
    example_name = lines.shift.delete(':')
    example_code = []
    example_code << "$stdout = StringIO.new"
    lines.each do |line|
      l = line
      if l =~ / # => /
        code, check = line.split(/ # => /)
        # Could make this more sophisticated, but just going with ==
        l = code + ".should == " + check
      elsif l =~ /(.*) ?# :([^ ]*) => (.*)$/
        code = $~[1]
        var = $~[2].to_sym
        value = $~[3]
        case var
        when :stdout
          l = "#{code}\n $stdout.string.chomp.split(%!\n!).last.should == %!#{value}!"
        else
          raise "Unkown variable type: #{var}"
        end
      end
      example_code << l
    end
    # Debugging to show all of stdout during example
    #example_code << "stdoutput = $stdout.string"
    example_code << "$stdout = STDOUT"
    #example_code << "puts stdoutput"
    examples[example_name.to_sym] = example_code.join("\n")
  end
  examples
end

def time
  t = Time.now
  yield
  Time.now - t
end