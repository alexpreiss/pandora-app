require 'spec_helper'

describe "Readme Rdoc examples" do
  rdoc_examples.each do |example_name, example_code|
    it "should pass the #{example_name} example from the README" do
      eval(example_code)
    end
  end
end