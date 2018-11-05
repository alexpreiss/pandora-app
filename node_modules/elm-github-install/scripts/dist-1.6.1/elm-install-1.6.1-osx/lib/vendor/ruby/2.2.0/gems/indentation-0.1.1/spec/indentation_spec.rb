require File.dirname(__FILE__) + '/spec_helper.rb'

# Time to add your specs!
# http://rspec.info/
describe "Single-line String indentation" do
  
  it "should indent a string using 2 spaces as the default" do
    "test string".indent.should == "  test string"
  end
  
  it "should indent a given string the passed amount of indentation" do
    "test string".indent(3).should == "   test string"
    "test string".indent(0).should == "test string"
  end
  
  it "should indent a string using 1 tab as default, if tabs were specified but an amount was not" do
    "test string".indent(nil, "\t").should == "\ttest string"
  end
  
  it "should indent a string the passed amount of tabbed indentation" do
    "test string ".indent(3, "\t").should == "\t\t\ttest string "
    "test string ".indent(0, "\t").should == "test string "
  end
  
  it "should indent a string using the passed indentation character" do
    "test string".indent(1, "!").should == "!test string"
    "test string".indent(2, "--").should == "----test string"
  end
  
  it "should remove tab and space indentation if a negative amount is specified" do
    "   test string ".indent(-2).should == " test string "
    "   test string ".indent(-3).should == "test string "
    "   test string ".indent(-4).should == "test string "
    
    "\t  test string ".indent(-2).should == " test string "
    "\t  test string ".indent(-3).should == "test string "
    "\t  test string ".indent(-4).should == "test string "

    " \t test string ".indent(-2).should == " test string "
    " \t test string ".indent(-3).should == "test string "
    " \t test string ".indent(-4).should == "test string "
    
    "  \ttest string ".indent(-2).should == "\ttest string "
    "  \ttest string ".indent(-3).should == "test string "
    "  \ttest string ".indent(-4).should == "test string "
    
    "\t\t\ttest string ".indent(-2).should == "\ttest string "
    "\t\t\ttest string ".indent(-3).should == "test string "
    "\t\t\ttest string ".indent(-4).should == "test string "
  end
  
  it "should remove tab, space, and the specified indentation type if a negative amount and indentation character is specified" do
    "   \t\t\t---my string".indent(-8, '-').should == "-my string"
    "   \t\t\t---my string".indent(-8).should == "---my string"
    "   --- my string".indent(-4, "---").should == " my string"
  end
  
end

describe "Multi-line String indentation" do
  
  it "should indent each line within the given string" do
    "  this\nis\na\n test\n".indent.should == "    this\n  is\n  a\n   test\n  "
    "  this\nis\na\n test\n".indent(3).should == "     this\n   is\n   a\n    test\n   "
    "  this\nis\na\n test\n".indent(3, "\t").should == "\t\t\t  this\n\t\t\tis\n\t\t\ta\n\t\t\t test\n\t\t\t"
  end
  
  it "should de-indent each line within the given string" do
    "   \t\t\t This\n   is \n a\ntest\n ".indent(-1).should == "  \t\t\t This\n  is \na\ntest\n"
    "   \t\t\t This\n   is \n a\ntest\n ".indent(-2).should == " \t\t\t This\n is \na\ntest\n"
    "   \t\t\t This\n   is \n a\ntest\n ".indent(-3).should == "\t\t\t This\nis \na\ntest\n"
    "   \t\t\t This\n   is \n a\ntest\n ".indent(-6).should == " This\nis \na\ntest\n"
    "   \t\t\t This\n   is \n a\ntest\n ".indent(-7).should == "This\nis \na\ntest\n"
    "   \t\t\t This\n   is \n a\ntest\n ".indent(-8).should == "This\nis \na\ntest\n"
  end
end

describe "Single-line Array indentation" do
  before :all do
    @test_array = ['ali', '  bob', 'charlie']
  end
  
  it "should indent each element 2 spaces by default" do
    indented_array = @test_array.indent
    
    indented_array[0].should == '  ali'
    indented_array[1].should == '    bob'
    indented_array[2].should == '  charlie'
  end
  
  it "should indent each element using 1 tab as default, if tabs were specified but an amount was not" do
    indented_array = @test_array.indent(nil, "\t")
    
    indented_array[0].should == "\tali"
    indented_array[1].should == "\t  bob"
    indented_array[2].should == "\tcharlie"
  end
  
  it "should remove indentation if a negative amount was specified" do
    indented_array = @test_array.indent.indent(1, "\t").indent(3) # => ["   \t  ali", "   \t    bob", "   \t  charlie"]
    # Check that the array was correctly created
    indented_array[0].should == "   \t  ali"
    indented_array[1].should == "   \t    bob"
    indented_array[2].should == "   \t  charlie"
    
    # De-indent the array
    deindented_array = indented_array.indent(-7)
    deindented_array[0].should == "ali"
    deindented_array[1].should == " bob"
    deindented_array[2].should == "charlie"
  end
  
end

describe "Multi-line Array indentation" do
  before :all do
    @test_array = ["\n \t\n test\n", " One\n  Two\n   Three", "This\nis\na\ntest"]
  end
  
  it "should indent each line of each element 2 spaces by default" do
    indented_array = @test_array.indent
    
    indented_array[0].should == "  \n   \t\n   test\n  "
    indented_array[1].should == "   One\n    Two\n     Three"
    indented_array[2].should == "  This\n  is\n  a\n  test"
  end
  
  it "should indent each line of each element using 1 tab as default, if tabs were specified but an amount was not" do
    indented_array = @test_array.indent(nil, "\t")
    
    indented_array[0].should == "\t\n\t \t\n\t test\n\t"
    indented_array[1].should == "\t One\n\t  Two\n\t   Three"
    indented_array[2].should == "\tThis\n\tis\n\ta\n\ttest"
  end
  
  it "should remove indentation if a negative amount was specified" do
    deindented_array = @test_array.indent(-1)
    
    deindented_array[0].should == "\n\t\ntest\n"
    deindented_array[1].should == "One\n Two\n  Three"
    deindented_array[2].should == "This\nis\na\ntest"
  end
  
  it "should indent any contained arrays if they exist" do
    array = @test_array
    array << ["This", 'is', "a", "test"]
    indented_array = array.indent
    
    indented_array[3][0].should == "  This"
    indented_array[3][1].should == "  is"
    indented_array[3][2].should == "  a"
    indented_array[3][3].should == "  test"
  end
end

describe "Append Separator function" do
  before :each do
    @strings = ['bob', 'sue', 'joe']
    @arrays = [['one', 'two', 'three'], ['1', '2', '3'], ['uno', 'dos', 'tres']]
  end
  
  it "should append the given separator to the end of each string except the last if called on an array of strings" do
    separated_array = @strings.append_separator("!")

    separated_array[0].should == "bob!"
    separated_array[1].should == "sue!"
    separated_array[2].should == "joe"
  end
  
  it "should append the given separator to the end of each array except the last if called on an array of arrays" do
    separated_array = @arrays.append_separator("!")
    
    separated_array[0].last.should == "!"
    separated_array[1].last.should == "!"
    separated_array[2].last.should == "tres"
  end
  
  it "should not modify the current array unless the '!' version of the function is used - Strings" do
    @strings.append_separator("!")
    @strings[0].should == 'bob'
    @strings[1].should == 'sue'
    @strings[2].should == 'joe'
  end
  
  it "should not modify the current array unless the '!' version of the function is used - Arrays" do
    @arrays.append_separator("!")
    @arrays[0].last.should == "three"
    @arrays[1].last.should == "3"
    @arrays[2].last.should == "tres"
  end
  
  it "should apply changes to the current array if the '!' version of the function is used - Strings" do
    @strings.append_separator!("!")
    @strings[0].should == 'bob!'
    @strings[1].should == 'sue!'
    @strings[2].should == 'joe'
  end
  
  it "should apply changes to the current array if the '!' version of the function is used - Arrays" do
    @arrays.append_separator!("!")
    @arrays[0].last.should == "!"
    @arrays[1].last.should == "!"
    @arrays[2].last.should == "tres"
  end
end
  
describe "Find Least Indentation function" do
  it "should return the least amount of indentation on any line within an Array or String" do
    test_string_one = "   \tThis\n         is a\n \ttest"
    test_string_two = "     \t Test\n             Number\n                  Two"
    test_string_one.find_least_indentation.should == 2
    test_string_two.find_least_indentation.should == 7
    
    test_array = [test_string_one, test_string_two]
    test_array.find_least_indentation.should == 2
    test_array << "    Test\n again!"
    test_array.find_least_indentation.should == 1
    test_array << "No Indentation!"
    test_array.find_least_indentation.should == 0
    test_array << ""
    test_array.find_least_indentation.should == 0
  end
end

describe "Reset Indentation function" do
  it "should remove the least amount of indentation found in a String or Array" do
    test_string = <<-EOS.chomp.reset_indentation
      def method_name(arg)
        # Do stuff here
      end
      EOS
    test_string.should == "def method_name(arg)\n  # Do stuff here\nend"
    
    test_array = ["      def method_name(arg)", "        # Do stuff here", "      end"]
    
    test_array.reset_indentation.join("\n").should == test_string
  end
  
  it "should allow a modifier to be passed that changes how much indentation is added/removed" do
    
    string = <<-EOS
      def method_name(arg)
        # Do stuff here
      end
      EOS
    
    test_string = string.chomp.reset_indentation(2)
    test_string.should == "  def method_name(arg)\n    # Do stuff here\n  end"

    test_array = ["      def method_name(arg)", "        # Do stuff here", "      end"]

    test_array.reset_indentation(2).join("\n").should == test_string
    
    # Use of negative indentation works here too
    string.chomp.reset_indentation(-1).should == "def method_name(arg)\n # Do stuff here\nend"
    test_array.reset_indentation(-1).join("\n").should == string.chomp.reset_indentation(-1)
  end
    
  it "should not modify the current object unless the '!' version of the function is used - String" do
    string = "  This\n    is\n  a test"
    string.reset_indentation.should == "This\n  is\na test"
    string.should == "  This\n    is\n  a test"
  end
  
  it "should not modify the current object unless the '!' version of the function is used - Array" do
    array = ["  This", "    is", "  a test"]
    array.reset_indentation.should == ["This", "  is", "a test"]
    array.should == ["  This", "    is", "  a test"]
  end
  
  it "should apply changes to the current object if the '!' version of the function is used - String" do
    string = "  This\n    is\n  a test"
    string.reset_indentation!.should == "This\n  is\na test"
    string.should == "This\n  is\na test"
  end
  
  it "should apply changes to the current object if the '!' version of the function is used - Array" do
    array = ["  This", "    is", "  a test"]
    array.reset_indentation!.should == ["This", "  is", "a test"]
    array.should == ["This", "  is", "a test"]
  end
end

describe "English Join function" do
  it "should use the given conjunction to join the words with comma separation" do
    [].english_join.should == ''
    ['one'].english_join.should == 'one'
    ['one', 'two'].english_join.should == 'one and two'
    ['one', 'two', 'three'].english_join.should == 'one, two, and three'
  end
  
  it "should override the default conjunction with the passed conjunction" do
    ['one', 'two', 'three'].english_join('or').should == 'one, two, or three'
  end
  
  it "should override the default separator with the passed separator" do
    ['one', 'two', 'three'].english_join('or', ' ').should == 'one two or three'
  end
  
  it "should allow turning off the oxford comma" do
    ['one', 'two', 'three'].english_join('and', ', ', false).should == 'one, two and three'
  end
end