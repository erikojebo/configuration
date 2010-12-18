require 'path'

describe "with_ending_slash" do
  it "returnes string with ending slash unchanged" do
    "string_with_ending_slash/".with_ending_slash.should == "string_with_ending_slash/"
  end

  it "appends single forward slash to string without ending slash" do
    "string_without_ending_slash".with_ending_slash.should == "string_without_ending_slash/"
  end
end
