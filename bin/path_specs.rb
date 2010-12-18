require 'path'

describe "with_trailing_slash" do
  it "returnes string with ending slash unchanged" do
    "string_with_trailing_slash/".with_trailing_slash.should == "string_with_trailing_slash/"
  end

  it "appends single forward slash to string without ending slash" do
    "string_without_trailing_slash".with_trailing_slash.should == "string_without_trailing_slash/"
  end
end
