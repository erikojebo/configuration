class StringContainsMatcher
  def initialize(expected_substring)
    @expected_substring = expected_substring
  end

  def matches?(actual)
    @actual = actual
    actual =~ /#{Regexp.escape(@expected_substring)}/m
  end

  def description
    "contains substring"
  end

  def failure_message
    "expected string to contain '#{@expected_substring}' but was '#{@actual}'"
  end

  def negative_failure_message
    "expected string to NOT contain '#{@expected_substring}' but was '#{@actual}'"
  end
end

def contain(string)
  StringContainsMatcher.new(string)
end
