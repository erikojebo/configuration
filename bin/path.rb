class String
  def with_ending_slash
    return self if self =~ /\/$/
    self + "/"
  end
end
