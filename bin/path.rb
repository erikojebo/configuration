class String
  def with_trailing_slash
    return self if self =~ /\/$/
    self + "/"
  end
end
