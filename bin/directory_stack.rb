class DirectoryStack

  def initialize(root_directory = nil)
    @directories = []
    push root_directory if root_directory
  end

  def push(item)
    @directories.push item
  end

  def pop
    @directories.pop
  end

  def ends_with_slash(path)
    path =~ /\/$/
  end

  def depth
    @directories.length > 0 ? @directories.length - 1 : 0
  end

  def current
    return "" if (@directories.length == 0)

    path = nil

    @directories.each do |dir|
      if path
        path = File.join(path, dir)
      else
        path = dir
      end
    end
    
    if !ends_with_slash path
      path = path + "/"
    end

    path
  end
end
