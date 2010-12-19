class FileHierarchyPrinter
  def print_to_string(directory_entry)
    @root = directory_entry

    summary = create_summary

    listing = ".\n" + create_listing(@root)

    "#{listing}\n\n#{summary}\n"
  end

  private
  
  def create_listing(entry)
    listing = ""

    entries = entry.files.concat entry.directories

    entries.each_with_index do |e, i|
      listing += i == entries.size-1 ? "`" : "|"
      listing += "-- #{e.name}\n"
    end

    listing
  end

  def create_summary
    directory_summary = "#{@root.total_directory_count} directories"
    file_summary = "#{@root.total_file_count} files"

    if @root.total_directory_count == 1
      directory_summary = "1 directory"
    end
    
    if @root.total_file_count == 1
      file_summary = "1 file"
    end

    "#{directory_summary}, #{file_summary}"
  end
end
