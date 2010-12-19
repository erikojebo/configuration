class FileHierarchyPrinter
  def print_to_string(directory_entry)

    summary = create_summary directory_entry

    listing = ".\n"
    entries = directory_entry.files.concat directory_entry.directories

    entries.each_with_index do |e, i|
      listing += i == entries.size-1 ? "`" : "|"
      listing += "-- #{e.name}\n"
    end

    "#{listing}\n\n#{summary}\n"
  end

  private
  
  def create_summary(directory_entry)
    directory_summary = "#{directory_entry.total_directory_count} directories"
    file_summary = "#{directory_entry.total_file_count} files"

    if directory_entry.total_directory_count == 1
      directory_summary = "1 directory"
    end
    
    if directory_entry.total_file_count == 1
      file_summary = "1 file"
    end

    "#{directory_summary}, #{file_summary}"
  end
end
