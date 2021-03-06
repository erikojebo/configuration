class FileHierarchyPrinter
  def print_to_string(directory_entry)
    @root = directory_entry
    @indentation = ""

    summary = create_summary
    listing = ".\n" + create_listing(@root)

    "#{listing}\n\n#{summary}\n"
  end

  private
  
  def create_listing(entry)
    listing = ""

    # Output files before directories
    entries = entry.files.concat entry.directories

    entries.each_with_index do |e, i|
      is_last_entry = i == entries.size - 1

      listing += @indentation
      listing += is_last_entry ? "`" : "|"
      listing += "-- #{e.name}\n"

      with_indentation(is_last_entry) do
        listing += create_listing(e) if e.directory?
      end
    end

    listing
  end

  def with_indentation(is_last_entry)
    additional_indentation = is_last_entry ? "    " : "|   "
    old_indentation = @indentation.dup
    @indentation << additional_indentation

    yield
    
    @indentation = old_indentation
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
