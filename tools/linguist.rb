require 'linguist'
project = Linguist::Repository.from_directory(".")
puts project.language.name  #=> "Ruby"
puts project.languages      #=> { "Ruby" => 0.98, "Shell" => 0.02 }
