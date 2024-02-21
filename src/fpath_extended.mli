module Fpath : sig
  include module type of struct
    include Fpath
    include Fpath0
  end
end

module Classified_path = Path.Classified_path
module Absolute_path = Path.Absolute_path
module Relative_path = Path.Relative_path
module File_name = File_name
