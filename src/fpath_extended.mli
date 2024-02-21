module Fpath : sig
  include module type of struct
    include Fpath
    include Fpath0
  end
end
