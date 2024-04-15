module M : sig module N : sig type t end end
         = struct module N = struct type t end end
module type S = sig type t end
module F (X : S) = struct type t = X.t end

module R = F(M.N)
