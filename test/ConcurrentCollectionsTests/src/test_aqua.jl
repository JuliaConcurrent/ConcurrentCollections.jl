module TestAqua

import Aqua
import ConcurrentCollections

test() = Aqua.test_all(ConcurrentCollections; unbound_args = false)

end  # module
