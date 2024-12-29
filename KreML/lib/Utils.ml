let internalfail msg = failwith msg
let unreachable = internalfail "Reached unreachable by assumption code"

