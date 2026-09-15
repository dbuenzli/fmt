let isatty oc =
  try Unix.(isatty (descr_of_out_channel oc)) with
  | Unix.Unix_error _ -> false
