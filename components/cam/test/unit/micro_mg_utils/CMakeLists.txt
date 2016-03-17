# Local pFUnit files.
set(pf_sources
  test_mg_processes.pf test_mg_size_utils.pf)

# Sources to test.
set(sources_needed
  micro_mg_utils.F90)
extract_sources("${sources_needed}" "${cam_sources}" test_sources)

# Do source preprocessing and add the executable.
create_pFUnit_test(micro_mg_utils micro_mg_utils_exe "${pf_sources}"
  "${test_sources}")

target_link_libraries(micro_mg_utils_exe csm_share)
