#!/usr/bin/ruby

transform = ARGV[0]

if ARGV[1]
  all_tests = [ARGV[1]]
end

#### NOTE FOR MAC USERS (2023.11.13):
####
#### It appears that just running any test can trigger a bug where,
#### when the Python test-runner's setup script calls `resource.setrlimit`,
#### it gives a ValueError, even though the parameters are within range.
####
#### To fix this: open CPython/lib/test/libregrtest/setup.py in the test
#### directory and comment out
####
####  resource.setrlimit(resource.RLIMIT_STACK, (newsoft, hard))

TIMELIMIT = "180s"

# Python has some way of specifying certain tests as too resource-intensive, and only runs some of the tests by default. I'm not going to figure
# out how to determine which to include; instead, I'll just copy its list
all_tests = 
%w[test_unicode_identifiers test_funcattrs test_dbm test_fileinput test_sunau test___all__ test___future__ test__locale test__opcode
    test__osx_support test_abc test_abstract_numbers test_aifc
    test_argparse test_array test_asdl_parser test_ast test_asyncgen
    test_asynchat test_asyncio test_asyncore test_atexit test_audioop
    test_augassign test_base64 test_baseexception test_bigaddrspace
    test_bigmem test_binascii test_binhex test_binop test_bisect
    test_bool test_buffer test_bufio test_builtin test_bytes test_bz2
    test_calendar test_call test_capi test_cgi test_cgitb
    test_charmapcodec test_class test_cmath test_cmd test_cmd_line
    test_cmd_line_script test_code test_code_module
    test_codeccallbacks test_codecencodings_cn test_codecencodings_hk
    test_codecencodings_iso2022 test_codecencodings_jp
    test_codecencodings_kr test_codecencodings_tw test_codecmaps_cn
    test_codecmaps_hk test_codecmaps_jp test_codecmaps_kr
    test_codecmaps_tw test_codecs test_codeop test_collections
    test_colorsys test_compare test_compile test_compileall
    test_complex test_concurrent_futures test_configparser
    test_contains test_contextlib test_copy test_copyreg
    test_coroutines test_cprofile test_crashers test_crypt test_csv
    test_ctypes test_curses test_datetime test_dbm_dumb test_dbm_gnu
    test_dbm_ndbm test_decimal test_decorators test_defaultdict
    test_deque test_descr test_descrtut test_devpoll test_dict
    test_dict_version test_dictcomps test_dictviews test_difflib
    test_dis test_distutils test_doctest test_doctest2 test_docxmlrpc
    test_dtrace test_dummy_thread test_dummy_threading test_dynamic
    test_dynamicclassattribute test_eintr test_email test_ensurepip
    test_enum test_enumerate test_eof test_epoll test_errno
    test_exception_hierarchy test_exception_variations test_exceptions
    test_extcall test_faulthandler test_fcntl test_file
    test_file_eintr test_filecmp test_fileio test_finalization
    test_float test_flufl test_fnmatch test_fork1 test_format
    test_fractions test_frame test_fstring test_ftplib test_functools
    test_future test_future3 test_future4 test_future5 test_gc
    test_gdb test_generator_stop test_generators test_genericpath
    test_genexps test_getargs2 test_getopt test_getpass test_gettext
    test_glob test_global test_grammar test_grp test_gzip test_hash
    test_hashlib test_heapq test_hmac test_html test_htmlparser
    test_http_cookiejar test_http_cookies test_httplib
    test_httpservers test_idle test_imaplib test_imghdr test_imp
    test_import test_importlib test_index test_inspect test_int
    test_int_literal test_io test_ioctl test_ipaddress test_isinstance
    test_iter test_iterlen test_itertools test_json test_keyword
    test_keywordonlyarg test_kqueue test_largefile test_lib2to3
    test_linecache test_list test_listcomps test_locale test_logging
    test_long test_longexp test_lzma test_macpath test_mailbox
    test_mailcap test_marshal test_math test_memoryio test_memoryview
    test_metaclass test_mimetypes test_minidom test_mmap test_module
    test_modulefinder test_msilib test_multibytecodec
    test_multiprocessing_fork test_multiprocessing_forkserver
    test_multiprocessing_main_handling test_multiprocessing_spawn
    test_netrc test_nis test_nntplib test_normalization test_ntpath
    test_numeric_tower test_opcodes test_openpty test_operator
    test_optparse test_ordered_dict test_os test_ossaudiodev
    test_osx_env test_parser test_pathlib test_pdb test_peepholer
    test_pickle test_pickletools test_pipes test_pkg test_pkgimport
    test_pkgutil test_platform test_plistlib test_poll test_popen
    test_poplib test_posix test_posixpath test_pow test_pprint
    test_print test_profile test_property test_pstats test_pty
    test_pulldom test_pwd test_py_compile test_pyclbr test_pydoc
    test_pyexpat test_queue test_quopri test_raise test_random
    test_range test_re test_readline test_regrtest test_reprlib
    test_resource test_richcmp test_rlcompleter test_robotparser
    test_runpy test_sax test_sched test_scope test_script_helper
    test_secrets test_select test_selectors test_set test_setcomps
    test_shelve test_shlex test_shutil test_signal test_site
    test_slice test_smtpd test_smtplib test_smtpnet test_sndhdr
    test_socket test_socketserver test_sort test_source_encoding
    test_spwd test_sqlite test_ssl test_startfile test_stat
    test_statistics test_strftime test_string test_string_literals
    test_stringprep test_strptime test_strtod test_struct
    test_structmembers test_structseq test_subclassinit
    test_subprocess test_sundry test_super test_support test_symbol
    test_symtable test_syntax test_sys test_sys_setprofile
    test_sys_settrace test_sysconfig test_syslog test_tarfile test_tcl
    test_telnetlib test_tempfile test_textwrap test_thread
    test_threaded_import test_threadedtempfile test_threading
    test_threading_local test_threadsignals test_time test_timeit
    test_timeout test_tix test_tk test_tokenize test_tools test_trace
    test_traceback test_tracemalloc test_ttk_guionly test_ttk_textonly
    test_tuple test_turtle test_typechecks test_types test_typing
    test_ucn test_unary test_unicode test_unicode_file
    test_unicode_file_functions test_unicodedata test_unittest
    test_univnewlines test_unpack test_unpack_ex test_urllib
    test_urllib2 test_urllib2_localnet test_urllib2net
    test_urllib_response test_urllibnet test_urlparse test_userdict
    test_userlist test_userstring test_utf8source test_uu test_uuid
    test_venv test_wait3 test_wait4 test_warnings test_wave
    test_weakref test_weakset test_webbrowser test_winconsoleio
    test_winreg test_winsound test_with test_wsgiref test_xdrlib
    test_xml_dom_minicompat test_xml_etree test_xml_etree_c
    test_xmlrpc test_xmlrpc_net test_yield_from test_zipapp
    test_zipfile test_zipfile64 test_zipimport test_zipimport_support
    test_zlib]

# Jakub 2025.06.12: Updated to match new Lua tests script, but I
#                   have not run it.
PYTHON_TESTS = ENV["PYTHON_TESTS"]
RUNPROG = `cabal list-bin multi`.strip
OUT_DIR = PYTHON_TESTS

# Build driver first
system("cabal build cubix-examples:multi")
if !$?.success?
  puts "cubix-examples:multi failed to build, aborting..."
  exit
end

num_tests = 0
num_passed = 0

num_loc = 0

all_tests.each do |testnam|
  
  testfil = PYTHON_TESTS + testnam + ".py"
  out_test = testnam + "_trans_" + transform 
  outfil = PYTHON_TESTS +  out_test + ".py"

  
  if transform == "count_loc"
    num_loc += `wc -l #{testfil}`.to_i
    next
  end

  num_tests += 1

  if transform != "id"
    system("#{RUNPROG} python id #{testfil} > /dev/null")
    if $? != 0 then
      puts "Failed round trip or parse: #{testfil}"
      next
    end
  end
  
  puts "Transforming from #{testfil} to #{outfil}"
  res = `gtimeout #{TIMELIMIT} #{RUNPROG} python #{transform} #{testfil}`
  res.force_encoding(Encoding::UTF_8)

  if $? != 0 then
    puts "Failed to transform: #{testfil}"
    next
  end

  File.open(outfil, "w") do |f|
    lines = res.split(/\n/)
    future_imports, oth_lines = lines.partition {|x| x.match(/^from __future__/)}
    
    f.puts(future_imports.join("\n"))
    f.puts("class TestCoverage:\n  coverage = [False] * 10000")
    f.puts(oth_lines.join("\n"))
  end

  system("gtimeout #{TIMELIMIT} python -m test #{out_test}")

  if $? == 0
    num_passed += 1
  else
    puts "Test failed: #{testfil}"
  end
end

if transform == "count_loc"
  puts num_loc
else
  puts "Tests: #{num_tests}"
  puts "Passed: #{num_passed}"
end
