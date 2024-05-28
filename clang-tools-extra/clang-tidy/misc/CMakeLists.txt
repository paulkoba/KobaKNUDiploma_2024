set(LLVM_LINK_COMPONENTS
  FrontendOpenMP
  Support
  )

setup_host_tool(clang-tidy-confusable-chars-gen CLANG_TIDY_CONFUSABLE_CHARS_GEN clang_tidy_confusable_chars_gen clang_tidy_confusable_chars_gen_target)

add_subdirectory(ConfusableTable)

include_directories(BEFORE "${CMAKE_CURRENT_SOURCE_DIR}/../../include-cleaner/include")

add_custom_command(
    OUTPUT Confusables.inc
    COMMAND ${clang_tidy_confusable_chars_gen} ${CMAKE_CURRENT_SOURCE_DIR}/ConfusableTable/confusables.txt ${CMAKE_CURRENT_BINARY_DIR}/Confusables.inc
    DEPENDS ${clang_tidy_confusable_chars_gen_target} ConfusableTable/confusables.txt)

add_custom_target(genconfusable DEPENDS Confusables.inc)

add_clang_library(clangTidyMiscModule
  ConstCorrectnessCheck.cpp
  CustomizableRulesCheck.cpp
  DefinitionsInHeadersCheck.cpp
  ConfusableIdentifierCheck.cpp
  HeaderIncludeCycleCheck.cpp
  IncludeCleanerCheck.cpp
  MiscTidyModule.cpp
  MisleadingBidirectional.cpp
  MisleadingIdentifier.cpp
  MisplacedConstCheck.cpp
  NewDeleteOverloadsCheck.cpp
  NoRecursionCheck.cpp
  NonCopyableObjects.cpp
  NonPrivateMemberVariablesInClassesCheck.cpp
  RedundantExpressionCheck.cpp
  StaticAssertCheck.cpp
  ThrowByValueCatchByReferenceCheck.cpp
  UnconventionalAssignOperatorCheck.cpp
  UniqueptrResetReleaseCheck.cpp
  UnusedAliasDeclsCheck.cpp
  UnusedParametersCheck.cpp
  UnusedUsingDeclsCheck.cpp
  UseAnonymousNamespaceCheck.cpp
  Yaml.cpp

  LINK_LIBS
  clangAnalysis
  clangTidy
  clangTidyUtils

  DEPENDS
  omp_gen
  genconfusable
  )

set_source_files_properties(Yaml.cpp PROPERTIES COMPILE_OPTIONS -fexceptions)
set_source_files_properties(CustomizableRulesCheck.cpp PROPERTIES COMPILE_OPTIONS -fexceptions)

clang_target_link_libraries(clangTidyMiscModule
  PRIVATE
  clangAnalysis
  clangAST
  clangASTMatchers
  clangBasic
  clangFormat
  clangLex
  clangSerialization
  clangTooling
  clangToolingInclusions
  clangToolingInclusionsStdlib
  )
target_link_libraries(clangTidyMiscModule
  PRIVATE
  clangIncludeCleaner
  )

include(FetchContent)

FetchContent_Declare(yaml-cpp
  GIT_REPOSITORY https://github.com/jbeder/yaml-cpp.git
  GIT_TAG yaml-cpp-0.7.0)  #tag of a release

FetchContent_GetProperties(yaml-cpp)

if(NOT yaml-cpp_POPULATED)
  message(STATUS "Fetching yaml-cpp...")
  FetchContent_Populate(yaml-cpp)
  add_subdirectory(${yaml-cpp_SOURCE_DIR} ${yaml-cpp_BINARY_DIR})
endif()
  
# Create an export set
install(TARGETS clangTidyMiscModule yaml-cpp
        EXPORT MyExportSet
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin)

# Export the export set
install(EXPORT MyExportSet
        DESTINATION share/cmake/MyProject)

# Write the export set
export(EXPORT MyExportSet
       FILE MyProjectTargets.cmake)

# Include the exported targets
include_directories(${CMAKE_CURRENT_BINARY_DIR}/share/cmake/MyProject)

# Link the libraries
target_link_libraries(clangTidyMiscModule PUBLIC yaml-cpp::yaml-cpp)