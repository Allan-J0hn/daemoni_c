# Collect source files
file(GLOB SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/driver/*.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/lexer/*.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/parser/*.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/sema/*.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/codegen/*.cpp
)

# Create executable
add_executable(daemoni_c ${SOURCES})

# Include directories
target_include_directories(daemoni_c PRIVATE
    ${CMAKE_SOURCE_DIR}/include
)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
llvm_map_components_to_libnames(LLVM_LIBS core native mcjit)
target_link_libraries(daemoni_c PRIVATE ${LLVM_LIBS})

# Set output directory
set_target_properties(daemoni_c PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/build
)

# Install target
install(TARGETS daemoni_c DESTINATION bin)
