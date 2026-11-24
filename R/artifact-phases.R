# Artifact orchestration phases
#
# Validate and plan the graph before serialization, and admit a manifest
# before reconstructing any of its modules.

artifact_plan_program <- function(program, registry = list(), trusted = FALSE) {
  module_graph_check_program(program)
  registry <- artifact_merge_registries(
    artifact_bound_registry(program),
    artifact_validate_registry(registry)
  )
  trusted <- artifact_validate_trusted(trusted)

  graph <- module_graph(program, boundaries = "cross", cycles = "record")
  if (any(graph$cycle)) {
    cycle <- graph[which(graph$cycle)[1], , drop = FALSE]
    cli::cli_abort(
      c(
        "Cyclic module graphs cannot be persisted",
        "x" = "Path {.val {cycle$path}} points back to {.val {cycle$canonical_path}}.",
        "i" = "Break the cycle before creating an artifact. Shared acyclic modules are supported."
      ),
      class = "dsprrr_artifact_cycle",
      call = rlang::caller_env()
    )
  }

  canonical <- graph[!graph$shared, , drop = FALSE]
  node_ids <- stats::setNames(canonical$path, canonical$id)

  list(
    registry = registry,
    trusted = trusted,
    graph = graph,
    canonical = canonical,
    node_ids = node_ids
  )
}

artifact_serialize_program_plan <- function(plan) {
  exclusions <- new.env(parent = emptyenv())
  exclusions$records <- list()
  nodes <- lapply(seq_len(nrow(plan$canonical)), function(i) {
    module <- plan$canonical$module[[i]]
    artifact_serialize_node(
      module = module,
      id = plan$node_ids[[plan$canonical$id[[i]]]],
      path = plan$canonical$path[[i]],
      node_ids = plan$node_ids,
      registry = plan$registry,
      trusted = plan$trusted,
      exclusions = exclusions
    )
  })
  names(nodes) <- vapply(nodes, `[[`, character(1), "id")

  edges <- lapply(seq_len(nrow(plan$graph))[-1], function(i) {
    parent_row <- match(plan$graph$parent_path[[i]], plan$graph$path)
    list(
      from = plan$node_ids[[plan$graph$id[[parent_row]]]],
      to = plan$node_ids[[plan$graph$id[[i]]]],
      path = plan$graph$path[[i]]
    )
  })

  list(
    format = "dsprrr-program",
    format_version = artifact_format_version(),
    root = plan$node_ids[[plan$canonical$id[[1]]]],
    graph = list(
      cycle_policy = "reject",
      shared_identity = "preserve",
      nodes = nodes,
      edges = edges
    ),
    metadata = artifact_metadata(),
    exclusions = exclusions$records
  )
}

artifact_finalize_program <- function(payload) {
  artifact <- structure(
    payload,
    class = c("dsprrr_program_artifact", "list")
  )
  artifact$integrity <- artifact_integrity(artifact)
  artifact_validate_manifest(artifact)
  artifact
}

artifact_admit_restore <- function(
  artifact,
  registry = list(),
  trusted = FALSE
) {
  registry <- artifact_validate_registry(registry)
  trusted <- artifact_validate_trusted(trusted)
  artifact_validate_manifest(artifact)

  list(
    artifact = artifact,
    registry = registry,
    trusted = trusted
  )
}

artifact_execute_restore <- function(admission) {
  cache <- new.env(parent = emptyenv(), hash = TRUE)
  program <- artifact_build_node(
    admission$artifact$root,
    admission$artifact,
    cache,
    active = character(),
    registry = admission$registry,
    trusted = admission$trusted
  )
  list(program = program, cache = cache)
}

artifact_finalize_restore <- function(execution, admission) {
  artifact <- admission$artifact
  program <- execution$program
  artifact_validate_restored_graph(program, artifact)
  modules <- mget(
    ls(execution$cache, all.names = TRUE),
    envir = execution$cache,
    inherits = FALSE
  )
  artifact_bind_registry(modules, artifact, admission$registry)
  artifact_bind_state_exclusions(modules, artifact)
  artifact_bind_restored_identity(program, artifact)
  program
}
