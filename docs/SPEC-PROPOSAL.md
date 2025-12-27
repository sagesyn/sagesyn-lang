# Sage Agent Programming Language Specification v0.1

**Status:** DRAFT PROPOSAL
**Author:** SageSyn Team
**Date:** December 2025

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Lexical Structure](#2-lexical-structure)
3. [Types](#3-types)
4. [Declarations](#4-declarations)
   - 4.1 Agent Declaration
   - 4.2 Type Declaration
   - 4.3 Skill Declaration
   - 4.4 Tool Declaration
   - 4.5 Event Handlers
   - 4.6 Skill Declaration
   - **4.7 Context Management** *(NEW)*
   - **4.8 Tool Execution Model** *(NEW)*
5. [Expressions](#5-expressions)
6. [Statements](#6-statements)
7. [Modules](#7-modules)
8. [Error Handling](#8-error-handling)
9. [Protocols](#9-protocols)
   - **9.1 MCP (Model Context Protocol)** *(Enhanced)*
   - **9.2 A2A (Agent-to-Agent)** *(Enhanced)*
   - **9.3 AG-UI (Agent User Interface)** *(Enhanced)*
10. [Runtime Model](#10-runtime-model)
    - 10.1 Execution Model
    - 10.2 Async/Await
    - 10.3 State Semantics
    - 10.4 Context Object
    - **10.5 Agent Lifecycle** *(NEW)*
    - **10.6 Multi-Agent Patterns** *(NEW)*
    - **10.7 Memory and Persistence** *(NEW)*
11. [Standard Library](#11-standard-library)
12. [Compilation](#12-compilation)

**Appendices:**
- [Appendix A: Grammar (EBNF)](#appendix-a-grammar-ebnf)
- [Appendix B: Design Decisions](#appendix-b-design-decisions)
- **[Appendix C: Complete Agentic Examples](#appendix-c-complete-agentic-examples)** *(NEW)*
  - C.1 Research Agent (MCP, context, streaming)
  - C.2 Orchestrator Agent (A2A, parallel orchestration)
  - C.3 Interactive Assistant (AG-UI, forms, state sync)
  - C.4 Tool Pipeline Agent (caching, circuit breakers)

---

## 1. Introduction

### 1.1 Purpose

Sage Agent (`.sag`) is a **declarative, statically-typed, event-driven** domain-specific language for defining AI agents. It compiles to TypeScript, Python, or Go.

### 1.2 Design Goals

| Goal | Description |
|------|-------------|
| **Declarative** | Describe *what* an agent does, not *how* |
| **Type-Safe** | Catch errors at compile time |
| **Protocol-Native** | First-class MCP, A2A, AG-UI support |
| **Multi-Target** | Compile to TS, Python, Go |
| **Ergonomic** | Familiar syntax for web developers |

### 1.3 File Extension

Sage Agent source files use the `.sag` extension.

---

## 2. Lexical Structure

### 2.1 Character Set

UTF-8 encoded source files. Identifiers support ASCII alphanumeric plus underscore.

### 2.2 Comments

```sag
// Single-line comment
/// Documentation comment (attached to next declaration)
/* Multi-line
   comment */
```

### 2.3 Whitespace

Whitespace (space, tab, newline) is insignificant except as token separator.

### 2.4 Keywords

**Reserved keywords:**
```
agent    skill    type     fn       async    await
let      var      const    if       else     match
for      while    in       return   emit     on
tool     state    model    protocols true    false
null     import   export   from     as       pub
```

### 2.5 Identifiers

```
identifier = [a-zA-Z_][a-zA-Z0-9_]*
```

Identifiers are case-sensitive. `myAgent` and `MyAgent` are different.

### 2.6 Literals

**String:**
```sag
"hello world"           // Regular string
`Hello, ${name}!`       // Template string with interpolation
```

**Number:**
```sag
42                      // Integer
3.14                    // Float
1_000_000               // Underscores allowed (PROPOSAL)
```

**Boolean:**
```sag
true
false
```

**Null:**
```sag
null
```

---

## 3. Types

### 3.1 Primitive Types

| Type | Description | Example Values |
|------|-------------|----------------|
| `string` | UTF-8 text | `"hello"` |
| `number` | 64-bit float | `42`, `3.14` |
| `boolean` | True/false | `true`, `false` |
| `null` | Null value | `null` |
| `timestamp` | Date/time | (runtime value) |

### 3.2 Composite Types

**Array:**
```sag
array<string>           // Generic form
string[]                // Shorthand
```

**Record (Map):**
```sag
record<string, number>  // Key-value map
```

**Tuple:**
```sag
tuple<string, number, boolean>
```

**Optional:**
```sag
optional<string>        // Generic form
string?                 // Shorthand (equivalent)
```

### 3.3 Union Types

```sag
type Status = "pending" | "active" | "completed"
type Result = string | number | null
```

### 3.4 Custom Types

**Struct-like types:**
```sag
type User {
  id: string
  name: string
  email?: string        // Optional field
  age: number
  roles: array<string>
}
```

**Generic types with constraints:**
```sag
type ApiResponse<T> {
  data: T
  status: number
  error?: string
}

// Constrained generics
type Cache<K: Hashable, V: Serializable> {
  store: record<K, V>
  ttl: number
}

fn serialize<T: Serializable>(value: T) -> string {
  return json.stringify(value)
}
```

### 3.5 Built-in Constraints

| Constraint | Description |
|------------|-------------|
| `Serializable` | Can be converted to/from JSON |
| `Hashable` | Can be used as a key |
| `Comparable` | Supports comparison operators |
| `Equatable` | Supports equality checks |

### 3.6 Function Types

```sag
type Handler = (input: string) -> string
type AsyncHandler = async (input: string) -> Promise<string>
```

### 3.7 Type Compatibility

**Structural typing:** Types are compatible if their structure matches.

```sag
type A { x: number, y: number }
type B { x: number, y: number }
// A and B are compatible (same structure)
```

**Optional compatibility:**
- `T` is assignable to `T?`
- `null` is assignable to `T?`
- `T?` is NOT assignable to `T` (must narrow first)

### 3.8 Strict Null Safety

SageSyn enforces **strict null safety by default**. The compiler prevents null pointer errors at compile time.

**Rules:**
1. Non-optional types cannot be `null`
2. Must narrow optional types before use
3. Compiler tracks nullability through control flow

```sag
let name: string = "Alice"     // OK
let name: string = null        // ERROR: Cannot assign null to string

let email: string? = null      // OK: Optional can be null

// Must narrow before use
if email != null {
  send_email(email)            // OK: email is narrowed to string
}

// Or use optional chaining
let domain = email?.split("@")[1]   // Returns string?

// Or provide default
let safe = email ?? "default@example.com"
```

**Narrowing constructs:**
- `if x != null { ... }`
- `match x { null => ..., _ => ... }`
- `x ?? default` (null coalescing)
- `x?.method()` (optional chaining)

---

## 4. Declarations

### 4.1 Agent Declaration

```sag
agent AgentName {
  description: "Agent description"
  version: "1.0.0"

  model:
    provider: anthropic | openai | google | custom
    name: "model-name"
    context_window: 128000
    temperature: 0.7

  state {
    // State fields
  }

  protocols:
    // Protocol configuration

  tool toolName(params) -> ReturnType {
    // Tool definition
  }

  on event_name {
    // Event handler
  }
}
```

### 4.2 State Block

```sag
state {
  counter: number = 0           // With default (PROPOSAL)
  cache: record<string, Data>
  history: array<Message>
  lastQuery?: string            // Optional
}
```

**State semantics:**
- State is persistent across event handling
- State is mutable within handlers
- State is serializable (JSON-compatible types only)

### 4.3 Tool Declaration

**Inline tool:**
```sag
tool calculate(a: number, b: number) -> number {
  description: "Add two numbers"
  return a + b
}
```

**MCP-bound tool:**
```sag
tool search(query: string) -> SearchResults {
  description: "Search the web"
  mcp_server: brave_search
  mcp_tool: web_search
  params:
    q: query                    // Parameter mapping
}
```

### 4.4 Type Declaration

```sag
type Message {
  role: "user" | "assistant" | "system"
  content: string
  timestamp: timestamp
}
```

### 4.5 Function Declaration

```sag
fn greet(name: string) -> string {
  return `Hello, ${name}!`
}

async fn fetchData(url: string) -> Data {
  let response = await http.get(url)
  return response.data
}
```

### 4.6 Skill Declaration

```sag
skill WebSearch {
  description: "Web search capabilities"

  tool search(query: string) -> Results { ... }
  tool summarize(text: string) -> string { ... }
}
```

### 4.7 Context Management

Context management is fundamental to agent operation. SageSyn provides primitives for tracking token usage, managing conversation history, and handling context overflow.

#### 4.7.1 Token Counting API

```sag
// Read-only context properties
context.token_count -> number           // Current token usage
context.token_budget -> number          // Total budget (from context_window)
context.remaining_tokens -> number      // Budget minus current usage

// Token estimation
context.estimate_tokens(text: string) -> number
context.estimate_tokens(messages: array<Message>) -> number
```

#### 4.7.2 Context Budget Configuration

```sag
agent MyAgent {
  model:
    provider: anthropic
    name: "claude-sonnet-4"
    context_window: 200000

  context:
    budget:
      reserved_system: 3000             // Reserve for system prompt
      reserved_output: 8000             // Reserve for response generation
      warning_threshold: 0.75           // Trigger on context_warning at 75%
      critical_threshold: 0.95          // Trigger on context_overflow at 95%

    overflow_strategy: "summarize"      // "summarize" | "truncate" | "paginate" | "error"

    history:
      max_turns: 50                     // Maximum conversation turns to retain
      max_tokens: 100000                // Maximum tokens for history
      compression: "auto"               // "auto" | "manual" | "none"
      summary_model: "claude-haiku"     // Model for auto-summarization
}
```

#### 4.7.3 History Management

```sag
// History access
context.history -> array<Message>       // Full conversation history
context.history.length -> number        // Number of messages
context.history.token_count -> number   // Tokens used by history

// History operations
context.history.window(n: number) -> array<Message>     // Last n messages
context.history.slice(start: number, end: number) -> array<Message>
context.history.filter(predicate: (Message) -> boolean) -> array<Message>

// History modification (within handlers only)
context.history.truncate(n: number)     // Keep only last n messages
context.history.clear()                 // Clear all history
context.history.remove(index: number)   // Remove specific message

// Summarization
context.history.summarize() -> string   // Generate summary of conversation
context.history.summarize(options: SummarizeOptions) -> string

type SummarizeOptions {
  max_tokens?: number                   // Target summary length
  focus?: string                        // Focus area for summary
  include_tool_calls?: boolean          // Include tool call summaries
}

// Compression
context.history.compress(target_tokens: number) -> void
// Compresses history to fit within target_tokens while preserving key information

// Checkpointing
context.history.checkpoint() -> HistoryCheckpoint
context.history.restore(checkpoint: HistoryCheckpoint) -> void

type HistoryCheckpoint {
  id: string
  created_at: timestamp
  message_count: number
  token_count: number
}
```

#### 4.7.4 Message Builder

For token-aware message construction:

```sag
type MessageBuilder {
  fn new(budget: number) -> MessageBuilder
  fn add(role: string, content: string) -> MessageBuilder
  fn add_if_fits(role: string, content: string) -> boolean  // Returns false if won't fit
  fn remaining_tokens() -> number
  fn used_tokens() -> number
  fn build() -> array<Message>
}

// Usage
let builder = MessageBuilder.new(context.remaining_tokens)
builder.add("system", system_prompt)
builder.add("user", user_message)

// Add context if it fits
for doc in relevant_documents {
  if !builder.add_if_fits("system", `Context: ${doc.content}`) {
    break  // No more room
  }
}

let messages = builder.build()
```

#### 4.7.5 Context Overflow Hooks

```sag
on context_warning {
  // Triggered when usage exceeds warning_threshold (default: 75%)
  let usage_percent = context.token_count / context.token_budget * 100
  log.warn(`Context at ${usage_percent.to_fixed(1)}% capacity`)

  // Proactive compression
  if context.history.token_count > context.remaining_tokens * 0.5 {
    context.history.compress(context.remaining_tokens * 0.3)
  }
}

on context_overflow {
  // Triggered when usage exceeds critical_threshold (default: 95%)
  // Handle based on overflow_strategy

  match agent.context.overflow_strategy {
    "summarize" => {
      let summary = context.history.summarize()
      context.history.clear()
      context.history.add("system", `Previous conversation summary:\n${summary}`)
    }
    "truncate" => {
      context.history.truncate(10)  // Keep last 10 messages
    }
    "paginate" => {
      // Save current context and start fresh
      let checkpoint = context.history.checkpoint()
      await persistence.save(`context_${context.session_id}`, checkpoint)
      context.history.clear()
      context.history.add("system", "Conversation continued from previous context.")
    }
    "error" => {
      emit error({
        code: "CONTEXT_OVERFLOW",
        message: "Context window exceeded",
        recoverable: false
      })
    }
  }
}
```

#### 4.7.6 Context-Aware Patterns

```sag
// Check before expensive operations
fn safe_add_context(content: string) -> boolean {
  let estimated = context.estimate_tokens(content)
  if context.remaining_tokens < estimated + 1000 {  // Leave buffer
    return false
  }
  context.history.add("system", content)
  return true
}

// Streaming with context awareness
on user_message {
  // Reserve tokens for response
  let response_budget = 4000
  let available_for_context = context.remaining_tokens - response_budget

  // Fetch relevant context within budget
  let docs = await fetch_relevant_docs(message.content)
  let included_docs = []
  var token_sum = 0

  for doc in docs {
    let doc_tokens = context.estimate_tokens(doc.content)
    if token_sum + doc_tokens <= available_for_context {
      included_docs.push(doc)
      token_sum = token_sum + doc_tokens
    } else {
      break
    }
  }

  // Process with available context
  let response = await generate_response(message, included_docs)
  emit response(response)
}
```

### 4.8 Tool Execution Model

SageSyn provides comprehensive primitives for tool execution including parallel/sequential patterns, error handling, streaming, caching, and result metadata.

#### 4.8.1 Execution Patterns

```sag
// Sequential execution (default) - each awaits the previous
let data = await fetch_data(url)
let validated = await validate(data)
let result = await process(validated)

// Parallel execution - all run concurrently
let [users, posts, comments] = await tools.parallel([
  fetch_users(),
  fetch_posts(),
  fetch_comments()
])

// Race execution - first to complete wins
let result = await tools.race([
  slow_but_accurate_search(query),
  fast_approximate_search(query)
], timeout: 5000)

// Pipeline execution - chain of transformations
let result = await tools.pipeline([
  fetch_raw_data,
  parse_data,
  validate_data,
  enrich_data
], input: source_url)

// Conditional execution
let result = await tools.conditional({
  if: is_cached(key),
  then: get_from_cache(key),
  else: fetch_and_cache(key)
})

// Retry with backoff
let result = await tools.retry(
  () => unreliable_api_call(params),
  {
    max_attempts: 3,
    backoff: "exponential",     // "fixed" | "linear" | "exponential"
    initial_delay_ms: 100,
    max_delay_ms: 5000
  }
)
```

#### 4.8.2 Tool Result Type

```sag
// Every tool call returns ToolResult<T> with metadata
type ToolResult<T> {
  value: T                              // The actual result
  metadata: ToolMetadata
}

type ToolMetadata {
  tool_name: string                     // Name of the tool
  execution_time_ms: number             // How long the tool took
  token_cost: number                    // Tokens consumed (if applicable)
  cache_hit: boolean                    // Whether result came from cache
  retries: number                       // Number of retry attempts
  source: "local" | "mcp" | "a2a"       // Where tool executed
  trace_id: string                      // For debugging/observability
}

// Access metadata when needed
let result = await fetch_data(url)
log.debug(`Fetched in ${result.metadata.execution_time_ms}ms`)
process(result.value)

// Or use shorthand (auto-unwraps)
let data = await fetch_data(url)  // data is T, not ToolResult<T>
```

#### 4.8.3 Tool Configuration

```sag
tool fetch_api_data(endpoint: string, params: RequestParams) -> ApiResponse {
  description: "Fetch data from external API"
  mcp_server: api_server
  mcp_tool: fetch

  // Timeout configuration
  timeout: 30000                        // 30 second timeout

  // Retry configuration
  retry:
    max_attempts: 3
    backoff: exponential
    initial_delay_ms: 100
    max_delay_ms: 5000
    jitter: 0.2                         // +/- 20% randomization to prevent thundering herd
    retry_on: [NetworkError, TimeoutError, RateLimitError]

  // Circuit breaker (prevents cascading failures)
  circuit_breaker:
    failure_threshold: 5                // Open circuit after 5 consecutive failures
    reset_timeout_ms: 60000             // Try again after 1 minute
    half_open_max_requests: 3           // Max requests in half-open state

  // Result schema validation
  result_schema:
    validate: true
    on_invalid: "reject"                // "reject" | "coerce" | "warn"

  // Error handling
  on_error: {
    NetworkError => {
      log.warn("Network error, retrying...")
      retry()                           // Built-in retry function
    }
    TimeoutError => {
      return fallback_response()
    }
    RateLimitError(e) => {
      let wait_time = e.retry_after ?? 60000
      await time.sleep(wait_time)
      retry()
    }
    CircuitOpenError => {
      log.error("Circuit breaker open")
      return cached_response(endpoint)
    }
    _ => throw                          // Re-throw unknown errors
  }
}
```

#### 4.8.4 Tool Caching

```sag
tool expensive_computation(input: ComputeInput) -> ComputeResult {
  description: "Expensive cached computation"

  cache:
    enabled: true
    ttl: 3600000                        // 1 hour TTL
    key: (input) => crypto.hash(json.stringify(input), "sha256")
    storage: "memory"                   // "memory" | "persistent"
    max_entries: 1000                   // Maximum cache size

    // Conditional caching
    cache_if: (result) => result.status == "success"

    // Stale-while-revalidate pattern
    stale_ttl: 300000                   // Serve stale for 5 min while refreshing
    refresh_ahead: true                 // Proactively refresh before expiry
}

// Manual cache control
tools.cache.get("expensive_computation", key)
tools.cache.set("expensive_computation", key, value, ttl?)
tools.cache.invalidate("expensive_computation", key)
tools.cache.clear("expensive_computation")

// Cache statistics
let stats = tools.cache.stats("expensive_computation")
// { hits: number, misses: number, size: number, evictions: number }
```

#### 4.8.5 Streaming Tools

```sag
// Define a streaming tool
tool stream_search(query: string) -> Stream<SearchResult> {
  description: "Search with streaming results"
  streaming: true

  mcp_server: search_server
  mcp_tool: streaming_search

  stream_config:
    buffer_size: 10                     // Buffer n items before emitting
    timeout_per_chunk: 5000             // Timeout between chunks
    max_chunks: 1000                    // Maximum chunks to receive
}

// Consume streaming results
on user_message {
  let stream = stream_search(message.content)

  emit ag_ui.event(TextMessageStart { message_id: context.turn_id })

  for await result in stream {
    emit ag_ui.event(TextMessageContent {
      message_id: context.turn_id,
      delta: format_result(result)
    })
  }

  emit ag_ui.event(TextMessageEnd { message_id: context.turn_id })
}

// Stream type operations
type Stream<T> {
  // Iteration
  fn next() -> Promise<Option<T>>
  fn collect() -> Promise<array<T>>     // Collect all into array

  // Transformation
  fn map<U>(transform: (T) -> U) -> Stream<U>
  fn filter(predicate: (T) -> boolean) -> Stream<T>
  fn take(n: number) -> Stream<T>
  fn skip(n: number) -> Stream<T>

  // Aggregation
  fn reduce<U>(reducer: (U, T) -> U, initial: U) -> Promise<U>
  fn count() -> Promise<number>

  // Control
  fn buffer(size: number) -> Stream<array<T>>
  fn throttle(ms: number) -> Stream<T>
  fn timeout(ms: number) -> Stream<T>
}
```

#### 4.8.6 Tool Dependencies

```sag
// Declare tool dependencies
tool enriched_search(query: string) -> EnrichedResults {
  description: "Search and enrich results"

  // This tool depends on other tools
  depends_on: [search, fetch_metadata, summarize]

  // Dependency graph is validated at compile time
  // Runtime ensures dependencies are available before execution
}

// Tool groups for related operations
tool_group DataPipeline {
  tools: [fetch, transform, validate, store]

  // Shared configuration for the group
  timeout: 60000
  retry: { max_attempts: 2 }

  // Execution order constraints
  order: fetch -> transform -> validate -> store
}

// Execute the group
let result = await DataPipeline.execute(input)
```

#### 4.8.7 Error Propagation

```sag
// The ? operator for tool error propagation
fn process_data(url: string) -> Result<ProcessedData, Error> {
  let response = await fetch_data(url)?         // Returns early if error
  let parsed = json.try_parse(response.body)?
  let validated = validate_schema(parsed)?
  return Result.ok(process(validated))
}

// Tool-specific error types
type ToolError: Error {
  tool_name: string
  phase: "setup" | "execution" | "parsing"
  retries_attempted: number
  last_error: Error
}

type NetworkError: ToolError {
  status_code?: number
  url: string
}

type TimeoutError: ToolError {
  timeout_ms: number
  elapsed_ms: number
}

type RateLimitError: ToolError {
  retry_after?: number                  // Milliseconds until retry
  limit: number
  remaining: number
}

type CircuitOpenError: ToolError {
  failure_count: number
  last_failure_at: timestamp
  reset_at: timestamp
}
```

#### 4.8.8 Tool Observability

```sag
// Built-in tool metrics
tools.metrics.get("tool_name") -> ToolMetrics

type ToolMetrics {
  total_calls: number
  successful_calls: number
  failed_calls: number
  avg_latency_ms: number
  p50_latency_ms: number
  p95_latency_ms: number
  p99_latency_ms: number
  cache_hit_rate: number
  error_rate: number
  last_error?: Error
  last_success_at?: timestamp
}

// Tool execution hooks for custom observability
on before_tool_call {
  let { tool_name, args, trace_id } = tool_call
  log.debug(`[${trace_id}] Calling ${tool_name}`, args)
  telemetry.span_start(trace_id, tool_name)
}

on after_tool_call {
  let { tool_name, result, error, duration_ms, trace_id } = tool_call
  telemetry.span_end(trace_id, { duration_ms, error: error?.message })

  if error {
    metrics.increment(`tool.${tool_name}.errors`)
  } else {
    metrics.histogram(`tool.${tool_name}.latency`, duration_ms)
  }
}
```

---

## 5. Expressions

### 5.1 Operators

**Arithmetic:**
| Operator | Description |
|----------|-------------|
| `+` | Addition / String concatenation |
| `-` | Subtraction |
| `*` | Multiplication |
| `/` | Division |
| `%` | Modulo |
| `**` | Exponentiation |

**Comparison:**
| Operator | Description |
|----------|-------------|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `<=` | Less than or equal |
| `>` | Greater than |
| `>=` | Greater than or equal |

**Logical:**
| Operator | Description |
|----------|-------------|
| `&&` | Logical AND |
| `\|\|` | Logical OR |
| `!` | Logical NOT |

### 5.2 Precedence (highest to lowest)

1. `()` - Grouping
2. `.` `[]` `()` - Member access, index, call
3. `!` `-` (unary) - Unary operators
4. `**` - Exponentiation
5. `*` `/` `%` - Multiplicative
6. `+` `-` - Additive
7. `<` `<=` `>` `>=` - Comparison
8. `==` `!=` - Equality
9. `&&` - Logical AND
10. `||` - Logical OR
11. `=` - Assignment

### 5.3 Function Calls

```sag
greet("World")
object.method(arg1, arg2)
await asyncFunc()
```

### 5.4 Member Access

```sag
user.name                 // Dot notation
user["name"]             // Bracket notation
users[0]                 // Array index
```

### 5.5 Template Literals

```sag
let greeting = `Hello, ${user.name}! You have ${count} messages.`
```

### 5.6 Array and Record Literals

```sag
let numbers = [1, 2, 3, 4, 5]
let person = { name: "Alice", age: 30 }
```

### 5.7 Match Expression

**Basic matching:**
```sag
let result = match status {
  "pending" => "Waiting..."
  "active" => "In progress"
  "completed" => "Done!"
  _ => "Unknown"              // Wildcard (required for exhaustiveness)
}
```

**Pattern guards:**
```sag
let description = match age {
  n if n < 0 => "Invalid"
  n if n < 13 => "Child"
  n if n < 20 => "Teenager"
  n if n < 65 => "Adult"
  _ => "Senior"
}
```

**Destructuring in patterns:**
```sag
let message = match user {
  { name, age } if age >= 18 => `${name} is an adult`
  { name, age } => `${name} is ${age} years old`
}

let sum = match point {
  [x, y] => x + y
  [x, y, z] => x + y + z
  _ => 0
}
```

**Multiple patterns:**
```sag
let is_weekend = match day {
  "Saturday" | "Sunday" => true
  _ => false
}
```

**Optional matching:**
```sag
let display = match email {
  null => "No email"
  value => `Email: ${value}`
}
```

**Exhaustiveness checking:**
The compiler enforces exhaustive matching for known types:

```sag
type Status = "pending" | "active" | "completed"

// ERROR: Non-exhaustive match, missing "completed"
let msg = match status {
  "pending" => "..."
  "active" => "..."
}

// OK: All cases covered
let msg = match status {
  "pending" => "..."
  "active" => "..."
  "completed" => "..."
}
```

### 5.8 Spread Operator

**Array spread:**
```sag
let arr1 = [1, 2, 3]
let arr2 = [4, 5, 6]
let combined = [...arr1, ...arr2]        // [1, 2, 3, 4, 5, 6]
let withExtra = [0, ...arr1, 10]         // [0, 1, 2, 3, 10]
```

**Object spread:**
```sag
let defaults = { theme: "dark", lang: "en" }
let user = { name: "Alice", lang: "fr" }
let settings = { ...defaults, ...user }  // { theme: "dark", name: "Alice", lang: "fr" }
```

**In function calls:**
```sag
fn sum(a: number, b: number, c: number) -> number {
  return a + b + c
}

let nums = [1, 2, 3]
let result = sum(...nums)                // 6
```

**Rest in destructuring:**
```sag
let [first, ...rest] = [1, 2, 3, 4]      // first=1, rest=[2,3,4]
let { name, ...props } = user             // name="Alice", props={age:30}
```

### 5.9 Pipeline Operator

The pipeline operator `|>` chains function calls left-to-right:

```sag
// Without pipeline (nested calls)
let result = uppercase(trim(sanitize(input)))

// With pipeline (readable flow)
let result = input
  |> sanitize
  |> trim
  |> uppercase
```

**With arguments:**
```sag
// Placeholder _ for piped value
let result = data
  |> filter(_, x => x > 0)
  |> map(_, x => x * 2)
  |> reduce(_, 0, (a, b) => a + b)

// Or with partial application
let result = data
  |> filter(x => x > 0)
  |> map(x => x * 2)
  |> sum
```

**Async pipeline:**
```sag
let user = await userId
  |> fetchUser
  |> validateUser
  |> enrichWithProfile
```

**Method chaining vs pipeline:**
```sag
// Method chaining (when methods exist)
let result = array
  .filter(x => x > 0)
  .map(x => x * 2)
  .reduce((a, b) => a + b, 0)

// Pipeline (for standalone functions)
let result = array
  |> positiveOnly
  |> doubled
  |> sum
```

### 5.10 Arrow Functions

```sag
let double = (x: number) => x * 2
let greet = (name: string) => {
  let message = `Hello, ${name}!`
  return message
}
```

---

## 6. Statements

### 6.1 Variable Binding

```sag
let x = 10              // Immutable binding
var y = 20              // Mutable binding
const PI = 3.14159      // Compile-time constant
```

### 6.2 Destructuring

**Object destructuring:**
```sag
let user = { name: "Alice", age: 30, email: "alice@example.com" }

let { name, age } = user                    // Extract fields
let { name: userName, age } = user          // Rename
let { name, ...rest } = user                // Rest pattern
let { email = "default@example.com" } = user // Default value
```

**Array destructuring:**
```sag
let coords = [10, 20, 30]

let [x, y] = coords                         // Extract elements
let [first, ...rest] = coords               // Rest pattern
let [a, , c] = coords                       // Skip elements
let [x = 0, y = 0] = []                     // Default values
```

**In function parameters:**
```sag
fn greet({ name, age }: User) -> string {
  return `Hello ${name}, you are ${age}`
}

fn processPoint([x, y]: array<number>) -> number {
  return x + y
}
```

### 6.3 Default Parameters

```sag
fn greet(name: string = "World") -> string {
  return `Hello, ${name}!`
}

fn createUser(
  name: string,
  age: number = 0,
  roles: array<string> = ["user"]
) -> User {
  return { name, age, roles }
}

// Calling with defaults
greet()                    // "Hello, World!"
greet("Alice")             // "Hello, Alice!"
createUser("Bob")          // { name: "Bob", age: 0, roles: ["user"] }
```

### 6.4 Assignment

```sag
y = 30                  // Reassign mutable variable
state.counter = 5       // Assign to state field
user.name = "Bob"       // Assign to object property
```

### 6.5 Conditional

```sag
if condition {
  // then block
} else if other_condition {
  // else if block
} else {
  // else block
}
```

### 6.6 Loops

**For-in loop:**
```sag
for item in items {
  process(item)
}
```

**Range syntax:**
```sag
for i in 0..10 {           // 0 to 9 (exclusive end)
  print(i)
}

for i in 0..=10 {          // 0 to 10 (inclusive end)
  print(i)
}

for i in 10..0 {           // 10 to 1 (reverse, exclusive)
  print(i)
}

// With step (FUTURE)
for i in 0..100 step 10 {  // 0, 10, 20, ... 90
  print(i)
}
```

**Range expressions:**
```sag
let range = 0..5           // Range type
let nums = range.collect() // [0, 1, 2, 3, 4]
let contains = 3 in 0..10  // true
```

**While loop:**
```sag
while condition {
  // loop body
}
```

### 6.7 Return

```sag
return value
return                  // Return void/unit
```

### 6.8 Emit

```sag
emit response(data)
emit error("Something went wrong")
emit status("processing")
emit stream(chunk)
emit ag_ui.progress({ id: "task1", percent: 50 })
```

### 6.9 Event Handler

```sag
on user_message {
  let query = message.content
  let result = await search(query)
  emit response(result)
}

on tool_result {
  // Handle tool execution result
}

on agent_start {
  // Initialization logic
}

on agent_stop {
  // Cleanup logic
}
```

---

## 7. Modules

### 7.1 Import/Export (PROPOSAL)

**Export:**
```sag
pub type User { ... }           // Public type
pub fn greet(name: string) { }  // Public function
```

**Import:**
```sag
import { User, greet } from "./utils"
import * as utils from "./utils"
import type { Config } from "./config"   // Type-only import
```

### 7.2 Module Resolution

1. Relative paths: `./module`, `../module`
2. Package paths: `@sagesyn/stdlib`
3. File extension `.sag` is implicit

---

## 8. Error Handling

SageSyn provides multiple error handling mechanisms for different use cases.

### 8.1 Result Type

The `Result` type represents operations that may fail:

```sag
type Result<T, E> = { ok: true, value: T } | { ok: false, error: E }

fn divide(a: number, b: number) -> Result<number, string> {
  if b == 0 {
    return { ok: false, error: "Division by zero" }
  }
  return { ok: true, value: a / b }
}
```

**Working with Result:**
```sag
let result = divide(10, 0)

// Pattern matching (recommended)
match result {
  { ok: true, value } => log.info(`Result: ${value}`)
  { ok: false, error } => log.error(`Error: ${error}`)
}

// Conditional check
if result.ok {
  process(result.value)
} else {
  handle_error(result.error)
}
```

**Result Helpers:**
```sag
// Create results
let success = Result.ok(42)           // { ok: true, value: 42 }
let failure = Result.err("failed")    // { ok: false, error: "failed" }

// Unwrap (throws if error)
let value = result.unwrap()           // Returns value or throws
let value = result.unwrap_or(default) // Returns value or default
let value = result.expect("message")  // Returns value or throws with message

// Transform
let mapped = result.map(x => x * 2)   // Map success value
let mapped = result.map_err(e => ...)  // Map error value

// Chain operations
let final = result
  .and_then(x => validate(x))         // Chain if ok
  .or_else(e => recover(e))           // Chain if error
```

### 8.2 Option Type

The `Option` type represents values that may be absent (alternative to nullable):

```sag
type Option<T> = { some: true, value: T } | { some: false }

fn find_user(id: string) -> Option<User> {
  let user = db.get(id)
  if user != null {
    return { some: true, value: user }
  }
  return { some: false }
}
```

**Option Helpers:**
```sag
let some = Option.some(42)            // { some: true, value: 42 }
let none = Option.none()              // { some: false }

// Unwrap
let value = option.unwrap()           // Returns value or throws
let value = option.unwrap_or(default) // Returns value or default
let value = option.unwrap_or_else(fn) // Returns value or compute default

// Transform
let mapped = option.map(x => x * 2)   // Map if some
let filtered = option.filter(x => x > 0) // Filter to none if predicate fails

// Convert
let result = option.ok_or("error")    // Convert Option to Result
```

### 8.3 Try-Catch

For exception-based error handling:

```sag
try {
  let data = await fetchData(url)
  process(data)
} catch error {
  emit error(error.message)
} finally {
  cleanup()
}
```

**Typed catch blocks:**
```sag
try {
  await riskyOperation()
} catch error: NetworkError {
  // Handle network errors
  retry()
} catch error: ValidationError {
  // Handle validation errors
  log.warn(error.message)
} catch error {
  // Handle any other error
  emit error(error.message)
}
```

**Error types:**
```sag
type Error {
  message: string
  code?: string
  cause?: Error
  stack?: string
}

type NetworkError: Error {
  status_code: number
  url: string
}

type ValidationError: Error {
  field: string
  constraint: string
}
```

### 8.4 Error Propagation

**The `?` operator** (Result/Option early return):
```sag
fn process_data(url: string) -> Result<Data, Error> {
  let response = await http.get(url)?     // Returns early if error
  let parsed = json.try_parse(response.text())?
  let validated = validate(parsed)?
  return Result.ok(validated)
}
```

**The `throw` keyword:**
```sag
fn require_admin(user: User) {
  if !user.is_admin {
    throw Error { message: "Admin required", code: "FORBIDDEN" }
  }
}
```

### 8.5 Error Emission

Emit errors to the agent framework:

```sag
// Simple error message
emit error("User not found")

// Structured error
emit error({
  code: "NOT_FOUND",
  message: "User not found",
  details: { user_id: id }
})

// Error with recovery suggestion
emit error({
  code: "RATE_LIMITED",
  message: "Too many requests",
  retry_after: 60,
  recoverable: true
})
```

### 8.6 Assertions

Runtime assertions for debugging and invariants:

```sag
// Assert condition (throws if false)
assert(user != null, "User must exist")
assert(count >= 0, "Count cannot be negative")

// Debug-only assertions (removed in production)
debug_assert(expensive_check(), "Invariant violated")
```

### 8.7 Error Best Practices

1. **Prefer Result over try-catch** for expected errors
2. **Use try-catch** for unexpected/exceptional errors
3. **Use Option** when absence is normal (not an error)
4. **Always handle errors** - the compiler warns on unhandled Results
5. **Provide context** in error messages
6. **Use typed errors** for different error categories

---

## 9. Protocols

SageSyn provides first-class support for three agent protocols: MCP (Model Context Protocol), A2A (Agent-to-Agent), and AG-UI (Agent-User Interface).

### 9.1 MCP (Model Context Protocol)

MCP enables agents to connect to external tool servers. SageSyn provides comprehensive MCP support including server lifecycle management, resources, prompts, and sampling.

#### 9.1.1 Server Declaration

```sag
protocols:
  mcp:
    servers:
      // Local process server (stdio transport)
      - name: filesystem
        transport: stdio
        command: "npx"
        args: ["-y", "@modelcontextprotocol/server-filesystem"]
        env:
          ROOT_DIR: "${env.WORKSPACE_DIR}"

        // Lifecycle management
        lifecycle:
          startup_timeout_ms: 10000     // Max time to start
          health_check_interval_ms: 30000
          restart_on_failure: true
          max_restarts: 3
          graceful_shutdown_ms: 5000

        // Required capabilities (validated at startup)
        requires:
          tools: ["read_file", "write_file", "list_directory"]
          resources: ["file://"]

      // Remote server (SSE transport)
      - name: search_api
        transport: sse
        url: "https://mcp.example.com/sse"
        headers:
          Authorization: "Bearer ${env.API_KEY}"

        // Connection settings
        connection:
          timeout_ms: 30000
          reconnect: true
          max_reconnect_attempts: 5
          reconnect_delay_ms: 1000

      // WebSocket transport
      - name: realtime_server
        transport: websocket
        url: "wss://mcp.example.com/ws"

        // Heartbeat configuration
        heartbeat:
          interval_ms: 30000
          timeout_ms: 10000
```

#### 9.1.2 Transport Types

| Transport | Description | Use Case |
|-----------|-------------|----------|
| `stdio` | Local process via stdin/stdout | CLI tools, local utilities |
| `http` | HTTP request/response | Stateless API servers |
| `sse` | Server-sent events | Streaming, long-running operations |
| `websocket` | Bidirectional WebSocket | Real-time, high-frequency |

#### 9.1.3 Tool Binding

```sag
// Simple tool binding
tool read_file(path: string) -> string {
  description: "Read contents of a file"
  mcp_server: filesystem
  mcp_tool: read_file
}

// Tool with parameter mapping
tool search(query: string, limit: number = 10) -> SearchResults {
  description: "Search the web"
  mcp_server: search_api
  mcp_tool: web_search

  // Map local params to MCP tool params
  params:
    q: query                            // Rename parameter
    max_results: limit
    safe_search: true                   // Add constant parameter
}

// Tool with result transformation
tool fetch_data(url: string) -> Data {
  description: "Fetch and parse data"
  mcp_server: http_client
  mcp_tool: fetch

  // Transform MCP result
  transform: (result) => {
    return json.parse(result.content[0].text)
  }
}
```

#### 9.1.4 Resource Handling

MCP resources provide read access to server-managed content:

```sag
// List available resources
let resources = await mcp.list_resources("filesystem")
// Returns: array<McpResource>

type McpResource {
  uri: string                           // e.g., "file:///path/to/file.txt"
  name: string                          // Human-readable name
  description?: string
  mime_type?: string
}

// Read a resource
let content = await mcp.read_resource("filesystem", "file:///config.json")
// Returns: McpResourceContent

type McpResourceContent {
  uri: string
  mime_type?: string
  text?: string                         // Text content
  blob?: array<number>                  // Binary content (base64 decoded)
}

// Resource templates (parameterized URIs)
let template = await mcp.get_resource_template("database", "user")
// Template: "db://users/{user_id}"

let user = await mcp.read_resource("database", template.fill({ user_id: "123" }))

// Subscribe to resource changes
mcp.subscribe_resource("filesystem", "file:///watched.json", (update) => {
  log.info("Resource changed:", update.uri)
  state.config = json.parse(update.text)
})

// Unsubscribe
mcp.unsubscribe_resource("filesystem", "file:///watched.json")
```

#### 9.1.5 Prompts

MCP prompts are reusable prompt templates:

```sag
// List available prompts
let prompts = await mcp.list_prompts("prompt_server")

// Get a prompt with arguments
let prompt = await mcp.get_prompt("prompt_server", "code_review", {
  language: "rust",
  code: source_code,
  focus: "security"
})
// Returns: { messages: array<Message>, description?: string }

// Use prompt messages in context
context.history.add_all(prompt.messages)
```

#### 9.1.6 Sampling

MCP sampling allows servers to request LLM completions:

```sag
// Server-initiated sampling (for agentic MCP servers)
let result = await mcp.create_message("agentic_server", {
  messages: [
    { role: "user", content: "Analyze this data..." }
  ],
  max_tokens: 1000,

  // Model preferences
  model_preferences: {
    hints: [{ name: "claude-sonnet" }],
    cost_priority: 0.3,                 // 0-1, lower = prefer cheaper
    speed_priority: 0.7,                // 0-1, higher = prefer faster
    intelligence_priority: 0.5
  },

  // System prompt override
  system_prompt: "You are a data analyst...",

  // Include context
  include_context: "thisServer"         // "none" | "thisServer" | "allServers"
})
```

#### 9.1.7 Error Handling

```sag
// MCP error types
type McpError: Error {
  server: string                        // Server that errored
  code: number                          // JSON-RPC error code
  message: string
  data?: any                            // Additional error data
}

// Standard error codes
const MCP_PARSE_ERROR = -32700
const MCP_INVALID_REQUEST = -32600
const MCP_METHOD_NOT_FOUND = -32601
const MCP_INVALID_PARAMS = -32602
const MCP_INTERNAL_ERROR = -32603

// Handle MCP errors
try {
  let result = await mcp.call("server", "tool", params)
} catch error: McpError {
  match error.code {
    MCP_METHOD_NOT_FOUND => {
      log.error(`Tool not found on server: ${error.server}`)
      return fallback_result()
    }
    MCP_INVALID_PARAMS => {
      log.error("Invalid parameters:", error.data)
      throw ValidationError { message: "Invalid tool parameters" }
    }
    _ => throw error
  }
}
```

#### 9.1.8 Server Lifecycle Hooks

```sag
on mcp_server_connected {
  let { server_name, capabilities } = event
  log.info(`MCP server ${server_name} connected`)
  log.debug("Capabilities:", capabilities)
}

on mcp_server_disconnected {
  let { server_name, reason } = event
  log.warn(`MCP server ${server_name} disconnected: ${reason}`)
}

on mcp_server_error {
  let { server_name, error } = event
  log.error(`MCP server ${server_name} error:`, error)

  // Attempt reconnection or use fallback
  if agent.protocols.mcp.servers[server_name].lifecycle.restart_on_failure {
    await mcp.reconnect(server_name)
  }
}
```

### 9.2 A2A (Agent-to-Agent Protocol)

A2A enables agent discovery and communication. Agents can advertise capabilities, discover other agents, and delegate tasks.

#### 9.2.1 Agent Card Configuration

```sag
protocols:
  a2a:
    // Enable discovery
    discoverable: true

    // Agent capabilities
    capabilities: [research, summarize, translate, code_generation]

    // Agent card (advertised to other agents)
    card:
      name: "Research Agent"
      version: "1.0.0"
      author: "SageSyn"
      description: "Performs comprehensive research on any topic"
      icon: "search"
      tags: ["research", "ai", "summarization"]

      // Documentation URL
      documentation_url: "https://docs.example.com/research-agent"

      // Pricing (optional)
      pricing:
        model: "per_request"            // "free" | "per_request" | "subscription"
        cost_per_request: 0.01
        currency: "USD"
```

#### 9.2.2 AgentCard Type

```sag
type AgentCard {
  // Identity
  agent_id: string                      // Unique identifier
  name: string
  version: string
  author: string
  description: string

  // Discovery
  url: string                           // Agent endpoint
  capabilities: array<string>
  tags: array<string>

  // Schemas
  input_schema: JsonSchema              // Expected input format
  output_schema: JsonSchema             // Output format

  // Metadata
  icon?: string
  documentation_url?: string

  // Authentication requirements
  auth: AgentAuth

  // Limits
  rate_limits: RateLimitConfig

  // Quality metrics
  metrics?: AgentMetrics
}

type AgentAuth {
  type: "none" | "api_key" | "oauth2" | "mtls"
  oauth_scopes?: array<string>
  api_key_header?: string
}

type RateLimitConfig {
  requests_per_minute: number
  concurrent_requests: number
  burst_size?: number
}

type AgentMetrics {
  avg_response_time_ms: number
  success_rate: number
  rating: number                        // 0-5 stars
  total_requests: number
}
```

#### 9.2.3 Discovery API

```sag
// Discover agents by capability
let agents = await a2a.discover({
  capability: "research",
  tags: ["fast", "accurate"],
  min_rating: 4.0,
  max_latency_ms: 5000,
  limit: 10
})
// Returns: array<AgentCard>

// Get specific agent card
let card = await a2a.get_card("agent://sagesyn/research-agent/v1")

// Verify agent identity
let verified = await a2a.verify({
  agent_id: card.agent_id,
  check_certificate: true,
  check_reputation: true
})

// Check agent availability
let health = await a2a.health_check("agent://sagesyn/research-agent/v1")
// Returns: { available: boolean, latency_ms: number, load: number }
```

#### 9.2.4 Agent Invocation

```sag
// Simple call
let result = await a2a.call({
  agent: "agent://sagesyn/research-agent/v1",
  task: "Research quantum computing advances",
  input: { topic: "quantum computing", depth: "comprehensive" },
  timeout: 60000
})

// Call with context sharing
let result = await a2a.delegate({
  agent: "agent://sagesyn/research-agent/v1",
  task: "Continue the research",
  input: { topic: last_topic },
  context: {
    share_history: true,
    history_summary: context.history.summarize(),
    metadata: { session_id: context.session_id }
  }
})

// Streaming call
let stream = await a2a.call_stream({
  agent: "agent://sagesyn/writer-agent/v1",
  task: "Write an article",
  input: { topic: "AI Safety", length: 2000 }
})

for await chunk in stream {
  match chunk {
    TextChunk { text } => emit ag_ui.stream({ delta: text })
    ProgressChunk { percent, message } => {
      emit ag_ui.progress({ id: "writing", percent, label: message })
    }
    ErrorChunk { error } => {
      log.error("Agent error:", error)
      break
    }
    DoneChunk { result } => {
      state.article = result
    }
  }
}
```

#### 9.2.5 Parallel Orchestration

```sag
// Execute multiple agents in parallel
let results = await a2a.parallel([
  { agent: "agent://sagesyn/analyzer/v1", task: "Analyze", input: data1 },
  { agent: "agent://sagesyn/reviewer/v1", task: "Review", input: data2 },
  { agent: "agent://sagesyn/validator/v1", task: "Validate", input: data3 }
], {
  max_concurrent: 5,
  timeout_per_agent: 30000,
  fail_strategy: "partial",             // "all" | "partial" | "first"
  progress_callback: (progress) => {
    emit ag_ui.progress({
      id: "parallel_work",
      percent: progress.completed / progress.total * 100
    })
  }
})

// Results include success/failure per agent
for result in results {
  if result.success {
    process(result.value)
  } else {
    log.error(`Agent ${result.agent} failed: ${result.error}`)
  }
}
```

#### 9.2.6 Result Aggregation

```sag
// Aggregation strategies
type AggregationStrategy =
  | Merge                               // Deep merge all results
  | Vote { threshold: number }          // Majority voting
  | Rank { top_n: number }              // Rank by score
  | First                               // First successful result
  | Custom { fn: (array<any>) -> any }  // Custom function

// Aggregate results
let merged = a2a.aggregate(results, { strategy: Merge })

let voted = a2a.aggregate(results, {
  strategy: Vote { threshold: 0.6 },
  extract_vote: (result) => result.recommendation
})

let best = a2a.aggregate(results, {
  strategy: Rank { top_n: 3 },
  score_fn: (result) => result.confidence
})
```

#### 9.2.7 Trust and Authorization

```sag
protocols:
  a2a:
    // Trust configuration
    trust:
      // Allowlist patterns
      accept_from:
        - "agent://sagesyn/*"           // All SageSyn agents
        - "agent://verified-partner/*"  // Verified partners

      // Blocklist patterns
      reject_from:
        - "agent://untrusted/*"
        - "agent://spam-*"

      // Trust levels with different permissions
      trust_levels:
        high:
          agents: ["agent://sagesyn/core-*"]
          permissions: ["file_access", "external_api", "delegation"]
        medium:
          agents: ["agent://sagesyn/*"]
          permissions: ["external_api"]
        low:
          agents: ["agent://external/*"]
          permissions: []

      // Require verification for unknown agents
      require_verification: true

    // Rate limits by trust level
    rate_limits:
      high:
        requests_per_minute: 1000
        concurrent: 50
      medium:
        requests_per_minute: 100
        concurrent: 10
      low:
        requests_per_minute: 10
        concurrent: 2

    // Audit logging
    audit:
      enabled: true
      log_requests: true
      log_responses: false              // Don't log potentially sensitive responses
```

#### 9.2.8 A2A Error Types

```sag
type A2aError: Error {
  agent_id: string
  code: string
  recoverable: boolean
}

type AgentNotFoundError: A2aError {
  searched_patterns: array<string>
}

type AgentUnavailableError: A2aError {
  retry_after_ms?: number
  reason: "overloaded" | "maintenance" | "offline"
}

type AuthorizationError: A2aError {
  required_trust_level: string
  current_trust_level: string
}

type RateLimitError: A2aError {
  limit: number
  remaining: number
  reset_at: timestamp
}
```

### 9.3 AG-UI (Agent-User Interface Protocol)

AG-UI enables rich, streaming communication between agents and user interfaces.

#### 9.3.1 Configuration

```sag
protocols:
  ag_ui:
    // Enable streaming
    stream_events: true

    // Available UI components
    ui_components:
      - text
      - markdown
      - code_block
      - progress
      - table
      - chart
      - form
      - approval_request
      - file_preview
      - image
      - accordion
      - tabs
      - alert
      - spinner

    // State synchronization
    state_sync:
      enabled: true
      mode: "delta"                     // "full" | "delta"
      interval_ms: 100
      exposed_state:
        - "progress"
        - "current_step"
        - "results"
      hidden_state:
        - "api_keys"
        - "internal_cache"

    // Streaming configuration
    streaming:
      buffer_size: 50                   // Characters before flush
      flush_interval_ms: 50
      heartbeat_interval_ms: 30000
```

#### 9.3.2 Event Types

```sag
// Complete AG-UI event type system
type AgUiEvent =
  // Lifecycle events
  | RunStarted { run_id: string, agent_id: string, timestamp: timestamp }
  | RunFinished { run_id: string, status: "completed" | "cancelled" | "error" }
  | RunCancelled { run_id: string, reason?: string }

  // Streaming text events
  | TextMessageStart { message_id: string }
  | TextMessageContent { message_id: string, delta: string }
  | TextMessageEnd { message_id: string }

  // Tool events
  | ToolCallStart { tool_call_id: string, tool_name: string, args?: any }
  | ToolCallProgress { tool_call_id: string, progress: number, message?: string }
  | ToolCallEnd { tool_call_id: string, result?: any, error?: string }

  // Step tracking
  | StepStarted { step_id: string, name: string, description?: string }
  | StepProgress { step_id: string, progress: number }
  | StepCompleted { step_id: string, result?: any }
  | StepFailed { step_id: string, error: string }

  // State events
  | StateSnapshot { state: any }
  | StateDelta { path: string, operation: "set" | "delete" | "append", value?: any }

  // Custom events
  | CustomEvent { type: string, data: any }

// Emit events
emit ag_ui.event(RunStarted {
  run_id: context.turn_id,
  agent_id: context.agent_id,
  timestamp: time.now()
})

emit ag_ui.event(TextMessageContent {
  message_id: "msg_1",
  delta: "Here are the results..."
})
```

#### 9.3.3 UI Components

```sag
// Text and formatting
emit ag_ui.text({ content: "Plain text message" })
emit ag_ui.markdown({ content: "# Heading\n\n**Bold** and *italic*" })

emit ag_ui.code_block({
  language: "typescript",
  content: code_string,
  filename: "example.ts",
  highlights: [3, 7, 12]                // Line numbers to highlight
})

// Progress indicators
emit ag_ui.progress({
  id: "main_task",
  percent: 75,
  label: "Processing files...",
  status: "active"                      // "active" | "paused" | "complete" | "error"
})

emit ag_ui.spinner({ message: "Loading..." })

// Data display
emit ag_ui.table({
  headers: ["Name", "Status", "Score"],
  rows: [
    ["Alice", "Active", "95"],
    ["Bob", "Pending", "87"]
  ],
  sortable: true,
  filterable: true
})

emit ag_ui.chart({
  type: "line",                         // "line" | "bar" | "pie" | "scatter"
  title: "Performance Over Time",
  data: {
    labels: ["Jan", "Feb", "Mar"],
    datasets: [{
      label: "Score",
      data: [65, 78, 90]
    }]
  }
})

// Structured content
emit ag_ui.accordion({
  sections: [
    { title: "Summary", content: summary_text, expanded: true },
    { title: "Details", content: details_text },
    { title: "Raw Data", content: raw_data }
  ]
})

emit ag_ui.tabs({
  tabs: [
    { id: "overview", label: "Overview", content: overview },
    { id: "analysis", label: "Analysis", content: analysis },
    { id: "code", label: "Code", content: code }
  ],
  active: "overview"
})

// Alerts and notifications
emit ag_ui.alert({
  severity: "warning",                  // "info" | "success" | "warning" | "error"
  title: "Rate Limit Warning",
  message: "Approaching API rate limit",
  dismissible: true
})

// File and media
emit ag_ui.file_preview({
  path: "/path/to/file.pdf",
  mime_type: "application/pdf",
  preview_url: "https://..."
})

emit ag_ui.image({
  url: "https://...",
  alt: "Description",
  width: 800,
  height: 600
})
```

#### 9.3.4 Interactive Components

```sag
// Approval request
let approval = await ag_ui.approval_request({
  id: "delete_confirm",
  title: "Confirm Deletion",
  description: "This will permanently delete 15 files. This action cannot be undone.",
  actions: [
    { id: "confirm", label: "Delete", style: "danger" },
    { id: "cancel", label: "Cancel", style: "secondary" }
  ],
  timeout: 60000,                       // Auto-cancel after 60s
  default_action: "cancel"
})

if approval.action == "confirm" {
  await delete_files()
}

// Form input
let form_data = await ag_ui.form({
  id: "user_config",
  title: "Configuration Options",
  description: "Please provide the following information:",
  fields: [
    {
      name: "name",
      label: "Project Name",
      type: "text",
      required: true,
      placeholder: "my-project",
      validation: { pattern: "^[a-z][a-z0-9-]*$", message: "Lowercase, numbers, hyphens only" }
    },
    {
      name: "email",
      label: "Email",
      type: "email",
      required: true
    },
    {
      name: "environment",
      label: "Environment",
      type: "select",
      required: true,
      options: [
        { value: "dev", label: "Development" },
        { value: "staging", label: "Staging" },
        { value: "prod", label: "Production" }
      ],
      default: "dev"
    },
    {
      name: "features",
      label: "Features",
      type: "checkbox_group",
      options: [
        { value: "auth", label: "Authentication" },
        { value: "api", label: "API Integration" },
        { value: "analytics", label: "Analytics" }
      ]
    },
    {
      name: "description",
      label: "Description",
      type: "textarea",
      rows: 4
    }
  ],
  submit_label: "Create Project"
})

// File upload
let file = await ag_ui.file_upload({
  id: "document_upload",
  accept: ".pdf,.docx,.txt",
  max_size_bytes: 10_000_000,           // 10MB
  multiple: false
})

// Multi-select
let selected = await ag_ui.multi_select({
  id: "file_select",
  title: "Select files to process",
  options: files.map(f => ({ value: f.path, label: f.name })),
  min_selections: 1,
  max_selections: 10
})
```

#### 9.3.5 Streaming Control

```sag
// Create a managed stream
let stream = ag_ui.create_stream("response_1")

// Write to stream
stream.write("Here are the results:\n\n")
stream.write("1. First finding\n")

// Pause/resume streaming
stream.pause()
await expensive_computation()
stream.resume()

stream.write("2. Second finding\n")

// End the stream
stream.end()

// Handle cancellation
stream.on_cancel(() => {
  cleanup_resources()
  log.info("Stream cancelled by user")
})

// Stream status
let status = stream.status()  // "active" | "paused" | "ended" | "cancelled"
```

#### 9.3.6 State Synchronization

```sag
// Manual state snapshot
emit ag_ui.event(StateSnapshot {
  state: {
    progress: state.progress,
    current_step: state.current_step,
    results: state.results
  }
})

// Delta updates (more efficient)
emit ag_ui.event(StateDelta {
  path: "progress.percent",
  operation: "set",
  value: 75
})

emit ag_ui.event(StateDelta {
  path: "results",
  operation: "append",
  value: new_result
})

// Bidirectional state sync (UI can update agent state)
on ag_ui.state_update {
  let { path, value } = event

  // Validate before applying
  if validate_state_update(path, value) {
    state.set_path(path, value)
    log.debug(`State updated via UI: ${path}`)
  }
}
```

#### 9.3.7 AG-UI Error Handling

```sag
type AgUiError: Error {
  component_id?: string
  event_type: string
}

type FormValidationError: AgUiError {
  field: string
  constraint: string
  value: any
}

type TimeoutError: AgUiError {
  timeout_ms: number
  component_type: string
}

type UserCancelledError: AgUiError {
  reason?: string
}

// Handle interactive component errors
try {
  let data = await ag_ui.form({ ... })
} catch error: UserCancelledError {
  log.info("User cancelled form")
  emit ag_ui.alert({ severity: "info", message: "Operation cancelled" })
} catch error: TimeoutError {
  log.warn("Form timed out")
  // Provide defaults or retry
} catch error: FormValidationError {
  log.error(`Validation failed for ${error.field}: ${error.constraint}`)
}

---

## 10. Runtime Model

### 10.1 Execution Model

SageSyn agents are **event-driven**. Execution proceeds as:

1. **Initialization:** `on agent_start` handler runs
2. **Event Loop:** Wait for events, dispatch to handlers
3. **Handler Execution:** Process event, emit responses
4. **Termination:** `on agent_stop` handler runs

### 10.2 Async/Await

All tool calls and external operations are async:

```sag
on user_message {
  let result = await search(query)   // Await tool result
  emit response(result)
}
```

### 10.3 State Semantics

- **Isolation:** Each agent instance has isolated state
- **Persistence:** State survives across event handling
- **Serialization:** State must be JSON-serializable
- **Thread Safety:** No concurrent access within a handler

### 10.4 Context Object

Available in all handlers:

```sag
context.agent_id     // Unique agent identifier
context.session_id   // Session identifier
context.model        // Model configuration
context.state        // Agent state (alias for state)
context.message      // Current message (in user_message handler)
context.history      // Conversation history
context.turn_count   // Number of turns
```

### 10.5 Agent Lifecycle

Agents have a well-defined lifecycle with hooks for each phase. These hooks enable agents to manage resources, respond to context pressure, and handle state transitions gracefully.

#### Lifecycle Phases

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Agent Lifecycle                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────┐     ┌──────────┐     ┌────────────┐     ┌──────────┐  │
│  │ Created │────▶│ Starting │────▶│   Active   │────▶│ Stopping │  │
│  └─────────┘     └──────────┘     └────────────┘     └──────────┘  │
│       │               │                  │                  │       │
│       │          on agent_start     [Event Loop]      on agent_stop │
│       │               │                  │                  │       │
│       │               ▼                  ▼                  │       │
│       │         ┌──────────┐     ┌──────────────┐           │       │
│       │         │ init     │     │ Turn Cycle   │           │       │
│       │         │ resources│     │ (see below)  │           │       │
│       │         └──────────┘     └──────────────┘           │       │
│       │                                                     ▼       │
│       │                                              ┌──────────┐   │
│       └─────────────────[error]─────────────────────▶│ Errored  │   │
│                                                      └──────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

#### Turn Cycle

Each interaction with the agent follows a turn cycle:

```
┌─────────────────────────────────────────────────────────────────┐
│                         Turn Cycle                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  on turn_start                                                  │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────┐    on before_tool_call                         │
│  │ Process     │◀──────────────────────────┐                    │
│  │ User Input  │                           │                    │
│  └──────┬──────┘                           │                    │
│         │                            ┌─────┴─────┐              │
│         ▼                            │ Tool Call │              │
│  ┌─────────────┐                     └─────┬─────┘              │
│  │ Generate    │                           │                    │
│  │ Response    │───[tool needed]──────────▶│                    │
│  └──────┬──────┘                     on after_tool_call         │
│         │                                                       │
│         ▼                                                       │
│  on turn_end                                                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### Core Lifecycle Hooks

```sag
agent LifecycleAgent {
  description: "Agent with comprehensive lifecycle management"

  // === Agent Start/Stop ===

  on agent_start {
    // Called once when agent is initialized
    // Use for: resource initialization, connection setup, state loading
    log.info("Agent starting", { agent_id: context.agent_id })

    // Initialize external connections
    await db.connect()

    // Load persisted state
    let saved_state = await storage.load("agent_state")
    if saved_state != null {
      state.restore(saved_state)
    }
  }

  on agent_stop {
    // Called when agent is shutting down
    // Use for: cleanup, resource release, state persistence
    log.info("Agent stopping", { agent_id: context.agent_id })

    // Persist state before shutdown
    await storage.save("agent_state", state.serialize())

    // Close connections
    await db.disconnect()
  }

  // === Turn Lifecycle ===

  on turn_start {
    // Called at the beginning of each user interaction
    let turn = context.turn_count
    log.debug("Turn starting", { turn_number: turn })

    // Track timing
    state.turn_start_time = time.now()

    // Refresh any cached data if needed
    if state.cache_expired() {
      await state.refresh_cache()
    }
  }

  on turn_end {
    // Called after response is complete
    let duration = time.now() - state.turn_start_time
    log.debug("Turn completed", {
      turn_number: context.turn_count,
      duration_ms: duration
    })

    // Record metrics
    emit telemetry.turn_completed({
      turn: context.turn_count,
      duration_ms: duration,
      tokens_used: context.token_count
    })
  }

  // === Tool Lifecycle ===

  on before_tool_call(tool_call: ToolCall) {
    // Called before each tool execution
    // Use for: validation, logging, authorization
    log.debug("Tool call starting", {
      tool: tool_call.name,
      args: tool_call.arguments
    })

    // Validate tool is allowed
    if !is_tool_allowed(tool_call.name) {
      return ToolCallDecision.Reject("Tool not allowed in current context")
    }

    // Rate limit check
    if state.tool_calls_this_minute > 100 {
      return ToolCallDecision.Defer(5000) // Defer by 5 seconds
    }

    return ToolCallDecision.Allow
  }

  on after_tool_call(tool_call: ToolCall, result: ToolResult<any>) {
    // Called after tool execution completes
    // Use for: logging, metrics, result transformation
    log.debug("Tool call completed", {
      tool: tool_call.name,
      success: result.is_ok,
      duration_ms: result.metadata.execution_time_ms
    })

    state.tool_calls_this_minute += 1

    // Track token usage
    state.total_tool_tokens += result.metadata.token_cost
  }
}
```

#### Context Lifecycle Hooks

```sag
agent ContextAwareAgent {
  description: "Agent with context pressure management"

  context:
    budget:
      reserved_system: 3000
      reserved_output: 8000
      warning_threshold: 0.75
    overflow_strategy: "summarize"

  // === Context Pressure Hooks ===

  on context_warning {
    // Called when context usage exceeds warning_threshold (75%)
    let usage = context.token_count / context.token_budget
    log.warn("Context pressure warning", {
      usage_percent: usage * 100,
      remaining_tokens: context.remaining_tokens
    })

    // Proactive measures
    if context.history.length > 20 {
      // Summarize older parts of conversation
      let old_messages = context.history.window(0, 10)
      let summary = await summarize(old_messages)
      context.history.replace_range(0, 10, [{
        role: "system",
        content: `[Previous conversation summary: ${summary}]`
      }])
    }

    // Notify UI
    emit ag_ui.event(ContextWarning {
      usage_percent: usage * 100,
      recommendation: "Consider starting a new session for complex tasks"
    })
  }

  on context_overflow {
    // Called when context is at or near limit
    // Agent must take action to continue
    log.error("Context overflow", {
      token_count: context.token_count,
      token_budget: context.token_budget
    })

    // Emergency compression
    let checkpoint = context.history.checkpoint()
    context.history.compress(context.token_budget * 0.5)

    // Notify user
    emit ag_ui.event(ContextOverflow {
      action_taken: "Compressed conversation history",
      checkpoint_id: checkpoint.id
    })
  }
}
```

#### Error and State Hooks

```sag
agent RobustAgent {
  description: "Agent with comprehensive error handling"

  // === Error Handling ===

  on error(error: AgentError) {
    // Centralized error handling for unhandled errors
    log.error("Agent error", {
      type: error.type,
      message: error.message,
      stack: error.stack
    })

    match error.type {
      "ToolError" => {
        // Tool-specific error handling
        if error.retryable {
          return ErrorDecision.Retry(error.suggested_delay)
        }
        emit ag_ui.event(ErrorNotification {
          title: "Tool Error",
          message: error.message,
          recoverable: true
        })
      }
      "ContextOverflow" => {
        // Already handled by context_overflow hook
        return ErrorDecision.Ignore
      }
      "RateLimitError" => {
        // Wait and retry
        return ErrorDecision.Retry(error.retry_after_ms)
      }
      _ => {
        // Unknown error - notify and continue if possible
        emit ag_ui.event(ErrorNotification {
          title: "Unexpected Error",
          message: "An unexpected error occurred. The agent will attempt to continue.",
          recoverable: true
        })
        return ErrorDecision.Continue
      }
    }
  }

  // === State Change Tracking ===

  on state_change(change: StateChange) {
    // Called whenever agent state is modified
    // Use for: auditing, synchronization, derived state
    log.debug("State changed", {
      path: change.path,
      old_value: change.old_value,
      new_value: change.new_value
    })

    // Emit to AG-UI for state sync
    emit ag_ui.event(StateDelta {
      path: change.path,
      operation: "set",
      value: change.new_value
    })

    // Trigger derived state updates
    if change.path.starts_with("user.preferences") {
      await recalculate_recommendations()
    }
  }
}
```

#### Reasoning Traces

Agents can maintain structured reasoning traces for debugging, auditing, and transparency:

```sag
agent ReasoningAgent {
  description: "Agent with reasoning trace support"

  reasoning:
    enabled: true
    storage: "session"         // "session" | "persistent" | "external"
    max_traces: 100
    include_tool_calls: true
    include_state_changes: true

  skill analyze_problem(problem: string) -> Analysis {
    // Start a reasoning trace
    let trace = reasoning.start("problem_analysis")

    // Record reasoning steps
    trace.step("understand", `Analyzing problem: ${problem}`)

    // Gather information
    let data = await gather_relevant_data(problem)
    trace.record("data_gathered", {
      sources: data.sources,
      item_count: data.items.length
    })

    // Analyze
    trace.step("analyze", "Identifying patterns and root causes")
    let patterns = identify_patterns(data)
    trace.record("patterns", patterns)

    // Form hypothesis
    trace.step("hypothesize", "Forming hypotheses based on patterns")
    let hypotheses = form_hypotheses(patterns)

    for hyp in hypotheses {
      trace.branch(`hypothesis_${hyp.id}`) {
        trace.step("test", `Testing hypothesis: ${hyp.description}`)
        let result = await test_hypothesis(hyp, data)
        trace.record("test_result", result)
      }
    }

    // Conclude
    trace.step("conclude", "Synthesizing findings")
    let analysis = synthesize(hypotheses)

    // Complete trace
    trace.complete(analysis)

    return analysis
  }
}

// Accessing reasoning traces
let traces = reasoning.list()                    // All traces in session
let trace = reasoning.get("trace_id")            // Specific trace
let steps = trace.steps                          // All steps
let branches = trace.branches                    // Parallel explorations
let result = trace.result                        // Final result

// Export for debugging/auditing
let export = reasoning.export("trace_id", format: "json")
```

#### Lifecycle Decision Types

```sag
// Tool call decisions
type ToolCallDecision =
  | Allow                           // Proceed with tool call
  | Reject { reason: string }       // Block tool call with reason
  | Defer { delay_ms: number }      // Delay execution
  | Transform { args: any }         // Modify arguments

// Error handling decisions
type ErrorDecision =
  | Retry { delay_ms: number }      // Retry after delay
  | Continue                        // Continue despite error
  | Abort { message: string }       // Stop agent execution
  | Ignore                          // Ignore (already handled)

// State change event
type StateChange {
  path: string                      // Dot-notation path (e.g., "user.preferences.theme")
  old_value: any                    // Previous value (null if new)
  new_value: any                    // New value (null if deleted)
  timestamp: timestamp              // When change occurred
}

// Agent error types
type AgentError {
  type: string                      // Error category
  message: string                   // Human-readable message
  stack: optional<string>           // Stack trace if available
  retryable: boolean                // Whether retry makes sense
  retry_after_ms: optional<number>  // Suggested retry delay
  context: record<string, any>      // Additional context
}
```

### 10.6 Multi-Agent Patterns

SageSyn supports sophisticated multi-agent coordination patterns for complex workflows that benefit from specialized agents working together.

#### Delegation Patterns

```sag
agent OrchestratorAgent {
  description: "Coordinates multiple specialized agents"

  protocols:
    a2a:
      enabled: true

  // Simple delegation
  skill delegate_research(topic: string) -> ResearchResult {
    let result = await a2a.delegate({
      agent: "agent://sagesyn/research/v1",
      task: "Research the following topic thoroughly",
      input: { topic: topic },
      context: {
        share_history: false,
        timeout_ms: 60000
      }
    })

    return result.output
  }

  // Supervised delegation with quality checks
  skill supervised_analysis(data: Data) -> Analysis {
    let result = await a2a.delegate_supervised({
      agent: "agent://sagesyn/analyst/v1",
      task: "Analyze this data and provide insights",
      input: { data: data },
      supervision: {
        check_interval_ms: 5000,        // Check progress every 5s
        quality_threshold: 0.8,          // Minimum quality score
        max_iterations: 3,               // Max feedback rounds
        on_low_quality: "provide_feedback", // "provide_feedback" | "retry" | "abort"
        quality_evaluator: (partial_result) => {
          // Custom quality evaluation
          return evaluate_analysis_quality(partial_result)
        },
        feedback_generator: (partial_result, quality_score) => {
          // Generate improvement feedback
          return generate_improvement_suggestions(partial_result)
        }
      }
    })

    return result.output
  }

  // Delegation with human-in-the-loop
  skill delegate_with_approval(task: AgentTask) -> TaskResult {
    // First, get preview from agent
    let preview = await a2a.preview({
      agent: task.agent,
      task: task.description,
      input: task.input
    })

    // Request human approval via AG-UI
    let approval = await ag_ui.approval_request({
      title: "Approve Agent Delegation",
      description: `Agent ${task.agent} will perform: ${task.description}`,
      details: {
        estimated_time: preview.estimated_time,
        estimated_cost: preview.estimated_cost,
        capabilities_used: preview.capabilities
      },
      actions: [
        { id: "approve", label: "Approve", style: "primary" },
        { id: "modify", label: "Modify Task", style: "secondary" },
        { id: "reject", label: "Reject", style: "danger" }
      ]
    })

    match approval.action {
      "approve" => await a2a.delegate({ ...task }),
      "modify" => {
        let modified = await ag_ui.form({ fields: task_modification_fields })
        await a2a.delegate({ ...task, ...modified })
      },
      "reject" => throw DelegationRejectedError(approval.reason)
    }
  }
}
```

#### Parallel Orchestration

```sag
agent ParallelOrchestrator {
  description: "Orchestrates parallel agent execution"

  // Fan-out pattern: one task to many agents
  skill parallel_research(topics: array<string>) -> array<ResearchResult> {
    let tasks = topics.map(topic => ({
      agent: "agent://sagesyn/research/v1",
      task: "Research this topic",
      input: { topic: topic }
    }))

    let results = await a2a.parallel(tasks, {
      max_concurrent: 5,              // Limit concurrent executions
      fail_strategy: "partial",       // "partial" | "all_or_nothing" | "best_effort"
      timeout_ms: 120000,             // Overall timeout
      progress_callback: (progress) => {
        emit ag_ui.event(Progress {
          completed: progress.completed,
          total: progress.total,
          current_tasks: progress.in_progress
        })
      }
    })

    // Handle partial failures
    let successes = results.filter(r => r.is_ok)
    let failures = results.filter(r => r.is_err)

    if failures.length > 0 {
      log.warn("Some research tasks failed", {
        failed_count: failures.length,
        errors: failures.map(f => f.error.message)
      })
    }

    return successes.map(r => r.value)
  }

  // Fan-in pattern: aggregate results from multiple agents
  skill comprehensive_analysis(topic: string) -> ComprehensiveReport {
    // Run different analyses in parallel
    let [technical, market, sentiment] = await a2a.parallel([
      { agent: "agent://sagesyn/technical-analyst/v1", task: "Technical analysis", input: { topic } },
      { agent: "agent://sagesyn/market-analyst/v1", task: "Market analysis", input: { topic } },
      { agent: "agent://sagesyn/sentiment-analyst/v1", task: "Sentiment analysis", input: { topic } }
    ])

    // Aggregate results
    return {
      topic: topic,
      technical_analysis: technical.output,
      market_analysis: market.output,
      sentiment_analysis: sentiment.output,
      generated_at: time.now()
    }
  }

  // Pipeline pattern: sequential agent chain
  skill agent_pipeline(input: RawData) -> ProcessedResult {
    let result = await a2a.pipeline([
      { agent: "agent://sagesyn/extractor/v1", task: "Extract structured data" },
      { agent: "agent://sagesyn/validator/v1", task: "Validate extracted data" },
      { agent: "agent://sagesyn/enricher/v1", task: "Enrich with external data" },
      { agent: "agent://sagesyn/formatter/v1", task: "Format final output" }
    ], {
      input: input,
      pass_through: true,            // Each stage receives previous output
      on_stage_complete: (stage, result) => {
        log.info(`Pipeline stage completed: ${stage.agent}`)
      }
    })

    return result
  }
}
```

#### Result Aggregation

```sag
// Aggregation strategies
let merged = a2a.aggregate(results, {
  strategy: Merge,                   // Combine all results
  conflict_resolution: "latest"      // "latest" | "first" | "custom"
})

let voted = a2a.aggregate(results, {
  strategy: Vote,                    // Consensus-based selection
  threshold: 0.6,                    // 60% agreement required
  tie_breaker: "confidence"          // Use confidence scores to break ties
})

let ranked = a2a.aggregate(results, {
  strategy: Rank,                    // Rank and select top results
  top_n: 3,
  ranking_criteria: (result) => result.confidence * result.completeness
})

let synthesized = a2a.aggregate(results, {
  strategy: Synthesize,              // AI-powered synthesis
  synthesizer_agent: "agent://sagesyn/synthesizer/v1",
  synthesis_prompt: "Combine these analyses into a coherent report"
})

// Custom aggregation
let custom = a2a.aggregate(results, {
  strategy: Custom,
  aggregator: (results) => {
    // Custom aggregation logic
    let best = results.max_by(r => r.score)
    let supporting = results.filter(r => r.agrees_with(best))
    return {
      primary: best,
      supporting_evidence: supporting,
      confidence: supporting.length / results.length
    }
  }
})
```

#### Agent Teams

```sag
// Define reusable agent teams
type AgentTeam = Team {
  name: string
  agents: array<AgentRef>
  coordination: CoordinationStrategy
}

let research_team = Team {
  name: "Research Team",
  agents: [
    { ref: "agent://sagesyn/researcher/v1", role: "lead" },
    { ref: "agent://sagesyn/fact-checker/v1", role: "validator" },
    { ref: "agent://sagesyn/summarizer/v1", role: "synthesizer" }
  ],
  coordination: Sequential {
    order: ["lead", "validator", "synthesizer"],
    pass_results: true
  }
}

let analysis_team = Team {
  name: "Analysis Team",
  agents: [
    { ref: "agent://sagesyn/data-analyst/v1", role: "analyst" },
    { ref: "agent://sagesyn/visualizer/v1", role: "visualizer" },
    { ref: "agent://sagesyn/reporter/v1", role: "reporter" }
  ],
  coordination: Parallel {
    roles_parallel: ["analyst", "visualizer"],
    then_sequential: ["reporter"]
  }
}

// Use teams
skill team_research(topic: string) -> Report {
  let result = await research_team.execute({
    task: "Research and validate information about this topic",
    input: { topic: topic }
  })
  return result.output
}
```

### 10.7 Memory and Persistence

Agents need various forms of memory beyond the immediate conversation context. SageSyn provides structured memory primitives for short-term working memory, long-term persistent storage, and semantic vector memory.

#### Memory Configuration

```sag
agent MemoryAgent {
  description: "Agent with comprehensive memory capabilities"

  memory:
    // Working memory (session-scoped)
    working:
      max_items: 100                 // Maximum items in working memory
      ttl_ms: 3600000               // Time-to-live (1 hour)
      overflow_strategy: "lru"      // "lru" | "priority" | "fifo"

    // Long-term memory (persistent across sessions)
    long_term:
      enabled: true
      storage: "local"              // "local" | "remote" | "hybrid"
      encryption: true              // Encrypt at rest

    // Semantic/vector memory for similarity search
    semantic:
      enabled: true
      embedding_model: "text-embedding-3-small"
      similarity_threshold: 0.75
      max_results: 10
      index:
        type: "hnsw"               // "flat" | "hnsw" | "ivf"
        dimensions: 1536
}
```

#### Working Memory

```sag
// Working memory is for temporary, session-scoped data
// Automatically cleared on session end

// Store items with optional metadata
memory.working.set("current_task", {
  description: "Analyzing sales data",
  started_at: time.now(),
  priority: "high"
})

// Retrieve items
let task = memory.working.get("current_task")

// Store with TTL override
memory.working.set("cache_data", data, { ttl_ms: 60000 })

// Check existence
if memory.working.has("current_task") {
  // ...
}

// Delete
memory.working.delete("current_task")

// List all keys
let keys = memory.working.keys()

// Get all items matching pattern
let task_items = memory.working.scan("task:*")

// Clear all working memory
memory.working.clear()
```

#### Long-Term Memory

```sag
// Long-term memory persists across sessions
// Organized in collections for different data types

// Create/access collections
let conversations = memory.long_term.collection("conversations")
let user_preferences = memory.long_term.collection("user_preferences")
let learned_facts = memory.long_term.collection("facts")

// Store documents
await conversations.store({
  id: context.session_id,
  content: context.history.summarize(),
  metadata: {
    user_id: context.user_id,
    timestamp: time.now(),
    turn_count: context.turn_count,
    topics: extract_topics(context.history)
  }
})

// Retrieve by ID
let conversation = await conversations.get(session_id)

// Query with filters
let recent_conversations = await conversations.query({
  filter: {
    "metadata.user_id": context.user_id,
    "metadata.timestamp": { $gt: time.days_ago(7) }
  },
  sort: { "metadata.timestamp": "desc" },
  limit: 10
})

// Update documents
await user_preferences.update(user_id, {
  $set: { "preferences.theme": "dark" },
  $push: { "preferences.recent_topics": topic }
})

// Delete documents
await conversations.delete(session_id)

// Delete with filter
await conversations.delete_many({
  filter: { "metadata.timestamp": { $lt: time.days_ago(30) } }
})
```

#### Semantic Memory

```sag
// Semantic memory enables similarity-based retrieval
// Useful for finding relevant past interactions, facts, or documents

// Store with automatic embedding
await memory.semantic.store({
  content: "The user prefers concise responses and technical detail.",
  metadata: {
    type: "user_preference",
    user_id: context.user_id,
    confidence: 0.9
  }
})

// Store with explicit embedding
let embedding = await embeddings.create(content)
await memory.semantic.store({
  content: content,
  embedding: embedding,
  metadata: { ... }
})

// Semantic search
let relevant = await memory.semantic.search({
  query: "What does the user prefer for response format?",
  filter: { "metadata.user_id": context.user_id },
  limit: 5,
  threshold: 0.7          // Minimum similarity score
})

for item in relevant {
  log.debug("Found relevant memory", {
    content: item.content,
    similarity: item.score,
    metadata: item.metadata
  })
}

// Hybrid search (semantic + keyword)
let results = await memory.semantic.hybrid_search({
  query: "user preferences for code formatting",
  keyword_filter: ["python", "formatting"],
  semantic_weight: 0.7,    // Balance between semantic and keyword
  limit: 10
})
```

#### Memory Consolidation

```sag
agent ConsolidatingAgent {
  description: "Agent that consolidates and maintains memories"

  // Periodic consolidation of working memory to long-term
  on turn_end {
    // Check if significant information was discussed
    if should_remember(context.history.last_turn()) {
      let facts = extract_facts(context.history.last_turn())

      for fact in facts {
        // Check for existing similar memories
        let existing = await memory.semantic.search({
          query: fact.content,
          threshold: 0.9,
          limit: 1
        })

        if existing.is_empty {
          // New fact - store it
          await memory.semantic.store(fact)
        } else {
          // Similar exists - potentially update
          await memory.semantic.update(existing[0].id, {
            content: merge_facts(existing[0].content, fact.content),
            metadata: {
              ...existing[0].metadata,
              updated_at: time.now(),
              reinforcement_count: existing[0].metadata.reinforcement_count + 1
            }
          })
        }
      }
    }
  }

  // Scheduled maintenance
  skill consolidate_memories() {
    // Merge similar memories
    await memory.semantic.deduplicate({
      similarity_threshold: 0.95,
      merge_strategy: "latest"
    })

    // Prune old, low-value memories
    await memory.semantic.prune({
      filter: {
        "metadata.updated_at": { $lt: time.days_ago(90) },
        "metadata.access_count": { $lt: 3 }
      }
    })

    // Optimize index
    await memory.semantic.optimize()
  }
}
```

#### Memory-Augmented Responses

```sag
agent MemoryAugmentedAgent {
  description: "Agent that uses memory to enhance responses"

  on user_message(msg) {
    // Retrieve relevant memories before responding
    let relevant_memories = await memory.semantic.search({
      query: msg.content,
      limit: 5
    })

    // Add to context
    let memory_context = relevant_memories
      .map(m => `[Memory: ${m.content}]`)
      .join("\n")

    // Use MessageBuilder to include memory context
    let response = context.message_builder()
      .system(`Relevant memories:\n${memory_context}`)
      .user(msg.content)
      .build()

    // Generate response with memory context
    // The model now has access to relevant past information
  }
}
```

---

## 11. Standard Library

The standard library provides built-in modules for common operations. All standard library functions are available without explicit import.

### 11.1 String Module

**Properties:**
```sag
str.length              // Number of characters
str.is_empty            // true if length == 0
```

**Case Operations:**
```sag
str.upper()             // Convert to uppercase: "hello" -> "HELLO"
str.lower()             // Convert to lowercase: "HELLO" -> "hello"
str.capitalize()        // Capitalize first: "hello world" -> "Hello world"
str.title()             // Title case: "hello world" -> "Hello World"
```

**Whitespace:**
```sag
str.trim()              // Remove leading/trailing whitespace
str.trim_start()        // Remove leading whitespace
str.trim_end()          // Remove trailing whitespace
str.pad_start(len, ch)  // Pad left: "5".pad_start(3, "0") -> "005"
str.pad_end(len, ch)    // Pad right: "5".pad_end(3, "0") -> "500"
```

**Search and Replace:**
```sag
str.contains(substr)    // Check if contains substring
str.starts_with(prefix) // Check prefix match
str.ends_with(suffix)   // Check suffix match
str.index_of(substr)    // First index or -1
str.last_index_of(sub)  // Last index or -1
str.replace(old, new)   // Replace first occurrence
str.replace_all(old, new) // Replace all occurrences
```

**Split and Join:**
```sag
str.split(sep)          // Split by separator: "a,b,c".split(",") -> ["a","b","c"]
str.split_lines()       // Split by newlines
str.chars()             // Split into characters: "abc" -> ["a","b","c"]
array.join(sep)         // Join array: ["a","b"].join(",") -> "a,b"
```

**Extraction:**
```sag
str.slice(start, end)   // Substring: "hello".slice(1, 4) -> "ell"
str.char_at(index)      // Character at index (or null)
str.substr(start, len)  // Substring by length
```

**Conversion:**
```sag
str.to_number()         // Parse as number (or null): "42".to_number() -> 42
str.to_boolean()        // "true" -> true, else false
str.bytes()             // Get UTF-8 bytes as array<number>
```

**Pattern Matching:**
```sag
str.matches(pattern)    // Regex match check
str.match(pattern)      // Extract match groups
str.match_all(pattern)  // Extract all matches
str.replace_regex(pat, rep) // Regex replace
```

### 11.2 Array Module

**Properties:**
```sag
arr.length              // Number of elements
arr.is_empty            // true if length == 0
arr.first               // First element (or null)
arr.last                // Last element (or null)
```

**Add/Remove:**
```sag
arr.push(item)          // Add to end, return new length
arr.pop()               // Remove and return last (or null)
arr.unshift(item)       // Add to start, return new length
arr.shift()             // Remove and return first (or null)
arr.insert(index, item) // Insert at index
arr.remove_at(index)    // Remove at index, return item
```

**Transform:**
```sag
arr.map(fn)             // Transform: [1,2].map(x => x*2) -> [2,4]
arr.filter(fn)          // Filter: [1,2,3].filter(x => x>1) -> [2,3]
arr.reduce(fn, init)    // Reduce: [1,2,3].reduce((a,b)=>a+b, 0) -> 6
arr.flat()              // Flatten one level: [[1],[2,3]] -> [1,2,3]
arr.flat_map(fn)        // Map then flatten
arr.compact()           // Remove nulls: [1,null,2] -> [1,2]
arr.unique()            // Remove duplicates: [1,1,2] -> [1,2]
```

**Search:**
```sag
arr.find(fn)            // First match or null
arr.find_index(fn)      // Index of first match or -1
arr.find_last(fn)       // Last match or null
arr.find_last_index(fn) // Index of last match or -1
arr.contains(item)      // Check if contains (by value)
arr.index_of(item)      // Index of item or -1
arr.every(fn)           // All match predicate
arr.some(fn)            // Any matches predicate
```

**Order:**
```sag
arr.sort()              // Sort ascending (strings/numbers)
arr.sort_by(fn)         // Sort by key: users.sort_by(u => u.age)
arr.sort_desc()         // Sort descending
arr.reverse()           // Reverse in place
arr.shuffle()           // Random shuffle
```

**Slice and Combine:**
```sag
arr.slice(start, end)   // Sub-array (non-mutating)
arr.splice(start, count, ...items) // Remove and insert
arr.concat(other)       // Concatenate arrays
arr.zip(other)          // Zip: [1,2].zip(["a","b"]) -> [[1,"a"],[2,"b"]]
arr.chunk(size)         // Split into chunks: [1,2,3,4].chunk(2) -> [[1,2],[3,4]]
arr.partition(fn)       // Split by predicate: [1,2,3].partition(x=>x>1) -> [[2,3],[1]]
```

**Aggregation:**
```sag
arr.sum()               // Sum numbers: [1,2,3].sum() -> 6
arr.min()               // Minimum value
arr.max()               // Maximum value
arr.average()           // Average value
arr.group_by(fn)        // Group: users.group_by(u => u.role)
arr.count_by(fn)        // Count by key
```

### 11.3 Record Module

**Properties:**
```sag
rec.size                // Number of entries
rec.is_empty            // true if size == 0
```

**Access:**
```sag
rec.keys()              // Get all keys as array
rec.values()            // Get all values as array
rec.entries()           // Get [key, value] pairs
rec.has(key)            // Check if key exists
rec.get(key)            // Get value or null
rec.get_or(key, def)    // Get value or default
```

**Modify:**
```sag
rec.set(key, value)     // Set value, return record
rec.delete(key)         // Remove key, return record
rec.clear()             // Remove all entries
rec.update(key, fn)     // Update value with function
```

**Transform:**
```sag
rec.merge(other)        // Shallow merge: { ...rec, ...other }
rec.deep_merge(other)   // Deep merge nested records
rec.map_keys(fn)        // Transform keys
rec.map_values(fn)      // Transform values
rec.filter(fn)          // Filter by (key, value) => boolean
rec.pick(keys)          // Select subset: rec.pick(["a","b"])
rec.omit(keys)          // Exclude keys: rec.omit(["password"])
rec.invert()            // Swap keys/values
```

### 11.4 Number Module

**Properties:**
```sag
num.is_nan              // Check if NaN
num.is_finite           // Check if finite
num.is_integer          // Check if whole number
```

**Rounding:**
```sag
num.round()             // Round to nearest integer
num.floor()             // Round down
num.ceil()              // Round up
num.trunc()             // Truncate decimals
num.to_fixed(digits)    // Format: 3.14159.to_fixed(2) -> "3.14"
```

**Math Operations:**
```sag
num.abs()               // Absolute value
num.sign()              // -1, 0, or 1
num.pow(exp)            // Exponentiation
num.sqrt()              // Square root
num.log()               // Natural logarithm
num.log10()             // Base-10 logarithm
```

**Conversion:**
```sag
num.to_string()         // Convert to string
num.to_hex()            // Hex string: 255.to_hex() -> "ff"
num.to_binary()         // Binary string: 5.to_binary() -> "101"
```

**Constants:**
```sag
math.PI                 // 3.141592653589793
math.E                  // 2.718281828459045
math.INFINITY           // Positive infinity
math.NEG_INFINITY       // Negative infinity
math.NAN                // Not a number
```

**Math Functions:**
```sag
math.sin(x)             // Sine
math.cos(x)             // Cosine
math.tan(x)             // Tangent
math.asin(x)            // Arc sine
math.acos(x)            // Arc cosine
math.atan(x)            // Arc tangent
math.atan2(y, x)        // Two-argument arc tangent
math.random()           // Random [0, 1)
math.random_int(min, max) // Random integer in range
math.clamp(val, min, max) // Clamp to range
```

### 11.5 JSON Module

```sag
json.parse(str)         // Parse JSON string -> value (throws on invalid)
json.try_parse(str)     // Parse JSON -> Result<value, Error>
json.stringify(val)     // Convert to JSON string
json.stringify_pretty(val, indent) // Pretty-print with indentation
json.valid(str)         // Check if valid JSON: true/false
```

### 11.6 HTTP Module

**Request Methods:**
```sag
http.get(url, options?) -> Promise<Response>
http.post(url, body, options?) -> Promise<Response>
http.put(url, body, options?) -> Promise<Response>
http.patch(url, body, options?) -> Promise<Response>
http.delete(url, options?) -> Promise<Response>
http.head(url, options?) -> Promise<Response>
```

**Options Type:**
```sag
type HttpOptions {
  headers?: record<string, string>
  timeout?: number                    // Milliseconds
  retry?: number                      // Retry count
  follow_redirects?: boolean          // Default: true
}
```

**Response Type:**
```sag
type Response {
  status: number                      // HTTP status code
  status_text: string                 // Status message
  headers: record<string, string>
  ok: boolean                         // status in 200-299

  json() -> Promise<any>              // Parse JSON body
  text() -> Promise<string>           // Get text body
  bytes() -> Promise<array<number>>   // Get raw bytes
}
```

### 11.7 Time Module

**Current Time:**
```sag
time.now()              // Current timestamp
time.now_millis()       // Current time in milliseconds
time.now_iso()          // ISO 8601 string: "2025-12-26T10:30:00Z"
```

**Parsing:**
```sag
time.parse(str)         // Parse date string -> timestamp
time.parse_iso(str)     // Parse ISO 8601 -> timestamp
time.from_millis(ms)    // Milliseconds -> timestamp
time.from_unix(secs)    // Unix seconds -> timestamp
```

**Formatting:**
```sag
ts.format(pattern)      // Custom format
ts.to_iso()             // ISO 8601 string
ts.to_unix()            // Unix seconds
ts.to_millis()          // Milliseconds since epoch
```

**Format Patterns:**
```
%Y - 4-digit year       %m - 2-digit month      %d - 2-digit day
%H - 24-hour            %M - Minutes            %S - Seconds
%I - 12-hour            %p - AM/PM              %z - Timezone offset
```

**Arithmetic:**
```sag
ts.add_days(n)          // Add days
ts.add_hours(n)         // Add hours
ts.add_minutes(n)       // Add minutes
ts.add_seconds(n)       // Add seconds
ts.add_millis(n)        // Add milliseconds

time.diff(a, b)         // Difference in milliseconds
time.diff_days(a, b)    // Difference in days
```

**Components:**
```sag
ts.year                 // Year component
ts.month                // Month (1-12)
ts.day                  // Day (1-31)
ts.hour                 // Hour (0-23)
ts.minute               // Minute (0-59)
ts.second               // Second (0-59)
ts.weekday              // Day of week (0=Sunday)
ts.day_of_year          // Day of year (1-366)
```

**Comparison:**
```sag
ts.is_before(other)     // Check if before
ts.is_after(other)      // Check if after
ts.is_same_day(other)   // Same calendar day
```

### 11.8 Log Module

**Levels:**
```sag
log.debug(message, data?)   // Debug level (verbose)
log.info(message, data?)    // Info level (normal)
log.warn(message, data?)    // Warning level
log.error(message, data?)   // Error level
```

**Structured Logging:**
```sag
log.info("User logged in", { user_id: "123", ip: "192.168.1.1" })
// Output: [INFO] User logged in {"user_id":"123","ip":"192.168.1.1"}
```

**Configuration:**
```sag
log.set_level("debug")      // Set minimum level
log.set_format("json")      // json | text | pretty
```

### 11.9 Crypto Module

**Hashing:**
```sag
crypto.hash(data, algo)     // Hash data: "sha256", "sha512", "md5"
crypto.hash_hex(data, algo) // Hash as hex string
```

**Random:**
```sag
crypto.random_bytes(n)      // n random bytes
crypto.random_uuid()        // UUID v4: "550e8400-e29b-41d4-a716-446655440000"
crypto.random_hex(n)        // n random hex characters
```

**Encoding:**
```sag
crypto.base64_encode(data)  // Encode to base64
crypto.base64_decode(str)   // Decode from base64
crypto.hex_encode(data)     // Encode to hex
crypto.hex_decode(str)      // Decode from hex
```

### 11.10 Env Module

```sag
env.get(name)               // Get environment variable (or null)
env.get_or(name, default)   // Get with default
env.require(name)           // Get or throw error
env.has(name)               // Check if set
env.all()                   // Get all as record
```

### 11.11 Assert Module (Testing)

```sag
assert.equal(a, b)          // a == b
assert.not_equal(a, b)      // a != b
assert.true(val)            // val is true
assert.false(val)           // val is false
assert.null(val)            // val is null
assert.not_null(val)        // val is not null
assert.throws(fn)           // fn throws an error
assert.deep_equal(a, b)     // Deep equality check
assert.contains(arr, item)  // Array contains item
assert.matches(str, pattern) // String matches regex
```

---

## 12. Compilation

### 12.1 Compilation Targets

| Target | Output |
|--------|--------|
| TypeScript | `.ts` files, ES2022+ |
| Python | `.py` files, 3.10+ |
| Go | `.go` files, 1.21+ |

### 12.2 CLI Commands

```bash
sag compile agent.sag --target typescript
sag compile agent.sag --target python
sag compile agent.sag --target go
sag check agent.sag
sag fmt agent.sag
```

### 12.3 Compilation Phases

1. **Lexing:** Source -> Tokens
2. **Parsing:** Tokens -> AST
3. **Type Checking:** AST validation
4. **Code Generation:** AST -> Target code

### 12.4 Source Maps

Generated code includes source maps for debugging.

---

## Appendix A: Grammar (EBNF)

```ebnf
program     = item* ;
item        = agent | skill | type_def | function ;

agent       = "agent" IDENT "{" agent_body "}" ;
agent_body  = (metadata | model | state | protocols | tool | handler)* ;

type_def    = "type" IDENT generics? ("=" type_expr | "{" field* "}") ;
generics    = "<" IDENT ("," IDENT)* ">" ;
field       = IDENT "?"? ":" type_expr ;

function    = "async"? "fn" IDENT "(" params? ")" ("->" type_expr)? block ;
params      = param ("," param)* ;
param       = IDENT ":" type_expr ("=" expr)? ;

type_expr   = named_type | array_type | record_type | tuple_type
            | optional_type | union_type | function_type ;

statement   = let_stmt | var_stmt | if_stmt | for_stmt | while_stmt
            | return_stmt | emit_stmt | expr_stmt | block ;

expr        = assignment ;
assignment  = or_expr ("=" assignment)? ;
or_expr     = and_expr ("||" and_expr)* ;
and_expr    = equality ("&&" equality)* ;
equality    = comparison (("==" | "!=") comparison)* ;
comparison  = term (("<" | "<=" | ">" | ">=") term)* ;
term        = factor (("+" | "-") factor)* ;
factor      = unary (("*" | "/" | "%") unary)* ;
unary       = ("!" | "-") unary | call ;
call        = primary (("(" args? ")" | "." IDENT | "[" expr "]"))* ;
primary     = literal | IDENT | "(" expr ")" | array_lit | record_lit ;

literal     = STRING | NUMBER | "true" | "false" | "null" ;
```

---

## Appendix B: Design Decisions

### Resolved

| Decision | Choice | Rationale |
|----------|--------|-----------|
| **Generics** | With constraints (`T: Serializable`) | More powerful type system |
| **Null Safety** | Strict by default | Prevent null pointer errors |
| **Pattern Matching** | Exhaustive with guards | Catch missing cases at compile time |
| **Range Syntax** | Yes (`0..10`, `0..=10`) | Ergonomic for loops |
| **Destructuring** | Yes (objects and arrays) | Modern syntax, reduces boilerplate |
| **Default Parameters** | Yes (`name = "World"`) | Flexible function APIs |
| **Spread Operator** | Yes (`...arr`, `...obj`) | Familiar JS/TS syntax, essential for immutability |
| **Pipeline Operator** | Yes (`\|>`) | Improves readability, functional style |
| **Error Handling** | Result + Option + try-catch | Multiple mechanisms for different use cases |
| **Type Compatibility** | Structural | Flexible, familiar to TypeScript users |

### Agent-Centric Decisions (Resolved)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| **Context Management** | First-class token budget API | Essential for LLM agents to manage context limits |
| **Tool Execution** | Parallel/Race/Pipeline patterns | Real agents need sophisticated tool orchestration |
| **Tool Configuration** | Declarative with retry/cache/circuit-breaker | Reliability is critical for production agents |
| **Streaming** | `Stream<T>` type with pause/resume | Progressive output improves UX |
| **MCP Integration** | Deep protocol support with lifecycle hooks | MCP is the standard for agent-tool integration |
| **A2A Protocol** | AgentCard + discovery + delegation + trust | Multi-agent systems need standardized interop |
| **AG-UI Protocol** | Full event types + interactive components | Rich UI enables human-in-the-loop workflows |
| **Agent Lifecycle** | Extended hooks (turn, tool, context, error) | Agents need fine-grained lifecycle control |
| **Memory System** | Working + Long-term + Semantic (vector) | Different use cases need different memory types |
| **Multi-Agent** | Supervised delegation + parallel orchestration | Complex tasks benefit from agent coordination |
| **Reasoning Traces** | Built-in tracing with branching | Debugging and auditing require observability |

### Open Questions

1. **Module System:** How to handle circular imports?
2. **Concurrency:** Do we need explicit concurrency primitives beyond async/await?
3. **Macros:** Should the language support compile-time macros?
4. **Decorators:** Are decorators/attributes useful for metadata?
5. **Step Syntax:** Should `step` be part of range (`0..100 step 10`)?
6. **Type Inference:** How aggressive should inference be?
7. **Operator Overloading:** Allow custom types to define operators?
8. **Visibility Modifiers:** Beyond `pub`, do we need `private`/`protected`?
9. **Memory Backends:** Which vector databases should be supported natively?
10. **Agent Sandboxing:** How to isolate delegated agents for security?
11. **Streaming Backpressure:** How to handle slow consumers in streaming?
12. **Context Checkpoints:** Should checkpoints be persistent across sessions?

---

## Appendix C: Complete Agentic Examples

This appendix provides complete, production-ready agent implementations that demonstrate the full capabilities of SageSyn. Each example validates the language specification through practical use cases.

### C.1 Research Agent

A research agent that uses MCP tools, manages context intelligently, and streams responses to the user.

```sag
/// ResearchAgent - Conducts thorough research using web search and content analysis
/// Demonstrates: MCP integration, context management, tool caching, streaming, error handling

agent ResearchAgent {
  description: "AI-powered research assistant that searches the web, analyzes content, and synthesizes comprehensive reports"
  version: "1.0.0"

  model:
    id: "claude-sonnet-4-20250514"
    max_tokens: 16000
    temperature: 0.3

  // === Context Management ===
  context:
    budget:
      reserved_system: 2000      // System prompt and instructions
      reserved_output: 6000      // Space for comprehensive responses
      warning_threshold: 0.7     // Warn at 70% usage
    overflow_strategy: "summarize"
    history:
      max_turns: 30
      compression: "auto"

  // === MCP Server Configuration ===
  protocols:
    mcp:
      servers:
        - name: brave_search
          transport: stdio
          command: "npx"
          args: ["-y", "@anthropic/mcp-server-brave-search"]
          env:
            BRAVE_API_KEY: "${env.BRAVE_API_KEY}"
          lifecycle:
            health_check_interval_ms: 60000
            restart_on_failure: true
          requires:
            tools: ["brave_web_search"]

        - name: web_fetch
          transport: stdio
          command: "npx"
          args: ["-y", "@anthropic/mcp-server-fetch"]
          lifecycle:
            restart_on_failure: true
          requires:
            tools: ["fetch"]

    ag_ui:
      enabled: true
      streaming: true

  // === State ===
  state:
    current_research: optional<ResearchSession>
    cached_sources: record<string, CachedSource>
    search_history: array<SearchQuery>

  // === Types ===
  type ResearchSession {
    topic: string
    queries: array<string>
    sources: array<Source>
    findings: array<Finding>
    started_at: timestamp
  }

  type Source {
    url: string
    title: string
    content: string
    relevance_score: number
    fetched_at: timestamp
  }

  type Finding {
    claim: string
    evidence: array<string>
    source_urls: array<string>
    confidence: number
  }

  type CachedSource {
    content: string
    fetched_at: timestamp
    ttl_ms: number
  }

  // === Tools with Caching and Retry ===
  tool search_web(query: string, count: number = 10) -> array<SearchResult> {
    description: "Search the web for information"
    retry:
      max_attempts: 3
      backoff: exponential
    cache:
      enabled: true
      ttl: 1800000              // 30 minutes
      key: (query, count) => `search:${query}:${count}`

    let results = await mcp.call_tool("brave_search", "brave_web_search", {
      query: query,
      count: count
    })
    return results
  }

  tool fetch_content(url: string) -> Result<string, FetchError> {
    description: "Fetch and extract content from a URL"
    timeout: 30000
    retry:
      max_attempts: 2
      backoff: linear
    cache:
      enabled: true
      ttl: 3600000              // 1 hour
      stale_while_revalidate: true

    // Check local cache first
    if state.cached_sources[url] != null {
      let cached = state.cached_sources[url]
      if time.now() - cached.fetched_at < cached.ttl_ms {
        return Ok(cached.content)
      }
    }

    let result = await mcp.call_tool("web_fetch", "fetch", { url: url })

    if result.is_ok {
      state.cached_sources[url] = CachedSource {
        content: result.value,
        fetched_at: time.now(),
        ttl_ms: 3600000
      }
    }

    return result
  }

  // === Main Research Skill ===
  skill research(topic: string, depth: "quick" | "thorough" = "thorough") -> ResearchReport {
    description: "Conduct comprehensive research on a topic"

    // Initialize session
    state.current_research = ResearchSession {
      topic: topic,
      queries: [],
      sources: [],
      findings: [],
      started_at: time.now()
    }

    // Notify UI of research start
    emit ag_ui.event(RunStarted {
      run_id: context.session_id,
      agent_id: context.agent_id,
      metadata: { topic: topic, depth: depth }
    })

    // Step 1: Generate search queries
    emit ag_ui.event(StepStarted { step_id: "queries", name: "Generating search queries" })

    let queries = await generate_search_queries(topic, depth)
    state.current_research.queries = queries

    emit ag_ui.event(StepCompleted {
      step_id: "queries",
      name: "Generating search queries",
      result: { query_count: queries.length }
    })

    // Step 2: Execute searches in parallel
    emit ag_ui.event(StepStarted { step_id: "search", name: "Searching the web" })

    let search_results = await tools.parallel(
      queries.map(q => search_web(q, depth == "thorough" ? 10 : 5))
    )

    // Flatten and deduplicate results
    let all_results = search_results
      .flat()
      .unique_by(r => r.url)
      .sort_by(r => -r.relevance)
      .take(depth == "thorough" ? 20 : 10)

    emit ag_ui.event(StepCompleted {
      step_id: "search",
      name: "Searching the web",
      result: { results_found: all_results.length }
    })

    // Step 3: Fetch and analyze content
    emit ag_ui.event(StepStarted { step_id: "analyze", name: "Analyzing sources" })

    let stream = ag_ui.create_stream("analysis_progress")

    for (index, result) in all_results.enumerate() {
      stream.write(`Analyzing source ${index + 1}/${all_results.length}: ${result.title}\n`)

      let content_result = await fetch_content(result.url)

      match content_result {
        Ok(content) => {
          let analysis = await analyze_content(content, topic)
          state.current_research.sources.push(Source {
            url: result.url,
            title: result.title,
            content: content.slice(0, 5000),  // Keep summary
            relevance_score: analysis.relevance,
            fetched_at: time.now()
          })

          for finding in analysis.findings {
            state.current_research.findings.push(finding)
          }
        }
        Err(error) => {
          log.warn("Failed to fetch source", { url: result.url, error: error.message })
        }
      }
    }

    stream.end()
    emit ag_ui.event(StepCompleted {
      step_id: "analyze",
      name: "Analyzing sources",
      result: { sources_analyzed: state.current_research.sources.length }
    })

    // Step 4: Synthesize findings
    emit ag_ui.event(StepStarted { step_id: "synthesize", name: "Synthesizing report" })

    let report = await synthesize_report(
      state.current_research.topic,
      state.current_research.findings,
      state.current_research.sources
    )

    emit ag_ui.event(StepCompleted { step_id: "synthesize", name: "Synthesizing report" })

    return report
  }

  // === Helper Skills ===
  skill generate_search_queries(topic: string, depth: string) -> array<string> {
    let base_queries = [
      topic,
      `${topic} overview`,
      `${topic} latest research`,
      `${topic} key facts`
    ]

    if depth == "thorough" {
      base_queries.push(...[
        `${topic} controversy`,
        `${topic} expert opinions`,
        `${topic} statistics data`,
        `${topic} future trends`
      ])
    }

    return base_queries
  }

  skill analyze_content(content: string, topic: string) -> ContentAnalysis {
    // Use LLM to analyze content relevance and extract findings
    let analysis = await llm.analyze({
      content: content,
      task: `Extract key findings about "${topic}" from this content`,
      output_schema: ContentAnalysis
    })
    return analysis
  }

  skill synthesize_report(topic: string, findings: array<Finding>, sources: array<Source>) -> ResearchReport {
    // Deduplicate and rank findings
    let ranked_findings = findings
      .unique_by(f => f.claim)
      .sort_by(f => -f.confidence)

    // Generate comprehensive report
    return ResearchReport {
      topic: topic,
      executive_summary: await generate_summary(ranked_findings),
      key_findings: ranked_findings.take(10),
      sources: sources.map(s => { url: s.url, title: s.title }),
      generated_at: time.now(),
      confidence_score: ranked_findings.avg(f => f.confidence)
    }
  }

  // === Lifecycle Hooks ===
  on context_warning {
    log.warn("Context pressure - compressing research history")

    // Summarize older findings to save space
    if state.current_research != null {
      let summary = await summarize_findings(state.current_research.findings.take(10))
      state.current_research.findings = [
        Finding { claim: summary, evidence: [], source_urls: [], confidence: 0.8 },
        ...state.current_research.findings.skip(10)
      ]
    }
  }

  on error(error: AgentError) {
    match error.type {
      "McpError" => {
        log.error("MCP server error", { error: error.message })
        emit ag_ui.event(ErrorNotification {
          title: "Search Service Error",
          message: "Unable to access search. Retrying...",
          recoverable: true
        })
        return ErrorDecision.Retry(5000)
      }
      _ => return ErrorDecision.Continue
    }
  }
}
```

### C.2 Orchestrator Agent

An orchestrator that coordinates multiple specialized agents, demonstrating A2A protocol usage and parallel execution.

```sag
/// OrchestratorAgent - Coordinates multiple specialized agents for complex tasks
/// Demonstrates: A2A protocol, parallel orchestration, supervised delegation, result aggregation

agent OrchestratorAgent {
  description: "Meta-agent that coordinates specialized agents to accomplish complex, multi-faceted tasks"
  version: "1.0.0"

  model:
    id: "claude-sonnet-4-20250514"
    max_tokens: 8000
    temperature: 0.2

  // === Protocol Configuration ===
  protocols:
    a2a:
      enabled: true
      discovery:
        registries: ["https://agents.sagesyn.ai/registry"]
        cache_ttl_ms: 300000
      trust:
        accept_from: ["agent://sagesyn/*", "agent://verified/*"]
        trust_levels:
          high:
            permissions: ["file_access", "external_api", "delegation"]
          standard:
            permissions: ["read_only"]

    ag_ui:
      enabled: true
      streaming: true
      state_sync:
        enabled: true
        mode: "delta"
        exposed_state: ["task_status", "agent_progress"]

  // === State ===
  state:
    active_tasks: record<string, TaskStatus>
    agent_pool: array<AgentCard>
    delegation_history: array<DelegationRecord>

  type TaskStatus {
    id: string
    description: string
    status: "pending" | "in_progress" | "completed" | "failed"
    assigned_agents: array<string>
    progress: number
    result: optional<any>
    started_at: timestamp
    completed_at: optional<timestamp>
  }

  type DelegationRecord {
    task_id: string
    agent: string
    delegated_at: timestamp
    completed_at: optional<timestamp>
    success: boolean
    quality_score: optional<number>
  }

  // === Agent Discovery ===
  skill discover_agents(capabilities: array<string>) -> array<AgentCard> {
    description: "Discover agents with specific capabilities"

    let agents = await a2a.discover({
      capabilities: capabilities,
      min_rating: 4.0,
      max_latency_ms: 5000,
      include_metadata: true
    })

    // Cache discovered agents
    state.agent_pool = [
      ...state.agent_pool.filter(a => !agents.some(n => n.agent_id == a.agent_id)),
      ...agents
    ]

    return agents
  }

  skill select_best_agent(task: string, required_capabilities: array<string>) -> AgentCard {
    description: "Select the best agent for a specific task"

    // Check cache first
    let candidates = state.agent_pool.filter(a =>
      required_capabilities.every(cap => a.capabilities.contains(cap))
    )

    if candidates.is_empty {
      candidates = await discover_agents(required_capabilities)
    }

    // Rank by past performance
    let ranked = candidates.map(agent => {
      let history = state.delegation_history
        .filter(d => d.agent == agent.agent_id)
        .take_last(10)

      let avg_quality = history.is_empty
        ? 0.5
        : history.filter(d => d.quality_score != null).avg(d => d.quality_score)

      let success_rate = history.is_empty
        ? 0.5
        : history.filter(d => d.success).length / history.length

      return {
        agent: agent,
        score: (avg_quality * 0.6) + (success_rate * 0.3) + (agent.rating / 10 * 0.1)
      }
    })

    return ranked.max_by(r => r.score).agent
  }

  // === Task Orchestration ===
  skill orchestrate(task: ComplexTask) -> TaskResult {
    description: "Break down and orchestrate a complex multi-step task"

    let task_id = crypto.uuid()

    state.active_tasks[task_id] = TaskStatus {
      id: task_id,
      description: task.description,
      status: "in_progress",
      assigned_agents: [],
      progress: 0,
      result: null,
      started_at: time.now(),
      completed_at: null
    }

    emit ag_ui.event(RunStarted {
      run_id: task_id,
      agent_id: context.agent_id,
      metadata: { task_type: "orchestration" }
    })

    // Step 1: Decompose task
    emit ag_ui.event(StepStarted { step_id: "decompose", name: "Analyzing task structure" })

    let subtasks = await decompose_task(task)

    emit ag_ui.event(StepCompleted {
      step_id: "decompose",
      name: "Analyzing task structure",
      result: { subtask_count: subtasks.length }
    })

    // Step 2: Identify dependencies and create execution plan
    let execution_plan = create_execution_plan(subtasks)

    // Step 3: Execute plan
    let results: array<SubtaskResult> = []

    for phase in execution_plan.phases {
      emit ag_ui.event(StepStarted {
        step_id: `phase_${phase.index}`,
        name: `Executing phase ${phase.index + 1}/${execution_plan.phases.length}`
      })

      // Execute phase tasks in parallel
      let phase_results = await execute_phase(task_id, phase)
      results.push(...phase_results)

      // Update progress
      state.active_tasks[task_id].progress =
        (phase.index + 1) / execution_plan.phases.length * 100

      emit ag_ui.event(StateDelta {
        path: `task_status.${task_id}.progress`,
        operation: "set",
        value: state.active_tasks[task_id].progress
      })

      emit ag_ui.event(StepCompleted {
        step_id: `phase_${phase.index}`,
        name: `Executing phase ${phase.index + 1}/${execution_plan.phases.length}`,
        result: { completed: phase_results.filter(r => r.success).length }
      })
    }

    // Step 4: Aggregate results
    emit ag_ui.event(StepStarted { step_id: "aggregate", name: "Synthesizing results" })

    let final_result = await aggregate_results(task, results)

    state.active_tasks[task_id].status = "completed"
    state.active_tasks[task_id].result = final_result
    state.active_tasks[task_id].completed_at = time.now()

    emit ag_ui.event(StepCompleted { step_id: "aggregate", name: "Synthesizing results" })

    return final_result
  }

  skill execute_phase(task_id: string, phase: ExecutionPhase) -> array<SubtaskResult> {
    // Prepare delegation tasks
    let delegations = await tools.parallel(
      phase.subtasks.map(async subtask => {
        let agent = await select_best_agent(subtask.description, subtask.required_capabilities)

        state.active_tasks[task_id].assigned_agents.push(agent.agent_id)

        return {
          subtask: subtask,
          agent: agent
        }
      })
    )

    // Execute delegations with supervision
    let results = await a2a.parallel(
      delegations.map(d => a2a.delegate_supervised({
        agent: d.agent.url,
        task: d.subtask.description,
        input: d.subtask.input,
        context: {
          share_history: false,
          timeout_ms: d.subtask.timeout_ms ?? 120000,
          streaming: true
        },
        supervision: {
          check_interval_ms: 10000,
          quality_threshold: 0.7,
          max_iterations: 2,
          on_low_quality: "provide_feedback",
          quality_evaluator: (result) => evaluate_subtask_quality(d.subtask, result),
          feedback_generator: (result, score) => generate_improvement_feedback(d.subtask, result, score)
        }
      })),
      {
        max_concurrent: 5,
        fail_strategy: "partial",
        progress_callback: (progress) => {
          emit ag_ui.event(Progress {
            completed: progress.completed,
            total: progress.total,
            phase: phase.index
          })
        }
      }
    )

    // Record delegation results
    for (index, result) in results.enumerate() {
      let delegation = delegations[index]
      state.delegation_history.push(DelegationRecord {
        task_id: task_id,
        agent: delegation.agent.agent_id,
        delegated_at: time.now() - result.duration_ms,
        completed_at: time.now(),
        success: result.is_ok,
        quality_score: result.is_ok ? result.value.quality_score : null
      })
    }

    return results.map((r, i) => SubtaskResult {
      subtask_id: delegations[i].subtask.id,
      success: r.is_ok,
      output: r.is_ok ? r.value.output : null,
      error: r.is_err ? r.error : null
    })
  }

  skill aggregate_results(task: ComplexTask, results: array<SubtaskResult>) -> TaskResult {
    // Collect successful results
    let successful = results.filter(r => r.success)
    let failed = results.filter(r => !r.success)

    if failed.length > results.length * 0.3 {
      // Too many failures - request human intervention
      let approval = await ag_ui.approval_request({
        title: "Partial Task Failure",
        description: `${failed.length} of ${results.length} subtasks failed. Continue with partial results?`,
        details: {
          successful_count: successful.length,
          failed_count: failed.length,
          failed_tasks: failed.map(f => f.subtask_id)
        },
        actions: [
          { id: "continue", label: "Continue with partial results", style: "primary" },
          { id: "retry_failed", label: "Retry failed tasks", style: "secondary" },
          { id: "abort", label: "Abort task", style: "danger" }
        ],
        timeout_ms: 300000
      })

      match approval.action {
        "abort" => throw TaskAbortedError("User aborted due to failures"),
        "retry_failed" => {
          // Retry failed subtasks (simplified)
          log.info("Retrying failed subtasks")
        }
        _ => {}  // Continue with partial
      }
    }

    // Synthesize final result
    let aggregated = a2a.aggregate(successful.map(r => r.output), {
      strategy: Synthesize,
      synthesis_prompt: `Combine these results for task: ${task.description}`
    })

    return TaskResult {
      task_id: task.id,
      success: true,
      output: aggregated,
      subtask_results: results,
      completion_rate: successful.length / results.length,
      generated_at: time.now()
    }
  }

  // === Helper Functions ===
  skill decompose_task(task: ComplexTask) -> array<Subtask> {
    // Use LLM to break down the task
    let decomposition = await llm.analyze({
      task: `Break down this complex task into subtasks: ${task.description}`,
      context: { constraints: task.constraints, requirements: task.requirements },
      output_schema: TaskDecomposition
    })

    return decomposition.subtasks.map((s, i) => Subtask {
      id: `${task.id}_sub_${i}`,
      description: s.description,
      required_capabilities: s.capabilities,
      input: s.input,
      dependencies: s.depends_on,
      timeout_ms: s.estimated_duration_ms * 2
    })
  }

  fn create_execution_plan(subtasks: array<Subtask>) -> ExecutionPlan {
    // Topological sort based on dependencies
    let phases: array<ExecutionPhase> = []
    let completed: array<string> = []

    while completed.length < subtasks.length {
      let ready = subtasks.filter(s =>
        !completed.contains(s.id) &&
        s.dependencies.every(d => completed.contains(d))
      )

      if ready.is_empty {
        throw CyclicDependencyError("Circular dependency detected in subtasks")
      }

      phases.push(ExecutionPhase {
        index: phases.length,
        subtasks: ready
      })

      completed.push(...ready.map(s => s.id))
    }

    return ExecutionPlan { phases: phases }
  }

  // === Lifecycle ===
  on agent_start {
    // Pre-warm agent discovery
    log.info("Warming up agent discovery cache")
    await discover_agents(["research", "analysis", "writing", "coding"])
  }
}
```

### C.3 Interactive Assistant Agent

An interactive assistant that leverages AG-UI for rich user interactions, forms, and real-time state synchronization.

```sag
/// InteractiveAssistant - Rich interactive agent with UI components
/// Demonstrates: AG-UI protocol, interactive forms, streaming, state sync, approvals

agent InteractiveAssistant {
  description: "Interactive assistant with rich UI components for guided workflows"
  version: "1.0.0"

  model:
    id: "claude-sonnet-4-20250514"
    max_tokens: 8000
    temperature: 0.4

  // === AG-UI Configuration ===
  protocols:
    ag_ui:
      enabled: true
      streaming: true
      state_sync:
        enabled: true
        mode: "delta"
        debounce_ms: 100
        exposed_state: ["workflow", "form_data", "progress", "current_step"]
      components:
        forms: true
        approvals: true
        progress: true
        file_upload: true
        selections: true

    mcp:
      servers:
        - name: filesystem
          transport: stdio
          command: "npx"
          args: ["-y", "@anthropic/mcp-server-filesystem"]
          env:
            ALLOWED_DIRECTORIES: "${env.HOME}/Documents,${env.HOME}/Downloads"

  // === State ===
  state:
    workflow: optional<Workflow>
    form_data: record<string, any>
    progress: number
    current_step: optional<string>
    user_preferences: UserPreferences

  type Workflow {
    id: string
    name: string
    steps: array<WorkflowStep>
    current_step_index: number
    data: record<string, any>
    started_at: timestamp
  }

  type WorkflowStep {
    id: string
    name: string
    description: string
    type: "form" | "approval" | "processing" | "selection"
    config: any
    completed: boolean
    result: optional<any>
  }

  type UserPreferences {
    theme: "light" | "dark" | "system"
    notifications: boolean
    auto_save: boolean
    language: string
  }

  // === Interactive Workflows ===
  skill start_workflow(workflow_type: string) -> Workflow {
    description: "Start a guided workflow with interactive steps"

    let workflow = create_workflow(workflow_type)
    state.workflow = workflow
    state.progress = 0
    state.current_step = workflow.steps[0].id

    emit ag_ui.event(RunStarted {
      run_id: workflow.id,
      agent_id: context.agent_id,
      metadata: { workflow_type: workflow_type }
    })

    // Sync initial state
    emit ag_ui.event(StateSnapshot { state: {
      workflow: workflow,
      progress: 0,
      current_step: workflow.steps[0].id
    }})

    // Execute workflow
    await execute_workflow(workflow)

    return workflow
  }

  skill execute_workflow(workflow: Workflow) {
    for (index, step) in workflow.steps.enumerate() {
      state.current_step = step.id
      state.progress = (index / workflow.steps.length) * 100

      emit ag_ui.event(StepStarted {
        step_id: step.id,
        name: step.name,
        metadata: { description: step.description }
      })

      emit ag_ui.event(StateDelta {
        path: "current_step",
        operation: "set",
        value: step.id
      })

      let result = match step.type {
        "form" => await execute_form_step(step),
        "approval" => await execute_approval_step(step),
        "processing" => await execute_processing_step(step),
        "selection" => await execute_selection_step(step)
      }

      workflow.steps[index].completed = true
      workflow.steps[index].result = result
      workflow.data[step.id] = result

      emit ag_ui.event(StepCompleted {
        step_id: step.id,
        name: step.name,
        result: result
      })
    }

    state.progress = 100
    emit ag_ui.event(StateDelta {
      path: "progress",
      operation: "set",
      value: 100
    })
  }

  // === Step Executors ===
  skill execute_form_step(step: WorkflowStep) -> record<string, any> {
    let config = step.config as FormConfig

    // Build form fields with validation
    let fields = config.fields.map(f => {
      let field: FormField = {
        name: f.name,
        type: f.type,
        label: f.label,
        required: f.required ?? false,
        placeholder: f.placeholder,
        default_value: f.default_value
      }

      // Add validation rules
      if f.validation != null {
        field.validation = f.validation
      }

      // Add options for select/radio/checkbox
      if f.options != null {
        field.options = f.options
      }

      return field
    })

    // Display form and wait for submission
    let form_result = await ag_ui.form({
      title: step.name,
      description: step.description,
      fields: fields,
      submit_label: config.submit_label ?? "Continue",
      cancel_label: config.cancel_label ?? "Cancel",
      validation_mode: "on_submit"  // or "on_change"
    })

    if form_result.cancelled {
      throw WorkflowCancelledError(`User cancelled at step: ${step.name}`)
    }

    // Store form data
    state.form_data = { ...state.form_data, ...form_result.data }

    return form_result.data
  }

  skill execute_approval_step(step: WorkflowStep) -> ApprovalResult {
    let config = step.config as ApprovalConfig

    // Build approval request
    let approval = await ag_ui.approval_request({
      title: step.name,
      description: step.description,
      details: config.details,
      actions: config.actions ?? [
        { id: "approve", label: "Approve", style: "primary" },
        { id: "reject", label: "Reject", style: "danger" }
      ],
      timeout_ms: config.timeout_ms ?? 300000,  // 5 minutes default
      require_reason: config.require_reason ?? false
    })

    return approval
  }

  skill execute_processing_step(step: WorkflowStep) -> any {
    let config = step.config as ProcessingConfig

    // Create progress stream
    let stream = ag_ui.create_stream(`${step.id}_progress`)

    // Execute processing with progress updates
    let result = await process_with_progress(config, (progress, message) => {
      stream.write(`${message}\n`)
      emit ag_ui.event(Progress {
        step_id: step.id,
        percent: progress,
        message: message
      })
    })

    stream.end()
    return result
  }

  skill execute_selection_step(step: WorkflowStep) -> SelectionResult {
    let config = step.config as SelectionConfig

    // Display selection UI
    let selection = await ag_ui.selection({
      title: step.name,
      description: step.description,
      mode: config.mode,  // "single" | "multiple"
      options: config.options,
      search_enabled: config.searchable ?? false,
      min_selections: config.min_selections ?? (config.mode == "multiple" ? 1 : null),
      max_selections: config.max_selections
    })

    return selection
  }

  // === File Operations ===
  skill upload_file() -> UploadedFile {
    description: "Allow user to upload a file"

    let upload = await ag_ui.file_upload({
      title: "Upload File",
      description: "Select a file to upload",
      accept: [".pdf", ".doc", ".docx", ".txt", ".csv"],
      max_size_bytes: 10 * 1024 * 1024,  // 10MB
      multiple: false
    })

    if upload.cancelled {
      throw UploadCancelledError("User cancelled upload")
    }

    // Process uploaded file
    let file = upload.files[0]
    log.info("File uploaded", { name: file.name, size: file.size })

    return file
  }

  // === Preference Management ===
  skill update_preferences() -> UserPreferences {
    description: "Update user preferences via form"

    let result = await ag_ui.form({
      title: "Preferences",
      description: "Customize your experience",
      fields: [
        {
          name: "theme",
          type: "select",
          label: "Theme",
          options: [
            { value: "light", label: "Light" },
            { value: "dark", label: "Dark" },
            { value: "system", label: "System" }
          ],
          default_value: state.user_preferences.theme
        },
        {
          name: "notifications",
          type: "checkbox",
          label: "Enable notifications",
          default_value: state.user_preferences.notifications
        },
        {
          name: "auto_save",
          type: "checkbox",
          label: "Auto-save progress",
          default_value: state.user_preferences.auto_save
        },
        {
          name: "language",
          type: "select",
          label: "Language",
          options: [
            { value: "en", label: "English" },
            { value: "es", label: "Spanish" },
            { value: "fr", label: "French" },
            { value: "de", label: "German" }
          ],
          default_value: state.user_preferences.language
        }
      ]
    })

    if !result.cancelled {
      state.user_preferences = result.data as UserPreferences
      emit ag_ui.event(StateDelta {
        path: "user_preferences",
        operation: "set",
        value: state.user_preferences
      })
    }

    return state.user_preferences
  }

  // === Streaming Responses ===
  skill stream_response(content: string) {
    description: "Stream a long response to the user"

    let stream = ag_ui.create_stream("response")
    let message_id = crypto.uuid()

    emit ag_ui.event(TextMessageStart { message_id: message_id })

    // Simulate streaming by chunking content
    let chunks = content.split_into_chunks(50)  // 50 chars per chunk

    for chunk in chunks {
      stream.write(chunk)
      emit ag_ui.event(TextMessageContent {
        message_id: message_id,
        delta: chunk
      })
      await time.sleep(50)  // Small delay for effect
    }

    stream.end()
    emit ag_ui.event(TextMessageEnd { message_id: message_id })
  }

  // === Workflow Helpers ===
  fn create_workflow(workflow_type: string) -> Workflow {
    let steps = match workflow_type {
      "onboarding" => [
        WorkflowStep {
          id: "welcome",
          name: "Welcome",
          description: "Let's get you set up",
          type: "form",
          config: {
            fields: [
              { name: "name", type: "text", label: "Your Name", required: true },
              { name: "email", type: "email", label: "Email", required: true }
            ]
          },
          completed: false,
          result: null
        },
        WorkflowStep {
          id: "preferences",
          name: "Preferences",
          description: "Customize your experience",
          type: "selection",
          config: {
            mode: "multiple",
            options: [
              { value: "research", label: "Research & Analysis" },
              { value: "writing", label: "Writing & Content" },
              { value: "coding", label: "Coding & Development" },
              { value: "data", label: "Data Processing" }
            ]
          },
          completed: false,
          result: null
        },
        WorkflowStep {
          id: "confirm",
          name: "Confirmation",
          description: "Review and confirm your settings",
          type: "approval",
          config: {
            details: "Please confirm your setup",
            actions: [
              { id: "confirm", label: "Confirm & Start", style: "primary" }
            ]
          },
          completed: false,
          result: null
        }
      ],
      _ => throw UnknownWorkflowError(`Unknown workflow type: ${workflow_type}`)
    }

    return Workflow {
      id: crypto.uuid(),
      name: workflow_type,
      steps: steps,
      current_step_index: 0,
      data: {},
      started_at: time.now()
    }
  }

  // === State Hooks ===
  on state_change(change: StateChange) {
    if change.path.starts_with("user_preferences") && state.user_preferences.auto_save {
      // Auto-save preferences
      await storage.save("user_preferences", state.user_preferences)
    }
  }

  on agent_start {
    // Load saved preferences
    let saved = await storage.load("user_preferences")
    if saved != null {
      state.user_preferences = saved
    } else {
      state.user_preferences = UserPreferences {
        theme: "system",
        notifications: true,
        auto_save: true,
        language: "en"
      }
    }
  }
}
```

### C.4 Tool Pipeline Agent

An agent that demonstrates advanced tool execution patterns including pipelines, parallel execution, caching, and circuit breakers.

```sag
/// ToolPipelineAgent - Advanced tool execution patterns
/// Demonstrates: Tool pipelines, parallel execution, caching, circuit breakers, error handling

agent ToolPipelineAgent {
  description: "Data processing agent demonstrating advanced tool execution patterns"
  version: "1.0.0"

  model:
    id: "claude-sonnet-4-20250514"
    max_tokens: 8000
    temperature: 0.1

  // === Context Configuration ===
  context:
    budget:
      reserved_system: 1500
      reserved_output: 4000
      warning_threshold: 0.8
    overflow_strategy: "truncate"

  // === MCP Servers ===
  protocols:
    mcp:
      servers:
        - name: database
          transport: stdio
          command: "npx"
          args: ["-y", "@sagesyn/mcp-server-postgres"]
          env:
            DATABASE_URL: "${env.DATABASE_URL}"
          lifecycle:
            health_check_interval_ms: 30000
            restart_on_failure: true
            max_restarts: 3

        - name: api_gateway
          transport: sse
          url: "${env.API_GATEWAY_URL}"
          headers:
            Authorization: "Bearer ${env.API_KEY}"

    ag_ui:
      enabled: true
      streaming: true

  // === State ===
  state:
    cache: ToolCache
    circuit_breakers: record<string, CircuitBreaker>
    execution_metrics: ExecutionMetrics

  type ToolCache {
    entries: record<string, CacheEntry>
    max_size: number
    current_size: number
  }

  type CacheEntry {
    value: any
    created_at: timestamp
    ttl_ms: number
    hits: number
    stale: boolean
  }

  type CircuitBreaker {
    state: "closed" | "open" | "half_open"
    failure_count: number
    last_failure_at: optional<timestamp>
    reset_timeout_ms: number
  }

  type ExecutionMetrics {
    total_calls: number
    cache_hits: number
    cache_misses: number
    circuit_opens: number
    total_latency_ms: number
    error_count: number
  }

  // === Tool Definitions with Advanced Patterns ===
  tool fetch_from_db(query: string, params: array<any> = []) -> Result<QueryResult, DbError> {
    description: "Execute a database query with connection pooling and retry"
    timeout: 30000
    retry:
      max_attempts: 3
      backoff: exponential
      retry_on: ["ConnectionError", "TimeoutError"]
    circuit_breaker:
      name: "database"
      failure_threshold: 5
      reset_timeout_ms: 60000

    // Check circuit breaker
    let cb = state.circuit_breakers["database"]
    if cb != null && cb.state == "open" {
      if time.now() - cb.last_failure_at < cb.reset_timeout_ms {
        return Err(CircuitOpenError("Database circuit breaker is open"))
      }
      // Try half-open
      state.circuit_breakers["database"].state = "half_open"
    }

    let start = time.now()
    let result = await mcp.call_tool("database", "query", { sql: query, params: params })
    let duration = time.now() - start

    // Update metrics
    state.execution_metrics.total_calls += 1
    state.execution_metrics.total_latency_ms += duration

    // Reset circuit breaker on success
    if result.is_ok && cb != null {
      state.circuit_breakers["database"] = CircuitBreaker {
        state: "closed",
        failure_count: 0,
        last_failure_at: null,
        reset_timeout_ms: 60000
      }
    }

    return result
  }

  tool call_external_api(endpoint: string, method: string = "GET", body: optional<any> = null) -> Result<ApiResponse, ApiError> {
    description: "Call external API with caching and rate limiting"
    timeout: 15000
    retry:
      max_attempts: 2
      backoff: linear
    cache:
      enabled: true
      ttl: 300000  // 5 minutes
      stale_while_revalidate: true
      key: (endpoint, method, body) => crypto.hash(`${method}:${endpoint}:${json.stringify(body)}`)
    rate_limit:
      requests_per_second: 10
      burst: 20

    let cache_key = crypto.hash(`${method}:${endpoint}:${json.stringify(body)}`)

    // Check cache
    let cached = state.cache.entries[cache_key]
    if cached != null {
      let age = time.now() - cached.created_at
      if age < cached.ttl_ms {
        state.execution_metrics.cache_hits += 1
        cached.hits += 1
        return Ok(cached.value)
      } else if cached.stale {
        // Return stale data while revalidating
        spawn revalidate_cache(cache_key, endpoint, method, body)
        return Ok(cached.value)
      }
    }
    state.execution_metrics.cache_misses += 1

    // Make API call
    let result = await mcp.call_tool("api_gateway", "http_request", {
      url: endpoint,
      method: method,
      body: body
    })

    // Cache successful result
    if result.is_ok {
      update_cache(cache_key, result.value, 300000)
    }

    return result
  }

  tool transform_data(data: any, transformations: array<Transformation>) -> Result<any, TransformError> {
    description: "Apply a series of data transformations"

    let current = data

    for transform in transformations {
      current = match transform.type {
        "map" => current.map(transform.fn),
        "filter" => current.filter(transform.fn),
        "reduce" => current.reduce(transform.fn, transform.initial),
        "sort" => current.sort_by(transform.key),
        "group" => current.group_by(transform.key),
        "flatten" => current.flat(transform.depth ?? 1),
        "unique" => current.unique_by(transform.key ?? (x => x)),
        _ => throw UnknownTransformError(`Unknown transformation: ${transform.type}`)
      }
    }

    return Ok(current)
  }

  tool validate_data(data: any, schema: JsonSchema) -> Result<ValidationResult, ValidationError> {
    description: "Validate data against a JSON schema"

    let errors: array<ValidationIssue> = []

    // Recursive validation
    fn validate_value(value: any, schema: JsonSchema, path: string) {
      // Type check
      if schema.type != null {
        let actual_type = typeof(value)
        if actual_type != schema.type && !(schema.type == "integer" && actual_type == "number" && value % 1 == 0) {
          errors.push(ValidationIssue {
            path: path,
            message: `Expected ${schema.type}, got ${actual_type}`,
            severity: "error"
          })
        }
      }

      // Required properties
      if schema.required != null && typeof(value) == "object" {
        for req in schema.required {
          if value[req] == null {
            errors.push(ValidationIssue {
              path: `${path}.${req}`,
              message: `Missing required property: ${req}`,
              severity: "error"
            })
          }
        }
      }

      // Nested properties
      if schema.properties != null && typeof(value) == "object" {
        for (key, prop_schema) in schema.properties {
          if value[key] != null {
            validate_value(value[key], prop_schema, `${path}.${key}`)
          }
        }
      }

      // Array items
      if schema.items != null && typeof(value) == "array" {
        for (index, item) in value.enumerate() {
          validate_value(item, schema.items, `${path}[${index}]`)
        }
      }

      // String constraints
      if typeof(value) == "string" {
        if schema.minLength != null && value.length < schema.minLength {
          errors.push(ValidationIssue {
            path: path,
            message: `String too short (min: ${schema.minLength})`,
            severity: "error"
          })
        }
        if schema.pattern != null && !regex.match(schema.pattern, value) {
          errors.push(ValidationIssue {
            path: path,
            message: `String does not match pattern: ${schema.pattern}`,
            severity: "error"
          })
        }
      }

      // Number constraints
      if typeof(value) == "number" {
        if schema.minimum != null && value < schema.minimum {
          errors.push(ValidationIssue {
            path: path,
            message: `Number below minimum (${schema.minimum})`,
            severity: "error"
          })
        }
        if schema.maximum != null && value > schema.maximum {
          errors.push(ValidationIssue {
            path: path,
            message: `Number above maximum (${schema.maximum})`,
            severity: "error"
          })
        }
      }
    }

    validate_value(data, schema, "$")

    return Ok(ValidationResult {
      valid: errors.filter(e => e.severity == "error").is_empty,
      errors: errors.filter(e => e.severity == "error"),
      warnings: errors.filter(e => e.severity == "warning")
    })
  }

  // === Pipeline Skills ===
  skill process_data_pipeline(source: DataSource, transformations: array<Transformation>, target: DataTarget) -> PipelineResult {
    description: "Execute a complete data processing pipeline"

    let pipeline_id = crypto.uuid()
    let stream = ag_ui.create_stream("pipeline_progress")

    emit ag_ui.event(RunStarted {
      run_id: pipeline_id,
      agent_id: context.agent_id,
      metadata: { type: "data_pipeline" }
    })

    // Stage 1: Extract
    stream.write("Stage 1/4: Extracting data...\n")
    emit ag_ui.event(StepStarted { step_id: "extract", name: "Data Extraction" })

    let extract_result = await tools.pipeline([
      () => fetch_source_data(source),
      (data) => validate_data(data, source.schema)
    ], null)

    if extract_result.is_err {
      emit ag_ui.event(StepFailed {
        step_id: "extract",
        name: "Data Extraction",
        error: extract_result.error.message
      })
      return PipelineResult { success: false, error: extract_result.error }
    }

    emit ag_ui.event(StepCompleted {
      step_id: "extract",
      name: "Data Extraction",
      result: { record_count: extract_result.value.length }
    })

    // Stage 2: Transform
    stream.write("Stage 2/4: Transforming data...\n")
    emit ag_ui.event(StepStarted { step_id: "transform", name: "Data Transformation" })

    let transform_result = await transform_data(extract_result.value, transformations)

    if transform_result.is_err {
      emit ag_ui.event(StepFailed {
        step_id: "transform",
        name: "Data Transformation",
        error: transform_result.error.message
      })
      return PipelineResult { success: false, error: transform_result.error }
    }

    emit ag_ui.event(StepCompleted {
      step_id: "transform",
      name: "Data Transformation",
      result: { output_count: transform_result.value.length }
    })

    // Stage 3: Validate
    stream.write("Stage 3/4: Validating output...\n")
    emit ag_ui.event(StepStarted { step_id: "validate", name: "Output Validation" })

    let validation_result = await validate_data(transform_result.value, target.schema)

    if validation_result.is_err || !validation_result.value.valid {
      emit ag_ui.event(StepFailed {
        step_id: "validate",
        name: "Output Validation",
        error: validation_result.is_err
          ? validation_result.error.message
          : `Validation failed: ${validation_result.value.errors.length} errors`
      })

      // Request approval to proceed despite validation errors
      let approval = await ag_ui.approval_request({
        title: "Validation Errors Detected",
        description: `Found ${validation_result.value.errors.length} validation errors. Proceed anyway?`,
        details: { errors: validation_result.value.errors.take(5) },
        actions: [
          { id: "proceed", label: "Proceed Anyway", style: "warning" },
          { id: "abort", label: "Abort Pipeline", style: "danger" }
        ]
      })

      if approval.action == "abort" {
        return PipelineResult {
          success: false,
          error: ValidationError("Aborted due to validation errors")
        }
      }
    }

    emit ag_ui.event(StepCompleted {
      step_id: "validate",
      name: "Output Validation",
      result: { valid: validation_result.value.valid }
    })

    // Stage 4: Load
    stream.write("Stage 4/4: Loading data to target...\n")
    emit ag_ui.event(StepStarted { step_id: "load", name: "Data Loading" })

    let load_result = await load_to_target(target, transform_result.value)

    if load_result.is_err {
      emit ag_ui.event(StepFailed {
        step_id: "load",
        name: "Data Loading",
        error: load_result.error.message
      })
      return PipelineResult { success: false, error: load_result.error }
    }

    emit ag_ui.event(StepCompleted {
      step_id: "load",
      name: "Data Loading",
      result: { records_loaded: load_result.value.count }
    })

    stream.write("Pipeline completed successfully!\n")
    stream.end()

    return PipelineResult {
      success: true,
      pipeline_id: pipeline_id,
      input_count: extract_result.value.length,
      output_count: transform_result.value.length,
      duration_ms: time.now() - pipeline_start,
      metrics: state.execution_metrics
    }
  }

  skill parallel_fetch(sources: array<DataSource>) -> array<Result<any, FetchError>> {
    description: "Fetch from multiple sources in parallel with failure isolation"

    let results = await tools.parallel(
      sources.map(source => fetch_source_data(source)),
      {
        max_concurrent: 5,
        timeout: 30000,
        fail_fast: false  // Continue even if some fail
      }
    )

    // Log failures but return all results
    let failures = results.filter(r => r.is_err)
    if failures.length > 0 {
      log.warn("Some parallel fetches failed", {
        total: sources.length,
        failed: failures.length,
        errors: failures.map(f => f.error.message)
      })
    }

    return results
  }

  skill race_fetch(sources: array<DataSource>) -> Result<any, FetchError> {
    description: "Fetch from first responding source"

    let result = await tools.race(
      sources.map(source => fetch_source_data(source)),
      { timeout: 10000 }
    )

    return result
  }

  // === Helper Functions ===
  skill fetch_source_data(source: DataSource) -> Result<any, FetchError> {
    match source.type {
      "database" => await fetch_from_db(source.query, source.params ?? []),
      "api" => await call_external_api(source.endpoint, source.method ?? "GET", source.body),
      "file" => await mcp.call_tool("filesystem", "read_file", { path: source.path }),
      _ => Err(UnknownSourceError(`Unknown source type: ${source.type}`))
    }
  }

  skill load_to_target(target: DataTarget, data: any) -> Result<LoadResult, LoadError> {
    match target.type {
      "database" => {
        // Batch insert
        let batches = data.chunk(100)
        let total = 0
        for batch in batches {
          let result = await fetch_from_db(target.insert_query, [batch])
          if result.is_err {
            return Err(result.error)
          }
          total += batch.length
        }
        return Ok(LoadResult { count: total })
      }
      "api" => await call_external_api(target.endpoint, "POST", data),
      "file" => await mcp.call_tool("filesystem", "write_file", {
        path: target.path,
        content: json.stringify(data, indent: 2)
      }),
      _ => Err(UnknownTargetError(`Unknown target type: ${target.type}`))
    }
  }

  fn update_cache(key: string, value: any, ttl_ms: number) {
    // Evict if cache is full (LRU)
    if state.cache.current_size >= state.cache.max_size {
      let oldest = state.cache.entries
        .entries()
        .min_by((k, v) => v.created_at)

      if oldest != null {
        state.cache.entries.delete(oldest[0])
        state.cache.current_size -= 1
      }
    }

    state.cache.entries[key] = CacheEntry {
      value: value,
      created_at: time.now(),
      ttl_ms: ttl_ms,
      hits: 0,
      stale: false
    }
    state.cache.current_size += 1
  }

  async fn revalidate_cache(key: string, endpoint: string, method: string, body: any) {
    let result = await mcp.call_tool("api_gateway", "http_request", {
      url: endpoint,
      method: method,
      body: body
    })

    if result.is_ok {
      update_cache(key, result.value, 300000)
    }
  }

  // === Lifecycle ===
  on agent_start {
    state.cache = ToolCache {
      entries: {},
      max_size: 1000,
      current_size: 0
    }
    state.circuit_breakers = {}
    state.execution_metrics = ExecutionMetrics {
      total_calls: 0,
      cache_hits: 0,
      cache_misses: 0,
      circuit_opens: 0,
      total_latency_ms: 0,
      error_count: 0
    }
  }

  on error(error: AgentError) {
    state.execution_metrics.error_count += 1

    // Update circuit breaker on failures
    if error.type == "DbError" {
      let cb = state.circuit_breakers["database"] ?? CircuitBreaker {
        state: "closed",
        failure_count: 0,
        last_failure_at: null,
        reset_timeout_ms: 60000
      }
      cb.failure_count += 1
      cb.last_failure_at = time.now()

      if cb.failure_count >= 5 {
        cb.state = "open"
        state.execution_metrics.circuit_opens += 1
        log.error("Circuit breaker opened for database")
      }

      state.circuit_breakers["database"] = cb
    }

    return ErrorDecision.Continue
  }

  on turn_end {
    // Log metrics periodically
    if context.turn_count % 10 == 0 {
      log.info("Tool execution metrics", {
        cache_hit_rate: state.execution_metrics.cache_hits /
          (state.execution_metrics.cache_hits + state.execution_metrics.cache_misses),
        avg_latency_ms: state.execution_metrics.total_latency_ms / state.execution_metrics.total_calls,
        error_rate: state.execution_metrics.error_count / state.execution_metrics.total_calls
      })
    }
  }
}
```

---

## Revision History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | Dec 2025 | Initial draft |

