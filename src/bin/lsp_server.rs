use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use lynxql::{parse_program, parse_program_resilient, typecheck_program_with_details, TypeCheckError};

#[derive(Debug)]
struct LynxLanguageServer {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, String>>>,
}

impl LynxLanguageServer {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    async fn validate_document(&self, uri: &Url, text: &str) {
        let mut diagnostics = Vec::new();

        // Parse the document with resilient parser
        let parse_result = parse_program_resilient(text);
        
        // Add parse errors as diagnostics
        for parse_error in &parse_result.errors {
            let diagnostic = Diagnostic {
                range: Range {
                    start: Position::new(parse_error.line, parse_error.column),
                    end: Position::new(parse_error.line, parse_error.column + 10), // Show a reasonable range
                },
                severity: Some(DiagnosticSeverity::WARNING),
                code: Some(NumberOrString::String("parse_error".to_string())),
                source: Some("lynx".to_string()),
                message: parse_error.message.clone(),
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            };
            diagnostics.push(diagnostic);
        }
        
        // Type check the parsed program (even if there were parse errors)
        match typecheck_program_with_details(&parse_result.program) {
            Ok(_env) => {
                // No type errors - only parse errors (if any) are in diagnostics
            }
            Err(errors) => {
                // Convert type errors to LSP diagnostics
                for error in errors {
                    let diagnostic = self.error_to_diagnostic(&error, text);
                    diagnostics.push(diagnostic);
                }
            }
        }

        // Send diagnostics to the client
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    fn error_to_diagnostic(&self, error: &TypeCheckError, text: &str) -> Diagnostic {
        let (severity, code, message, search_term) = match error {
            TypeCheckError::TypeError(msg) => (DiagnosticSeverity::ERROR, "type_error", msg.clone(), None),
            TypeCheckError::UndefinedType(name) => (
                DiagnosticSeverity::ERROR,
                "undefined_type",
                format!("Undefined type: {}", name),
                Some(name.clone()),
            ),
            TypeCheckError::UndefinedVariable(name) => (
                DiagnosticSeverity::ERROR,
                "undefined_variable",
                format!("Undefined variable: {}", name),
                Some(name.clone()),
            ),
            TypeCheckError::UndefinedEnum(name) => (
                DiagnosticSeverity::ERROR,
                "undefined_enum",
                format!("Undefined enum: {}", name),
                Some(name.clone()),
            ),
            TypeCheckError::UndefinedEnumVariant(enum_name, variant) => (
                DiagnosticSeverity::ERROR,
                "undefined_enum_variant",
                format!("Undefined enum variant: {}::{}", enum_name, variant),
                Some(format!("{}.{}", enum_name, variant)),
            ),
            TypeCheckError::FieldNotFound(field, _type_name) => (
                DiagnosticSeverity::ERROR,
                "field_not_found",
                format!("Field '{}' not found in type '{}'", field, _type_name),
                Some(field.clone()),
            ),
            TypeCheckError::TypeMismatch { expected, found } => {
                // Try to find a search term for common type mismatches
                let search_term = if found == "string" && expected == "int" {
                    // Look for string literals like "large"
                    self.find_string_literal_in_text(text)
                } else {
                    None
                };
                (
                    DiagnosticSeverity::ERROR,
                    "type_mismatch",
                    format!("Type mismatch: expected {}, found {}", expected, found),
                    search_term,
                )
            },
            TypeCheckError::InvalidFieldAssignment { field, type_name } => (
                DiagnosticSeverity::ERROR,
                "invalid_field_assignment",
                format!("Invalid field assignment: {} in {}", field, type_name),
                Some(field.clone()),
            ),
            TypeCheckError::MissingRequiredField { field, type_name } => (
                DiagnosticSeverity::ERROR,
                "missing_required_field",
                format!("Missing required field: {} in {}", field, type_name),
                Some(field.clone()),
            ),
            TypeCheckError::InvalidLogicExpression(msg) => (
                DiagnosticSeverity::ERROR,
                "invalid_logic_expression",
                format!("Invalid logic expression: {}", msg),
                None,
            ),
            TypeCheckError::InvalidBuiltinCall(name) => (
                DiagnosticSeverity::ERROR,
                "invalid_builtin_call",
                format!("Invalid builtin call: {}", name),
                Some(name.clone()),
            ),
            TypeCheckError::InvalidLambda(msg) => (
                DiagnosticSeverity::ERROR,
                "invalid_lambda",
                format!("Invalid lambda expression: {}", msg),
                None,
            ),
            TypeCheckError::InvalidSolveCall(msg) => (
                DiagnosticSeverity::ERROR,
                "invalid_solve_call",
                format!("Invalid solve call: {}", msg),
                None,
            ),
        };

        // Try to find the position of the error by searching for the search term
        let range = if let Some(term) = search_term {
            self.find_term_position(text, &term)
        } else {
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, u32::MAX),
            }
        };

        Diagnostic {
            range,
            severity: Some(severity),
            code: Some(NumberOrString::String(code.to_string())),
            source: Some("lynx".to_string()),
            message,
            related_information: None,
            tags: None,
            code_description: None,
            data: None,
        }
    }

    fn find_term_position(&self, text: &str, term: &str) -> Range {
        // Search for the term in the text and return its position
        let lines: Vec<&str> = text.lines().collect();
        
        for (line_idx, line) in lines.iter().enumerate() {
            if let Some(col_idx) = line.find(term) {
                return Range {
                    start: Position::new(line_idx as u32, col_idx as u32),
                    end: Position::new(line_idx as u32, (col_idx + term.len()) as u32),
                };
            }
        }
        
        // Fallback to line 0 if not found
        Range {
            start: Position::new(0, 0),
            end: Position::new(0, u32::MAX),
        }
    }

    fn find_string_literal_in_text(&self, text: &str) -> Option<String> {
        // Look for string literals in quotes
        let lines: Vec<&str> = text.lines().collect();
        
        for line in lines {
            // Simple regex-like search for quoted strings
            if let Some(start) = line.find('"') {
                if let Some(end) = line[start + 1..].find('"') {
                    let string_literal = &line[start..start + 1 + end + 1];
                    return Some(string_literal.to_string());
                }
            }
        }
        
        None
    }

    async fn get_hover_info(&self, _uri: &Url, text: &str, position: Position) -> Option<Hover> {
        // Parse with resilient parser
        let parse_result = parse_program_resilient(text);
        
        match typecheck_program_with_details(&parse_result.program) {
            Ok(env) => {
                // Provide hover information even if there were parse errors
                let hover_text = if parse_result.errors.is_empty() {
                    format!(
                        "**Lynx Program**\n\n- Types: {}\n- Enums: {}\n- Variables: {}\n- Instances: {}",
                        env.types.len(),
                        env.enums.len(),
                        env.variables.len(),
                        env.instances.len()
                    )
                } else {
                    format!(
                        "**Lynx Program** (with parse warnings)\n\n- Types: {}\n- Enums: {}\n- Variables: {}\n- Instances: {}\n- Parse warnings: {}",
                        env.types.len(),
                        env.enums.len(),
                        env.variables.len(),
                        env.instances.len(),
                        parse_result.errors.len()
                    )
                };

                Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: hover_text,
                    }),
                    range: Some(Range {
                        start: position,
                        end: position,
                    }),
                })
            }
            Err(_) => None,
        }
    }

    async fn get_completions(&self, _uri: &Url, text: &str, _position: Position) -> Option<CompletionResponse> {
        // Parse with resilient parser
        let parse_result = parse_program_resilient(text);
        
        match typecheck_program_with_details(&parse_result.program) {
            Ok(env) => {
                let mut items = Vec::new();

                // Add primitive types
                for primitive in ["int", "float", "string", "bool"] {
                    items.push(CompletionItem {
                        label: primitive.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        detail: Some("Primitive type".to_string()),
                        documentation: Some(Documentation::String(format!("Lynx primitive type: {}", primitive))),
                        ..Default::default()
                    });
                }

                        // Add logic types
                        for logic_type in ["All", "Any", "Not", "AtLeast", "AtMost", "Exactly"] {
                            items.push(CompletionItem {
                                label: logic_type.to_string(),
                                kind: Some(CompletionItemKind::KEYWORD),
                                detail: Some("Logic type".to_string()),
                                documentation: Some(Documentation::String(format!("Lynx logic type: {}", logic_type))),
                                ..Default::default()
                            });
                        }

                        // Add declared types
                        for (name, type_decl) in &env.types {
                            items.push(CompletionItem {
                                label: name.clone(),
                                kind: Some(CompletionItemKind::CLASS),
                                detail: Some(format!("Type: {}", type_decl.base_type.to_string())),
                                documentation: Some(Documentation::String(format!(
                                    "User-defined type with {} fields",
                                    type_decl.fields.len()
                                ))),
                                ..Default::default()
                            });
                        }

                        // Add declared enums
                        for (name, enum_decl) in &env.enums {
                            items.push(CompletionItem {
                                label: name.clone(),
                                kind: Some(CompletionItemKind::ENUM),
                                detail: Some("Enum".to_string()),
                                documentation: Some(Documentation::String(format!(
                                    "Enum with {} variants: {}",
                                    enum_decl.variants.len(),
                                    enum_decl.variants.iter().map(|v| v.name.as_str()).collect::<Vec<_>>().join(", ")
                                ))),
                                ..Default::default()
                            });

                            // Add enum variants
                            for variant in &enum_decl.variants {
                                items.push(CompletionItem {
                                    label: format!("{}.{}", name, variant.name),
                                    kind: Some(CompletionItemKind::ENUM_MEMBER),
                                    detail: Some(format!("Enum variant of {}", name)),
                                    documentation: Some(Documentation::String(format!(
                                        "Variant '{}' of enum '{}'{}",
                                        variant.name,
                                        name,
                                        variant.value.map(|v| format!(" (value: {})", v)).unwrap_or_default()
                                    ))),
                                    ..Default::default()
                                });
                            }
                        }

                        // Add instances
                        for (name, instance_type) in &env.instances {
                            items.push(CompletionItem {
                                label: name.clone(),
                                kind: Some(CompletionItemKind::VARIABLE),
                                detail: Some(format!("Instance: {}", instance_type.to_string())),
                                documentation: Some(Documentation::String(format!(
                                    "Instance of type {}",
                                    instance_type.to_string()
                                ))),
                                ..Default::default()
                            });
                        }

                        // Add builtin functions
                        for builtin in ["solve", "find", "sum", "first", "match", "propagate"] {
                            items.push(CompletionItem {
                                label: builtin.to_string(),
                                kind: Some(CompletionItemKind::FUNCTION),
                                detail: Some("Builtin function".to_string()),
                                documentation: Some(Documentation::String(format!("Lynx builtin function: {}", builtin))),
                                ..Default::default()
                            });
                        }

                Some(CompletionResponse::Array(items))
            }
            Err(_) => None,
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for LynxLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Lynx Language Server".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: Default::default(),
                    completion_item: None,
                }),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("lynx".to_string()),
                        inter_file_dependencies: true,
                        workspace_diagnostics: false,
                        work_done_progress_options: Default::default(),
                    },
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Lynx Language Server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;

        // Store the document
        self.documents.write().await.insert(uri.clone(), text.clone());

        // Validate the document
        self.validate_document(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        
        // For full sync, we get the entire document content
        if let Some(change) = params.content_changes.into_iter().next() {
            let text = change.text;
            
            // Update stored document
            self.documents.write().await.insert(uri.clone(), text.clone());
            
            // Validate the document
            self.validate_document(&uri, &text).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.validate_document(&params.text_document.uri, &text).await;
        } else if let Some(stored_text) = self.documents.read().await.get(&params.text_document.uri) {
            self.validate_document(&params.text_document.uri, stored_text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        // Remove document from storage and clear diagnostics
        self.documents.write().await.remove(&params.text_document.uri);
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(text) = self.documents.read().await.get(uri) {
            Ok(self.get_hover_info(uri, text, position).await)
        } else {
            Ok(None)
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(text) = self.documents.read().await.get(uri) {
            Ok(self.get_completions(uri, text, position).await)
        } else {
            Ok(None)
        }
    }

    async fn diagnostic(&self, params: DocumentDiagnosticParams) -> Result<DocumentDiagnosticReportResult> {
        let uri = &params.text_document.uri;
        
        if let Some(text) = self.documents.read().await.get(uri) {
            let mut diagnostics = Vec::new();

            // Parse the document with resilient parser
            let parse_result = parse_program_resilient(text);
            
            // Add parse errors as diagnostics
            for parse_error in &parse_result.errors {
                let diagnostic = Diagnostic {
                    range: Range {
                        start: Position::new(parse_error.line, parse_error.column),
                        end: Position::new(parse_error.line, parse_error.column + 10), // Show a reasonable range
                    },
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(NumberOrString::String("parse_error".to_string())),
                    source: Some("lynx".to_string()),
                    message: parse_error.message.clone(),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                };
                diagnostics.push(diagnostic);
            }
            
            // Type check the parsed program (even if there were parse errors)
            match typecheck_program_with_details(&parse_result.program) {
                Ok(_env) => {
                    // No type errors - only parse errors (if any) are in diagnostics
                }
                Err(errors) => {
                    // Convert type errors to LSP diagnostics
                    for error in errors {
                        let diagnostic = self.error_to_diagnostic(&error, text);
                        diagnostics.push(diagnostic);
                    }
                }
            }

            Ok(DocumentDiagnosticReportResult::Report(
                DocumentDiagnosticReport::Full(
                    RelatedFullDocumentDiagnosticReport {
                        related_documents: None,
                        full_document_diagnostic_report: FullDocumentDiagnosticReport {
                            result_id: None,
                            items: diagnostics,
                        },
                    }
                )
            ))
        } else {
            Ok(DocumentDiagnosticReportResult::Report(
                DocumentDiagnosticReport::Full(
                    RelatedFullDocumentDiagnosticReport {
                        related_documents: None,
                        full_document_diagnostic_report: FullDocumentDiagnosticReport {
                            result_id: None,
                            items: vec![],
                        },
                    }
                )
            ))
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| LynxLanguageServer::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}