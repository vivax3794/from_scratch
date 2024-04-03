#![feature(rustc_private)]
#![feature(let_chains)]

dylint_linting::dylint_library!();

extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_attr;
extern crate rustc_data_structures;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_lexer;
extern crate rustc_lint;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;

use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::match_def_path;
use rustc_lint::LateLintPass;

#[no_mangle]
pub fn register_lints(
    sess: &rustc_session::Session,
    lint_store: &mut rustc_lint::LintStore,
) {
    dylint_linting::init_config(sess);
    lint_store.register_lints(&[
        BRANCH_WITHOUT_TERM_CHECK,
        MISMATCHED_SCOPE,
    ]);
    lint_store
        .register_late_pass(|_| Box::new(BranchWithoutTermCheck));
    lint_store.register_late_pass(|_| Box::new(MismatchedScope));
}

rustc_session::declare_lint! {
    pub BRANCH_WITHOUT_TERM_CHECK,
    Deny,
    "Detects branching without checking termination"
}
rustc_session::declare_lint_pass!(BranchWithoutTermCheck => [BRANCH_WITHOUT_TERM_CHECK]);

impl<'tcx> LateLintPass<'tcx> for BranchWithoutTermCheck {
    // A list of things you might check can be found here:
    // https://doc.rust-lang.org/stable/nightly-rustc/rustc_lint/trait.LateLintPass.html

    // Find calls to `rust_compiler::codegen::CodeGen::generate_body` followed
    // by a call to `inkwell::builder::Builder::build_unconditional_branch`
    // or `inkwell::builder::Builder::build_conditional_branch`
    // and emit a deny that instructs to check the return type of generate_body

    fn check_block(
        &mut self,
        cx: &rustc_lint::LateContext<'tcx>,
        block: &rustc_hir::Block<'_>,
    ) {
        let mut previous_was_direct_gen_body = false;
        let mut gen_body_span = None;

        for stmt in block.stmts {
            if let rustc_hir::StmtKind::Semi(expr) = stmt.kind {
                if is_expr_meth_call(
                    cx,
                    expr,
                    &[
                        "rust_compiler",
                        "codegen",
                        "CodeGen",
                        "generate_body",
                    ],
                ) {
                    previous_was_direct_gen_body = true;
                    gen_body_span = Some(expr.span);
                } else if previous_was_direct_gen_body
                    && (is_expr_meth_call(
                        cx,
                        expr,
                        &[
                            "inkwell",
                            "builder",
                            "Builder",
                            "build_unconditional_branch",
                        ],
                    ) || is_expr_meth_call(
                        cx,
                        expr,
                        &[
                            "inkwell",
                            "builder",
                            "Builder",
                            "build_conditional_branch",
                        ],
                    ))
                {
                    span_lint_and_help(
                        cx,
                        BRANCH_WITHOUT_TERM_CHECK,
                        expr.span,
                        "branching without checking termination",
                        gen_body_span,
                        "check the return type of `Self::generate_body`",
                    );
                } else {
                    previous_was_direct_gen_body = false;
                }
            }
        }
    }
}

rustc_session::declare_lint! {
    pub MISMATCHED_SCOPE,
    Deny,
    "Checks for mismatched scope calls"
    // Checks that `new_scope` and `pop_scope` are called in pairs
}
rustc_session::declare_lint_pass!(MismatchedScope => [MISMATCHED_SCOPE]);

impl<'tcx> LateLintPass<'tcx> for MismatchedScope {
    fn check_block(
        &mut self,
        cx: &rustc_lint::LateContext<'tcx>,
        block: &rustc_hir::Block<'_>,
    ) {
        let mut scope_stack = Vec::new();

        for stmt in block.stmts {
            if let rustc_hir::StmtKind::Semi(expr) = stmt.kind {
                if is_expr_meth_call(
                    cx,
                    expr,
                    &[
                        "rust_compiler",
                        "type_system",
                        "TypeResolver",
                        "new_scope",
                    ],
                ) || is_expr_meth_call_name(
                    cx,
                    expr,
                    "new_scope_from_narrow",
                ) {
                    scope_stack.push(expr.span);
                } else if is_expr_meth_call(
                    cx,
                    expr,
                    &[
                        "rust_compiler",
                        "type_system",
                        "TypeResolver",
                        "pop_scope",
                    ],
                ) {
                    if scope_stack.pop().is_none() {
                        span_lint_and_help(
                            cx,
                            MISMATCHED_SCOPE,
                            expr.span,
                            "mismatched scope calls",
                            None,
                            "call `new_scope` before `pop_scope`",
                        );
                    }
                }
            }
        }

        if let Some(span) = scope_stack.pop() {
            span_lint_and_help(
                cx,
                MISMATCHED_SCOPE,
                span,
                "mismatched scope calls",
                None,
                "call `pop_scope` after `new_scope`",
            );
        }
    }
}

fn is_expr_meth_call<'tcx>(
    cx: &rustc_lint::LateContext<'tcx>,
    expr: &rustc_hir::Expr<'_>,
    def_path: &[&str],
) -> bool {
    let Some(method_def_id) =
        cx.typeck_results().type_dependent_def_id(expr.hir_id)
    else {
        return false;
    };

    match_def_path(cx, method_def_id, def_path)
}

fn is_expr_meth_call_name<'tcx>(
    cx: &rustc_lint::LateContext<'tcx>,
    expr: &rustc_hir::Expr<'_>,
    name: &str,
) -> bool {
    if let rustc_hir::ExprKind::MethodCall(path, _, _, _) = &expr.kind
    {
        return path.ident.name.as_str() == name;
    }

    false
}
