using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace MustInvokeBaseMethod
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MustInvokeBaseMethodAnalyzer : DiagnosticAnalyzer
    {
        public static string DiagnosticId = "MustInvokeBaseMethod";

        private static DiagnosticDescriptor rule = new DiagnosticDescriptor(
            DiagnosticId,
            "Find overriden methods that do not call the base class' method", 
            "Virtual methods with [MustInvoke] must be invoked in overrides", 
            "Usage", 
            DiagnosticSeverity.Error, 
            true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => new ImmutableArray<DiagnosticDescriptor> { rule };

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction<SyntaxKind>(this.AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration);
        }

        private void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
        {
            var method = context.Node as MethodDeclarationSyntax;
            var model = context.SemanticModel;
            var methodSymbol = model.GetDeclaredSymbol(method) as IMethodSymbol;

            context.CancellationToken.ThrowIfCancellationRequested();

            if (methodSymbol.IsOverride)
            {
                var overridenMethod = methodSymbol.OverriddenMethod;
                var hasAttribute = overridenMethod
                    .GetAttributes()
                    .Any(a => a.AttributeClass.Name == "MustInvokeAttribute");

                context.CancellationToken.ThrowIfCancellationRequested();

                if(hasAttribute)
                {
                    var invocations = method.DescendantNodes(_ => true)
                        .OfType<InvocationExpressionSyntax>();

                    foreach (var invocation in invocations)
                    {
                        var invocationSymbol = model.GetSymbolInfo(invocation.Expression).Symbol as IMethodSymbol;

                        if (invocationSymbol == overridenMethod)
                            return;

                        context.ReportDiagnostic(Diagnostic.Create(rule, method.Identifier.GetLocation()));
                    }
                }

            }
        }
    }
}
