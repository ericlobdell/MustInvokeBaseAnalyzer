using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MustInvokeBaseMethod
{
    [ExportCodeFixProvider(LanguageNames.CSharp)]
    [Shared]
    class MustInvokeBaseMethodCallMethodCodeFix : CodeFixProvider
    {
        private const string codeFixDescription = "Add base invocation";
        public override ImmutableArray<string> FixableDiagnosticIds =>
            new ImmutableArray<string> { MustInvokeBaseMethodAnalyzer.DiagnosticId };

        public override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken)
                .ConfigureAwait(false);

            context.CancellationToken.ThrowIfCancellationRequested();

            var diagnostic = context.Diagnostics[0];
            var methodNode = root.FindNode(diagnostic.Location.SourceSpan) as MethodDeclarationSyntax;
            var model = await context.Document.GetSemanticModelAsync(context.CancellationToken);
            var methodSybol = model.GetDeclaredSymbol(methodNode) as IMethodSymbol;
            var invocation = CreateInvocation(methodSybol);

            invocation = AddArguments(context, methodSybol, invocation);

            var statement = CreateStatement(context, methodNode, methodSybol, invocation);
            var newRoot = CreateRoot(root, methodNode, statement);

            context.RegisterCodeFix(CodeAction.Create(codeFixDescription, _ => Task.FromResult(context.Document.WithSyntaxRoot(newRoot)), codeFixDescription), diagnostic);
        }

        private SyntaxNode CreateRoot(SyntaxNode root, MethodDeclarationSyntax methodNode, StatementSyntax statement)
        {
            var body = methodNode.Body;
            var firstNode = body.ChildNodes().FirstOrDefault();
            var newBody = firstNode != null ?
                    body.InsertNodesBefore(body.ChildNodes().First(), new[] { statement }) :
                    SyntaxFactory.Block(statement);

            var newRoot = root.ReplaceNode(body, newBody);

            return newRoot;

        }

        private StatementSyntax CreateStatement(CodeFixContext context, MethodDeclarationSyntax methodNode, IMethodSymbol methodSybol, InvocationExpressionSyntax invocation)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            StatementSyntax statement = null;

            if (!methodSybol.ReturnsVoid)
            {
                var returnValueSafeName = CreateSafeLocalVariableName(methodNode, methodSybol);
                statement = SyntaxFactory.LocalDeclarationStatement(
                    SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName("var"))
                    .WithVariables(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.VariableDeclarator(
                                SyntaxFactory.Identifier(returnValueSafeName))
                            .WithInitializer(SyntaxFactory.EqualsValueClause(invocation)))));
            }
            else
            {
                statement = SyntaxFactory.ExpressionStatement(invocation);
            }

            return statement.WithAdditionalAnnotations(Formatter.Annotation);
        }

        private string CreateSafeLocalVariableName(MethodDeclarationSyntax methodNode, IMethodSymbol methodSymbol)
        {
            var localDeclarations = methodNode.DescendantNodes(_ => true)
                    .OfType<VariableDeclaratorSyntax>();

            var returnValueName = $"{methodSymbol.Name.Substring(0,1).ToLower()}{methodSymbol.Name.Substring(1)}Result";
            var returnValueSafeName = returnValueName;
            var returnValueCount = 0;

            while (localDeclarations.Any( _ => _.Identifier.Text == returnValueSafeName))
            {
                returnValueSafeName = $"{returnValueName}{returnValueCount}";
                returnValueCount++;
            }

            return returnValueSafeName;
        }

        private static InvocationExpressionSyntax AddArguments(CodeFixContext context, IMethodSymbol methodSybol, InvocationExpressionSyntax invocation)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            var argumentCount = methodSybol.Parameters.Length;
            if (argumentCount > 0)
            {
                var arguments = new SyntaxNodeOrToken[(2 * argumentCount) - 1];
                for (int i = 0; i < argumentCount; i++)
                {
                    var parameter = methodSybol.Parameters[i];
                    var argument = SyntaxFactory.Argument(SyntaxFactory.IdentifierName(parameter.Name));

                    if (parameter.RefKind.HasFlag(RefKind.Ref))
                    {
                        argument = argument.WithRefOrOutKeyword(SyntaxFactory.Token(SyntaxKind.RefKeyword));

                    }
                    else if (parameter.RefKind.HasFlag(RefKind.Out))
                    {
                        argument = argument.WithRefOrOutKeyword(SyntaxFactory.Token(SyntaxKind.OutKeyword));
                    }

                    arguments[2 * i] = argument;

                    if (i < argumentCount - 1)
                    {
                        arguments[(2 * i) + 1] = SyntaxFactory.Token(SyntaxKind.CommaToken);
                    }
                }

                invocation = invocation.WithArgumentList(
                    SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList<ArgumentSyntax>(arguments))
                        .WithOpenParenToken(SyntaxFactory.Token(SyntaxKind.OpenParenToken))
                        .WithCloseParenToken(SyntaxFactory.Token(SyntaxKind.CloseParenToken)));
            }

            return invocation;
        }

        private static InvocationExpressionSyntax CreateInvocation(IMethodSymbol methodSybol)
        {
            return SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.BaseExpression().WithToken(SyntaxFactory.Token(SyntaxKind.BaseKeyword)),
                    SyntaxFactory.IdentifierName(methodSybol.Name))
                .WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken)));
        }
    }
}
