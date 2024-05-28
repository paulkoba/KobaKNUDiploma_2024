#include <fstream>
#include <map>
#include <vector>

struct VariableLengthMatcher {
    const char* llvmName;
    const char* returnType;
    const char* argumentType;
    int minArg = 0;
    int maxArg = 10;
};

void GenerateSpecialCases(std::ofstream& os) {
    os << 
        "std::optional<std::string> parse_string(BaseCRNode* Root) {\n"
        "  CRNode* RootAsCRNode = dyn_cast<CRNode>(Root);\n"
        "  if (!RootAsCRNode) {\n"
        "    LOG_DEBUG(\"Invalid configuration, expected map\");\n"
        "    return {};\n"
        "  }\n"
        "\n"
        "  LOG_DEBUG(\"Handling matcher with name: \" << RootAsCRNode->Value);\n"
        "\n"
        "  return RootAsCRNode->Value;\n"
        "}\n"
        "\n"
        "std::optional<int64_t> parse_int64_t(BaseCRNode* Root) {\n"
        "  CRNode* RootAsCRNode = dyn_cast<CRNode>(Root);\n"
        "  if (!RootAsCRNode) {\n"
        "    LOG_DEBUG(\"Invalid configuration, expected map\");\n"
        "    return {};\n"
        "  }\n"
        "\n"
        "  LOG_DEBUG(\"Handling matcher with name: \" << RootAsCRNode->Value);\n"
        "\n"
        "  return std::stoll(RootAsCRNode->Value);\n"
        "}\n";
}

void OutputVariableLengthMatcherGenerator(const VariableLengthMatcher& VariableLengthMatcher, std::ofstream& os) {
    if (VariableLengthMatcher.llvmName == "int64_t" || VariableLengthMatcher.llvmName == "string") return;
    os <<
        "std::optional<clang::ast_matchers::internal::Matcher<" << VariableLengthMatcher.returnType << ">> generate_" << VariableLengthMatcher.llvmName << "_" << VariableLengthMatcher.returnType << "(BaseCRNode* Root) {\n"
        "  SequenceCRNode* Arguments = dyn_cast<SequenceCRNode>(Root);\n"
        "  if (!Arguments) {\n";

    // TODO: Rewrite terrible hack
    if (VariableLengthMatcher.minArg >= 1 && VariableLengthMatcher.maxArg <= 1) {
        os << "    auto Arg0 = parse_" << VariableLengthMatcher.argumentType << "(Root);\n"
              "    if (!Arg0) return {};\n"
              "    return std::optional<clang::ast_matchers::internal::Matcher<" << VariableLengthMatcher.returnType << ">>(" << VariableLengthMatcher.llvmName << "(Arg0.value()));\n";
    }

    os <<
        "    LOG_DEBUG(\"Invalid configuration, expected sequence\");\n"
        "    return {};\n"
        "  }\n"
        "  switch (Arguments->Children.size()) {\n";

    for (int i = VariableLengthMatcher.minArg; i <= VariableLengthMatcher.maxArg; ++i) {
        os << "  case " << i << ": {\n";

        for (int j = 0; j < i; ++j) {
            os << "    auto Arg" << j << " = parse_" << VariableLengthMatcher.argumentType << "(Arguments->Children[" << j << "].get());\n"
            "    if (!Arg" << j << ") return {};\n";
        }
        os << "    return std::optional<clang::ast_matchers::internal::Matcher<" << VariableLengthMatcher.returnType << ">>(" << VariableLengthMatcher.llvmName << "(";
        for (int j = 0; j < i; ++j) {
            os << "Arg" << j << ".value()";
            if (j != i - 1) os << ", ";
        }
        os << "));\n";
        os << "  }\n";
        ;
    }
    os << 
        "  default: {\n"
        "    LOG_DEBUG(\"Too many arguments: \" << Arguments->Children.size())\n"
        "    return {};\n"
        "  }\n"
        "  }\n"
        "}\n\n";
    ;
}

void OutputParser(const std::string& name, const std::vector<std::string>& sources, std::ofstream& os)
{
    os <<
        "std::optional<clang::ast_matchers::internal::Matcher<" << name << ">> parse_" << name << "(BaseCRNode* Root) {\n"
        "  MapCRNode* RootAsMap = dyn_cast<MapCRNode>(Root);\n"
        "  if (!RootAsMap) {\n"
        "    LOG_DEBUG(\"Invalid configuration, expected map\");\n"
        "    return {};\n"
        "  }\n"
        "\n"
        "  if (RootAsMap->Children.size() != 1) {\n"
        "    LOG_DEBUG(\"Invalid configuration, expected 1 element\");\n"
        "    return {};\n"
        "  }\n"
        "\n"
        "  auto& MatcherName = RootAsMap->Children[0].first;\n"
        "  LOG_DEBUG(\"Handling matcher with name: \" << MatcherName);\n"
        "\n"
        "  switch(hashCString(MatcherName.c_str())) {\n";

    for (const auto& el : sources) {
        os << 
            "  case hashCString(\"" << el << "\"):\n"
            "    return generate_" << el << "_" << name << "(RootAsMap->Children[0].second.get());\n";
    }

    os << 
        "  default:\n"
        "    LOG_DEBUG(\"Unknown matcher: \" << MatcherName);\n"
        "    return {};\n"
        "  }\n"
        "}\n\n"
    ;
}

int main() {
    VariableLengthMatcher VariableLengthMatchers[] = {
        /* function name, return type, argument type*/
        /* {"attr", "Attr", "Attr", 0, 0}, */
        {"cxxBaseSpecifier", "CXXBaseSpecifier", "CXXBaseSpecifier"},
        {"cxxCtorInitializer", "CXXCtorInitializer", "CXXCtorInitializer"},
        {"accessSpecDecl", "Decl", "AccessSpecDecl"},
        {"bindingDecl", "Decl", "BindingDecl"},
        {"blockDecl", "Decl", "BlockDecl"},
        {"classTemplateDecl", "Decl", "ClassTemplateDecl"},
        {"classTemplatePartialSpecializationDecl", "Decl", "ClassTemplatePartialSpecializationDecl"},
        {"classTemplateSpecializationDecl", "Decl", "ClassTemplateSpecializationDecl"},
        {"conceptDecl", "Decl", "ConceptDecl"},
        {"cxxConstructorDecl", "Decl", "CXXConstructorDecl"},
        {"cxxConversionDecl", "Decl", "CXXConversionDecl"},
        {"cxxDeductionGuideDecl", "Decl", "CXXDeductionGuideDecl"},
        {"cxxDestructorDecl", "Decl", "CXXDestructorDecl"},
        {"cxxMethodDecl", "Decl", "CXXMethodDecl"},
        {"cxxRecordDecl", "Decl", "CXXRecordDecl"},

        {"functionDecl", "Decl", "FunctionDecl"},
        {"functionTemplateDecl", "Decl", "FunctionTemplateDecl"},
        {"indirectFieldDecl", "Decl", "IndirectFieldDecl"},
        {"labelDecl", "Decl", "LabelDecl"},
        {"linkageSpecDecl", "Decl", "LinkageSpecDecl"},
        {"namespaceAliasDecl", "Decl", "NamespaceAliasDecl"},
        {"namespaceDecl", "Decl", "NamespaceDecl"},
        {"nonTypeTemplateParmDecl", "Decl", "NonTypeTemplateParmDecl"},
        {"objcCategoryDecl", "Decl", "ObjCCategoryDecl"},
        {"objcCategoryImplDecl", "Decl", "ObjCCategoryImplDecl"},
        {"objcImplementationDecl", "Decl", "ObjCImplementationDecl"},
        {"objcInterfaceDecl", "Decl", "ObjCInterfaceDecl"},
        {"objcIvarDecl", "Decl", "ObjCIvarDecl"},
        {"objcMethodDecl", "Decl", "ObjCMethodDecl"},
        {"objcPropertyDecl", "Decl", "ObjCPropertyDecl"},
        {"objcProtocolDecl", "Decl", "ObjCProtocolDecl"},

        {"parmVarDecl", "Decl", "ParmVarDecl"},
        {"recordDecl", "Decl", "RecordDecl"},
        {"staticAssertDecl", "Decl", "StaticAssertDecl"},
        {"tagDecl", "Decl", "TagDecl"},
        {"templateTemplateParmDecl", "Decl", "TemplateTemplateParmDecl"},
        {"templateTypeParmDecl", "Decl", "TemplateTypeParmDecl"},
        {"translationUnitDecl", "Decl", "TranslationUnitDecl"},
        {"typeAliasDecl", "Decl", "TypeAliasDecl"},
        {"typeAliasTemplateDecl", "Decl", "TypeAliasTemplateDecl"},

        {"typedefDecl", "Decl", "TypedefDecl"},
        {"typedefNameDecl", "Decl", "TypedefNameDecl"},
        {"unresolvedUsingTypenameDecl", "Decl", "UnresolvedUsingTypenameDecl"},
        {"unresolvedUsingValueDecl", "Decl", "UnresolvedUsingValueDecl"},
        {"usingDecl", "Decl", "UsingDecl"},
        {"usingDirectiveDecl", "Decl", "UsingDirectiveDecl"},
        {"usingEnumDecl", "Decl", "UsingEnumDecl"},
        {"valueDecl", "Decl", "ValueDecl"},
        {"varDecl", "Decl", "VarDecl"},

        {"namedDecl", "Decl", "NamedDecl"},
        {"decl", "Decl", "Decl"},
        {"declaratorDecl", "Decl", "DeclaratorDecl"},
        {"decompositionDecl", "Decl", "DecompositionDecl"},
        {"enumDecl", "Decl", "EnumDecl"},
        {"fieldDecl", "Decl", "FieldDecl"},
        {"friendDecl", "Decl", "FriendDecl"},
        {"callExpr", "CallExpr", "CallExpr", 0, 0},
        {"callExpr", "Stmt", "CallExpr"},
        {"callee", "CallExpr", "Decl", 1, 1},
        {"hasRHS", "BinaryOperator", "Expr", 1, 1},
        {"hasLHS", "BinaryOperator", "Expr", 1, 1},
        {"hasAnyArgument", "CallExpr", "Expr", 1, 1},
        {"hasDeclaration", "CallExpr", "Decl", 1, 1},
        {"hasCaseConstant", "CaseStmt", "Expr", 1, 1},
        {"caseStmt", "Stmt", "CaseStmt", 1, 1},
        {"castExpr", "Stmt", "CastExpr", 1, 1},
        {"binaryOperator", "BinaryOperator", "BinaryOperator"},
        {"parameterCountIs", "FunctionDecl", "int64_t", 1, 1},
        {"hasTrailingReturn", "FunctionDecl", "FunctionDecl", 0, 0},
        {"hasOverloadedOperatorName", "FunctionDecl", "string", 1, 1},
        {"hasDynamicExceptionSpec", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isConsteval", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isConstexpr", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isDefaulted", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isDefinition", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isDeleted", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isExternC", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isInline", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isMain", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isNoReturn", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isNoThrow", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isStaticStorageClass", "FunctionDecl", "FunctionDecl", 0, 0},
        {"hasDefinition", "CXXRecordDecl", "CXXRecordDecl", 0, 0},
        {"isDerivedFrom", "CXXRecordDecl", "string", 1, 1},
        {"isDirectlyDerivedFrom", "CXXRecordDecl", "string", 1, 1},
        {"isExplicitTemplateSpecialization", "CXXRecordDecl", "CXXRecordDecl", 0, 0},
        {"isFinal", "CXXRecordDecl", "CXXRecordDecl", 0, 0},
        {"isLambda", "CXXRecordDecl", "CXXRecordDecl", 0, 0},
        {"isSameOrDerivedFrom", "CXXRecordDecl", "string", 1, 1},
        /* {"isTemplateInstantiation", "FunctionDecl", "FunctionDecl", 0, 0}, */
        {"isVariadic", "FunctionDecl", "FunctionDecl", 0, 0},
        {"isWeak", "FunctionDecl", "FunctionDecl", 0, 0},
        {"hasName", "NamedDecl", "string", 1, 1},

        {"lambdaCapture", "LambdaCapture", "LambdaCapture"},
        {"nestedNameSpecifierLoc", "NestedNameSpecifierLoc", "NestedNameSpecifierLoc"},
        {"nestedNameSpecifier", "NestedNameSpecifier", "NestedNameSpecifier"},
        {"ompDefaultClause", "OMPClause", "OMPClause"},
        {"qualType", "QualType", "QualType"},

        {"addrLabelExpr", "Stmt", "AddrLabelExpr"},
        {"arrayInitIndexExpr", "Stmt", "ArrayInitIndexExpr"},
        {"arrayInitLoopExpr", "Stmt", "ArrayInitLoopExpr"},
        {"arraySubscriptExpr", "Stmt", "ArraySubscriptExpr"},
        {"asmStmt", "Stmt", "AsmStmt"},
        {"atomicExpr", "Stmt", "AtomicExpr"},
        {"autoreleasePoolStmt", "Stmt", "ObjCAutoreleasePoolStmt"},
        {"binaryConditionalOperator", "Stmt", "BinaryConditionalOperator"},
        {"binaryOperator", "Stmt", "BinaryOperator"},
        {"blockExpr", "Stmt", "BlockExpr"},
        {"breakStmt", "Stmt", "BreakStmt"},
        {"cStyleCastExpr", "Stmt", "CStyleCastExpr"},
        {"characterLiteral", "Stmt", "CharacterLiteral"},
        {"chooseExpr", "Stmt", "ChooseExpr"},
        {"coawaitExpr", "Stmt", "CoawaitExpr"},
        {"compoundLiteralExpr", "Stmt", "CompoundLiteralExpr"},
        {"compoundStmt", "Stmt", "CompoundStmt"},
        {"conditionalOperator", "Stmt", "ConditionalOperator"},
        {"constantExpr", "Stmt", "ConstantExpr"},
        {"continueStmt", "Stmt", "ContinueStmt"},
        /* {"convertVectorExpr", "Stmt", "ConvertVectorExpr"}, */
        {"coreturnStmt", "Stmt", "CoreturnStmt"},
        {"coroutineBodyStmt", "Stmt", "CoroutineBodyStmt"},
        {"coyieldExpr", "Stmt", "CoyieldExpr"},
        {"cudaKernelCallExpr", "Stmt", "CUDAKernelCallExpr"},
        {"cxxBindTemporaryExpr", "Stmt", "CXXBindTemporaryExpr"},
        {"cxxBoolLiteral", "Stmt", "CXXBoolLiteralExpr"},
        {"cxxCatchStmt", "Stmt", "CXXCatchStmt"},
        {"cxxConstCastExpr", "Stmt", "CXXConstCastExpr"},
        {"cxxConstructExpr", "Stmt", "CXXConstructExpr"},
        {"cxxDefaultArgExpr", "Stmt", "CXXDefaultArgExpr"},
        {"cxxDeleteExpr", "Stmt", "CXXDeleteExpr"},
        {"cxxDependentScopeMemberExpr", "Stmt", "CXXDependentScopeMemberExpr"},
        {"cxxDynamicCastExpr", "Stmt", "CXXDynamicCastExpr"},
        /* {"cxxFoldExpr", "Stmt", "CXXFoldExpr"}, */
        {"cxxForRangeStmt", "Stmt", "CXXForRangeStmt"},
        {"cxxFunctionalCastExpr", "Stmt", "CXXFunctionalCastExpr"},
        {"cxxMemberCallExpr", "Stmt", "CXXMemberCallExpr"},
        {"cxxNewExpr", "Stmt", "CXXNewExpr"},
        {"cxxNoexceptExpr", "Stmt", "CXXNoexceptExpr"},
        {"cxxNullPtrLiteralExpr", "Stmt", "CXXNullPtrLiteralExpr"},
        {"cxxOperatorCallExpr", "Stmt", "CXXOperatorCallExpr"},
        {"cxxReinterpretCastExpr", "Stmt", "CXXReinterpretCastExpr"},
        {"cxxRewrittenBinaryOperator", "Stmt", "CXXRewrittenBinaryOperator"},
        {"cxxStaticCastExpr", "Stmt", "CXXStaticCastExpr"},
        {"cxxStdInitializerListExpr", "Stmt", "CXXStdInitializerListExpr"},
        {"cxxTemporaryObjectExpr", "Stmt", "CXXTemporaryObjectExpr"},
        {"cxxThisExpr", "Stmt", "CXXThisExpr"},
        {"cxxThrowExpr", "Stmt", "CXXThrowExpr"},
        {"cxxTryStmt", "Stmt", "CXXTryStmt"},
        {"cxxUnresolvedConstructExpr", "Stmt", "CXXUnresolvedConstructExpr"},
        {"declRefExpr", "Stmt", "DeclRefExpr"},
        {"declStmt", "Stmt", "DeclStmt"},
        {"defaultStmt", "Stmt", "DefaultStmt"},
        {"dependentCoawaitExpr", "Stmt", "DependentCoawaitExpr"},
        {"designatedInitExpr", "Stmt", "DesignatedInitExpr"},
        {"doStmt", "Stmt", "DoStmt"},
        {"explicitCastExpr", "Stmt", "ExplicitCastExpr"},
        {"expr", "Stmt", "Expr"},
        {"exprWithCleanups", "Stmt", "ExprWithCleanups"},
        {"fixedPointLiteral", "Stmt", "FixedPointLiteral"},
        {"floatLiteral", "Stmt", "FloatingLiteral"},
        {"forStmt", "Stmt", "ForStmt"},
        {"genericSelectionExpr", "Stmt", "GenericSelectionExpr"},
        {"gnuNullExpr", "Stmt", "GNUNullExpr"},
        {"gotoStmt", "Stmt", "GotoStmt"},
        {"ifStmt", "Stmt", "IfStmt"},
        {"imaginaryLiteral", "Stmt", "ImaginaryLiteral"},
        {"implicitCastExpr", "Stmt", "ImplicitCastExpr"},
        {"implicitValueInitExpr", "Stmt", "ImplicitValueInitExpr"},
        {"initListExpr", "Stmt", "InitListExpr"},
        {"integerLiteral", "Stmt", "IntegerLiteral"},
        {"labelStmt", "Stmt", "LabelStmt"},
        {"lambdaExpr", "Stmt", "LambdaExpr"},
        {"materializeTemporaryExpr", "Stmt", "MaterializeTemporaryExpr"},
        {"memberExpr", "Stmt", "MemberExpr"},
        {"nullStmt", "Stmt", "NullStmt"},
        {"objcCatchStmt", "Stmt", "ObjCAtCatchStmt"},
        {"objcFinallyStmt", "Stmt", "ObjCAtFinallyStmt"},
        {"objcIvarRefExpr", "Stmt", "ObjCIvarRefExpr"},
        {"objcMessageExpr", "Stmt", "ObjCMessageExpr"},
        {"objcStringLiteral", "Stmt", "ObjCStringLiteral"},
        {"objcThrowStmt", "Stmt", "ObjCAtThrowStmt"},
        {"objcTryStmt", "Stmt", "ObjCAtTryStmt"},
        {"ompExecutableDirective", "Stmt", "OMPExecutableDirective"},
        {"opaqueValueExpr", "Stmt", "OpaqueValueExpr"},
        {"parenExpr", "Stmt", "ParenExpr"},
        {"parenListExpr", "Stmt", "ParenListExpr"},
        {"predefinedExpr", "Stmt", "PredefinedExpr"},
        {"returnStmt", "Stmt", "ReturnStmt"},
        {"stmtExpr", "Stmt", "StmtExpr"},
        {"substNonTypeTemplateParmExpr", "Stmt", "SubstNonTypeTemplateParmExpr"},
        {"switchCase", "Stmt", "SwitchCase"},
        {"switchStmt", "Stmt", "SwitchStmt"},
        {"unaryExprOrTypeTraitExpr", "Stmt", "UnaryExprOrTypeTraitExpr"},
        {"unresolvedLookupExpr", "Stmt", "UnresolvedLookupExpr"},
        {"unresolvedMemberExpr", "Stmt", "UnresolvedMemberExpr"},
        {"userDefinedLiteral", "Stmt", "UserDefinedLiteral"},
        {"unaryOperator", "Stmt", "UnaryOperator"},

    };

    std::map<std::string, std::vector<std::string>> argumentTypes;
    std::map<std::string, std::vector<std::string>> returnTypes;

    for (const auto& el : VariableLengthMatchers) {
        argumentTypes[el.argumentType].push_back(el.llvmName);
        returnTypes[el.returnType].push_back(el.llvmName);
    }

    std::ofstream os("../CRAutogen.h"); // TODO: fix this thing
    os << "#include \"clang/AST/AST.h\"\n";
    os << "#include \"clang/ASTMatchers/ASTMatchFinder.h\"\n";
    os << "#include \"clang/ASTMatchers/ASTMatchers.h\"\n";
    os << "#include <optional>\n\n";
    os << "namespace cr_autogen {\n\n";

    for (const auto& el : argumentTypes) {
        if (el.first == "int64_t" || el.first == "string") continue;;
        os << "std::optional<clang::ast_matchers::internal::Matcher<" << el.first << ">> parse_" << el.first << "(BaseCRNode* Root);\n";
    }

    os << "\n";

    GenerateSpecialCases(os);
    for (const auto& el : VariableLengthMatchers) {
        OutputVariableLengthMatcherGenerator(el, os);
    }

    for (const auto& el : returnTypes) {
        OutputParser(el.first, el.second, os);
    }

    for (const auto& el : argumentTypes) {
        if (returnTypes.find(el.first) == returnTypes.end() && el.first != "int64_t" && el.first != "string") {
            OutputParser(el.first, {}, os);
        }
    }

    os << "}\n";
}