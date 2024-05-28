//===--- CustomizableRulesCheck.h - clang-tidy ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_CUSTOMIZABLERULESCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_CUSTOMIZABLERULESCHECK_H

#include "../ClangTidyCheck.h"
#include <unordered_map>
#include <fstream>

#include "clang/AST/AST.h"

// TODO: get rid of this, used for debugging
template <typename T>
T readFileToString(const T& filePath) {
    std::ifstream file(filePath);
    if (!file.is_open()) {
        return "";
    }

    std::string content((std::istreambuf_iterator<char>(file)),
                        (std::istreambuf_iterator<char>()));
    file.close();
    return content;
}

namespace clang::tidy::misc {

class BaseCRNode;
class MapCRNode;

class CRRule {
public:
  std::string Name;
  std::string Description;

  std::unique_ptr<clang::ast_matchers::internal::Matcher<clang::Stmt>> MatcherStmt;
  std::unique_ptr<clang::ast_matchers::internal::Matcher<clang::Decl>> MatcherDecl;
};

/// FIXME: Write a short description.
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/misc/customizable-rules.html
class CustomizableRulesCheck : public ClangTidyCheck {
public:
  CustomizableRulesCheck(StringRef Name, ClangTidyContext *Context);

  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override; // Dummy
  void handleCustomRuleMatch(const ast_matchers::MatchFinder::MatchResult &Result, ast_matchers::MatchFinder::MatchCallback* Callback);

private:
  std::string findRuleFileFilepath(const std::string& From);

  bool populateRules(BaseCRNode* CRNode);
  void registerRules(ast_matchers::MatchFinder* Finder);
  std::unique_ptr<CRRule> generateRule(MapCRNode* RuleNode);

  std::string parseName(BaseCRNode* CRNode);
  std::string parseDescription(BaseCRNode* CRNode);
  std::unique_ptr<clang::ast_matchers::internal::Matcher<clang::Decl>> parseMatcher(BaseCRNode* CRNode);

  void printRules();
private:
  std::string RulesFilepath;
  std::vector<std::unique_ptr<CRRule>> Rules;
  std::vector<std::unique_ptr<ast_matchers::MatchFinder::MatchCallback>> MatchCallbacks;
  std::unordered_map<ast_matchers::MatchFinder::MatchCallback*, CRRule*> CallbackToRule;
  void generateRootMatcher(ast_matchers::MatchFinder *Finder, BaseCRNode *Root);
};

} // namespace clang::tidy::misc

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_CUSTOMIZABLERULESCHECK_H
