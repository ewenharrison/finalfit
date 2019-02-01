library(testit)

assert(
  'spaces(n) gives n spaces',
  spaces(-1) == '', spaces(0) == '', spaces(1) == ' ', spaces(5) == '     '
)

assert(
  'escape_latex() sanitizes backslashes and {}',
  escape_latex('\\') == '\\textbackslash{}',
  escape_latex('\\{}') == '\\textbackslash{}\\{\\}',
  escape_latex('{\\}') == '\\{\\textbackslash{}\\}',
  escape_latex('~!@#$%^&*()') == '~!@#$%^&*()'
)

assert(
  'escape_html() escapes HTML chars',
  escape_html('&"<>') == '&amp;&quot;&lt;&gt;',
  escape_html('~!@#$%^&*()') == '~!@#$%^&amp;*()'
)

assert(
  'try_parse() tells if a code fragment is complete or not',
  try_parse('1+1'), !try_parse('1+1+'), try_parse('if(TRUE)1'),
  !try_parse(c('if(T){','F')), try_parse(c('if(T){','F}'))
)

assert(
  'group_src() puts lines of the same expression into a list element',
  identical(group_src('1+1'), list('1+1')),
  identical(group_src(c('1+1+', '1')), list(c('1+1+', '1'))),
  identical(group_src(c('1+1+', '1', 'TRUE')), list(c('1+1+', '1'), 'TRUE'))
)

assert(
  'group_src() should signal an error for incomplete code',
  has_error(group_src('1+1+')),
  has_error(group_src(c('1+1', '1+1+')))
)
