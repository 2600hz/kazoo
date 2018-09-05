Manipulating JSON objects in Erlang

\## Lifting common JSON properties

\### Basic usage

\`\`\`erlang kz\_json:lift\_common\_properties([JObj1, JObj2]) -> {CommonJObj, [UniqueJObj1, UniqueJObj2]}. \`\`\`

All key/value pairs common to each JObj in the list will be collected into \`CommonJObj\` and removed from each \`JObj\`.

\### Blacklists

Sometimes you know there are keys that shouldn't be lifted (like leg-only variables on endpoints vs channel variables):

\`\`\`erlang kz\_json:lift\_common\_properties([JObj1, JObj2], [Key1, Path2]) -> {CommonJObj, [UniqueJObj1, UniqueJObj2]}. \`\`\`

This will not allow \`Key1\` and \`Path2\` to be in \`CommonJObj\` and will remain in the objects that have it set. Note that the blacklist can be keys (\`<a id="orga8d71d4"></a>\`) or paths (\`[<a id="org3c3da2d"></a>, <a id="orgd2a2d59"></a>]\`).