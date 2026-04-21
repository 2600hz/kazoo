# Step 10: User Journey Flows

## MANDATORY EXECUTION RULES (READ FIRST):

- 🛑 NEVER generate content without user input

- 📖 CRITICAL: ALWAYS read the complete step file before taking any action - partial understanding leads to incomplete decisions
- 🔄 CRITICAL: When loading next step with 'C', ensure the entire file is read and understood before proceeding
- ✅ ALWAYS treat this as collaborative discovery between UX facilitator and stakeholder
- 📋 YOU ARE A UX FACILITATOR, not a content generator
- 💬 FOCUS on designing user flows and journey interactions
- 🎯 COLLABORATIVE flow design, not assumption-based layouts
- ✅ YOU MUST ALWAYS SPEAK OUTPUT In your Agent communication style with the config `{communication_language}`

## EXECUTION PROTOCOLS:

- 🎯 Show your analysis before taking any action
- 💾 Write content directly to output file after generation
- 📖 Update output file frontmatter, adding this step to the end of the list of stepsCompleted.
- ⚠️ Present A/P/C/R menu after writing to file

## COLLABORATION MENUS (A/P/C/R):

This step will generate content and present choices:

- **A (Advanced Elicitation)**: Use discovery protocols to develop deeper journey insights
- **P (Party Mode)**: Bring multiple perspectives to design user flows
- **C (Continue)**: Accept the content and proceed to next step
- **R (Rewrite)**: Rewrite this section from scratch based on user feedback

## PROTOCOL INTEGRATION:

- When 'A' selected: Read fully and follow: {project-root}/_evo/core/workflows/advanced-elicitation/workflow.md
- When 'P' selected: Read fully and follow: {project-root}/_evo/core/workflows/party-mode/workflow.md
- PROTOCOLS always return to this step's A/P/C/R menu
- User accepts/rejects protocol changes before proceeding

## CONTEXT BOUNDARIES:

- Current document and frontmatter from previous steps are available
- Design direction from step 9 informs flow layout and visual design
- Core experience from step 7 defines key journey interactions
- Focus on designing detailed user flows with Mermaid diagrams

## YOUR TASK:

Design detailed user journey flows for critical user interactions.

## USER JOURNEY FLOWS SEQUENCE:

### 1. Load PRD User Journeys as Foundation

Start with user journeys already defined in the PRD:
"Great! Since we have the PRD available, let's build on the user journeys already documented there.

**Existing User Journeys from PRD:**
I've already loaded these user journeys from your PRD:
[Journey narratives from PRD input documents]

These journeys tell us **who** users are and **why** they take certain actions. Now we need to design **how** those journeys work in detail.

**Critical Journeys to Design Flows For:**
Looking at the PRD journeys, I need to design detailed interaction flows for:

- [Critical journey 1 identified from PRD narratives]
- [Critical journey 2 identified from PRD narratives]
- [Critical journey 3 identified from PRD narratives]

The PRD gave us the stories - now we design the mechanics!"

### 2. Design Each Journey Flow

For each critical journey, design detailed flow:

**For [Journey Name]:**
"Let's design the flow for users accomplishing [journey goal].

**Flow Design Questions:**

- How do users start this journey? (entry point)
- What information do they need at each step?
- What decisions do they need to make?
- How do they know they're progressing successfully?
- What does success look like for this journey?
- Where might they get confused or stuck?
- How do they recover from errors?"

### 3. Create Flow Diagrams

Visualize each journey with Mermaid diagrams:
"I'll create detailed flow diagrams for each journey showing:

**[Journey Name] Flow:**

- Entry points and triggers
- Decision points and branches
- Success and failure paths
- Error recovery mechanisms
- Progressive disclosure of information

Each diagram will map the complete user experience from start to finish."

### 4. Optimize for Efficiency and Delight

Refine flows for optimal user experience:
"**Flow Optimization:**
For each journey, let's ensure we're:

- Minimizing steps to value (getting users to success quickly)
- Reducing cognitive load at each decision point
- Providing clear feedback and progress indicators
- Creating moments of delight or accomplishment
- Handling edge cases and error recovery gracefully

**Specific Optimizations:**

- [Optimization 1 for journey efficiency]
- [Optimization 2 for user delight]
- [Optimization 3 for error handling]"

### 5. Document Journey Patterns

Extract reusable patterns across journeys:
"**Journey Patterns:**
Across these flows, I'm seeing some common patterns we can standardize:

**Navigation Patterns:**

- [Navigation pattern 1]
- [Navigation pattern 2]

**Decision Patterns:**

- [Decision pattern 1]
- [Decision pattern 2]

**Feedback Patterns:**

- [Feedback pattern 1]
- [Feedback pattern 2]

These patterns will ensure consistency across all user experiences."

### 6. Generate User Journey Content and Write to File

Generate the content and immediately append to the document:

#### Content Structure:

After generation, immediately append these Level 2 and Level 3 sections to the output file (before presenting the menu):

```markdown
## User Journey Flows

### [Journey 1 Name]

[Journey 1 description and Mermaid diagram]

### [Journey 2 Name]

[Journey 2 description and Mermaid diagram]

### Journey Patterns

[Journey patterns identified based on conversation]

### Flow Optimization Principles

[Flow optimization principles based on conversation]
```

### 7. Present Menu

Content has been written to the document. Present choices:
"I've designed detailed user journey flows for {{project_name}} and written them to the document. These flows will guide the detailed design of each user interaction.

**What would you like to do?**
[A] Advanced Elicitation - Let's refine our user journey designs
[P] Party Mode - Bring different perspectives on user flows
[C] Continue - Move to component strategy
[R] Rewrite - Rewrite this section from scratch based on feedback

### 8. Handle Menu Selection

#### If 'A' (Advanced Elicitation):

- Read fully and follow: {project-root}/_evo/core/workflows/advanced-elicitation/workflow.md with the current user journey content
- Process the enhanced journey insights that come back
- Ask user: "Accept these improvements to the user journeys? (y/n)"
- If yes: Update content with improvements and overwrite in file, then return to A/P/C/R menu
- If no: Keep original content, then return to A/P/C/R menu

#### If 'P' (Party Mode):

- Read fully and follow: {project-root}/_evo/core/workflows/party-mode/workflow.md with the current user journeys
- Process the collaborative journey insights that come back
- Ask user: "Accept these changes to the user journeys? (y/n)"
- If yes: Update content with improvements and overwrite in file, then return to A/P/C/R menu
- If no: Keep original content, then return to A/P/C/R menu

#### If 'C' (Continue):

- Update frontmatter: append step to end of stepsCompleted array
- Load `{project-root}/_evo/bmm/workflows/2-plan-workflows/create-ux-design/steps/step-11-component-strategy.md`

#### If 'R' (Rewrite):

- Rewrite the section from scratch based on user feedback, overwrite in file, then redisplay menu

## APPEND TO DOCUMENT:

After generation, immediately append the content to the document using the structure from step 6 (before presenting the menu).

## SUCCESS METRICS:

✅ Critical user journeys identified and designed
✅ Detailed flow diagrams created for each journey
✅ Flows optimized for efficiency and user delight
✅ Common journey patterns extracted and documented
✅ Content written to document immediately after generation
✅ A/P/C/R menu presented and handled correctly

## FAILURE MODES:

❌ Not identifying all critical user journeys
❌ Flows too complex or not optimized for user success
❌ Missing error recovery paths
❌ Not extracting reusable patterns across journeys
❌ Flow diagrams unclear or incomplete
❌ Not presenting A/P/C/R menu after writing content to file

❌ **CRITICAL**: Reading only partial step file - leads to incomplete understanding and poor decisions
❌ **CRITICAL**: Proceeding with 'C' without fully reading and understanding the next step file
❌ **CRITICAL**: Making decisions without complete understanding of step requirements and protocols

## NEXT STEP:

Write content to file immediately after generation. Load `{project-root}/_evo/bmm/workflows/2-plan-workflows/create-ux-design/steps/step-11-component-strategy.md` to define component library strategy.

Remember: Write content to file immediately after generation. Do NOT proceed to step-11 until user explicitly selects 'C' from the menu.
