# Step 2: Project Understanding

## MANDATORY EXECUTION RULES (READ FIRST):

- 🛑 NEVER generate content without user input

- 📖 CRITICAL: ALWAYS read the complete step file before taking any action - partial understanding leads to incomplete decisions
- 🔄 CRITICAL: When loading next step with 'C', ensure the entire file is read and understood before proceeding
- ✅ ALWAYS treat this as collaborative discovery between UX facilitator and stakeholder
- 📋 YOU ARE A UX FACILITATOR, not a content generator
- 💬 FOCUS on understanding project context and user needs
- 🎯 COLLABORATIVE discovery, not assumption-based design
- ✅ YOU MUST ALWAYS SPEAK OUTPUT In your Agent communication style with the config `{communication_language}`

## EXECUTION PROTOCOLS:

- 🎯 Show your analysis before taking any action
- 💾 Write content directly to output file after generation
- 📖 Update output file frontmatter, adding this step to the end of the list of stepsCompleted
- ⚠️ Present A/P/C/R menu after writing to file

## COLLABORATION MENUS (A/P/C):

This step will generate content and present choices:

- **A (Advanced Elicitation)**: Use discovery protocols to develop deeper project insights
- **P (Party Mode)**: Bring multiple perspectives to understand project context
- **C (Continue)**: Save the content to the document and proceed to next step

## PROTOCOL INTEGRATION:

- When 'A' selected: Read fully and follow: {project-root}/_evo/core/workflows/advanced-elicitation/workflow.md
- When 'P' selected: Read fully and follow: {project-root}/_evo/core/workflows/party-mode/workflow.md
- PROTOCOLS always return to this step's A/P/C menu
- User accepts/rejects protocol changes before proceeding

## CONTEXT BOUNDARIES:

- Current document and frontmatter from step 1 are available
- Input documents (PRD, briefs, epics) already loaded are in memory
- No additional data files needed for this step
- Focus on project and user understanding

## YOUR TASK:

Understand the project context, target users, and what makes this product special from a UX perspective.

## PROJECT DISCOVERY SEQUENCE:

### 1. Review Loaded Context

Start by analyzing what we know from the loaded documents:
"Based on the project documentation we have loaded, let me confirm what I'm understanding about {{project_name}}.

**From the documents:**
{summary of key insights from loaded PRD, briefs, and other context documents}

**Target Users:**
{summary of user information from loaded documents}

**Key Features/Goals:**
{summary of main features and goals from loaded documents}

Does this match your understanding? Are there any corrections or additions you'd like to make?"

### 2. Fill Context Gaps (If no documents or gaps exist)

If no documents were loaded or key information is missing:
"Since we don't have complete documentation, let's start with the essentials:

**What are you building?** (Describe your product in 1-2 sentences)

**Who is this for?** (Describe your ideal user or target audience)

**What makes this special or different?** (What's the unique value proposition?)

**What's the main thing users will do with this?** (Core user action or goal)"

### 3. Explore User Context Deeper

Dive into user understanding:
"Let me understand your users better to inform the UX design:

**User Context Questions:**

- What problem are users trying to solve?
- What frustrates them with current solutions?
- What would make them say 'this is exactly what I needed'?
- How tech-savvy are your target users?
- What devices will they use most?
- When/where will they use this product?"

### 4. Identify UX Design Challenges

Surface the key UX challenges to address:
"From what we've discussed, I'm seeing some key UX design considerations:

**Design Challenges:**

- [Identify 2-3 key UX challenges based on project type and user needs]
- [Note any platform-specific considerations]
- [Highlight any complex user flows or interactions]

**Design Opportunities:**

- [Identify 2-3 areas where great UX could create competitive advantage]
- [Note any opportunities for innovative UX patterns]

Does this capture the key UX considerations we need to address?"

### 5. Generate Project Understanding Content

Prepare the content to append to the document:

#### Content Structure:

When saving to document, append these Level 2 and Level 3 sections:

```markdown
## Executive Summary

### Project Vision

[Project vision summary based on conversation]

### Target Users

[Target user descriptions based on conversation]

### Key Design Challenges

[Key UX challenges identified based on conversation]

### Design Opportunities

[Design opportunities identified based on conversation]
```

### 6. Write to File and Present Menu

After generating the project understanding content:

1. Append the content to `{planning_artifacts}/{active_feature}/ux-design-specification.md` using the structure from step 5
2. Update frontmatter: append this step to end of stepsCompleted array

Then display menu:

Display: "**Select:** [A] Advanced Elicitation [P] Party Mode [C] Continue to Core Experience (Step 3) [R] Rewrite this section"

### 7. Handle Menu Selection

- IF A: Read fully and follow: {project-root}/_evo/core/workflows/advanced-elicitation/workflow.md, process results, ask user "Accept improvements? (y/n)", if yes overwrite section in file, then redisplay menu
- IF P: Read fully and follow: {project-root}/_evo/core/workflows/party-mode/workflow.md, process results, ask user "Accept changes? (y/n)", if yes overwrite section in file, then redisplay menu
- IF C: Read fully and follow: `{project-root}/_evo/bmm/workflows/2-plan-workflows/create-ux-design/steps/step-03-core-experience.md`
- IF R: Rewrite the section from scratch based on user feedback, overwrite in file, then redisplay menu
- IF Any other: help user respond, then redisplay menu

## APPEND TO DOCUMENT:

After generation, immediately append the content directly to the document using the structure from step 5 (before presenting the menu).

## SUCCESS METRICS:

✅ All available context documents reviewed and synthesized
✅ Project vision clearly articulated
✅ Target users well understood
✅ Key UX challenges identified
✅ Design opportunities surfaced
✅ A/P/C menu presented and handled correctly
✅ Content properly appended to document when C selected

## FAILURE MODES:

❌ Not reviewing loaded context documents thoroughly
❌ Making assumptions about users without asking
❌ Missing key UX challenges that will impact design
❌ Not identifying design opportunities
❌ Generating generic content without real project insight
❌ Not presenting A/P/C/R menu after writing content to file

❌ **CRITICAL**: Reading only partial step file - leads to incomplete understanding and poor decisions
❌ **CRITICAL**: Proceeding with 'C' without fully reading and understanding the next step file
❌ **CRITICAL**: Making decisions without complete understanding of step requirements and protocols

## NEXT STEP:

Remember: Write content to file immediately after generation. Do NOT proceed to step-03 until user explicitly selects 'C' from the menu.
