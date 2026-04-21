# Step 3: Core Experience Definition

## MANDATORY EXECUTION RULES (READ FIRST):

- 🛑 NEVER generate content without user input

- 📖 CRITICAL: ALWAYS read the complete step file before taking any action - partial understanding leads to incomplete decisions
- 🔄 CRITICAL: When loading next step with 'C', ensure the entire file is read and understood before proceeding
- ✅ ALWAYS treat this as collaborative discovery between UX facilitator and stakeholder
- 📋 YOU ARE A UX FACILITATOR, not a content generator
- 💬 FOCUS on defining the core user experience and platform
- 🎯 COLLABORATIVE discovery, not assumption-based design
- ✅ YOU MUST ALWAYS SPEAK OUTPUT In your Agent communication style with the config `{communication_language}`

## EXECUTION PROTOCOLS:

- 🎯 Show your analysis before taking any action
- 💾 Write content directly to output file after generation
- 📖 Update output file frontmatter, adding this step to the end of the list of stepsCompleted
- ⚠️ Present A/P/C/R menu after writing to file

## COLLABORATION MENUS (A/P/C):

This step will generate content and present choices:

- **A (Advanced Elicitation)**: Use discovery protocols to develop deeper experience insights
- **P (Party Mode)**: Bring multiple perspectives to define optimal user experience
- **C (Continue)**: Save the content to the document and proceed to next step

## PROTOCOL INTEGRATION:

- When 'A' selected: Read fully and follow: {project-root}/_evo/core/workflows/advanced-elicitation/workflow.md
- When 'P' selected: Read fully and follow: {project-root}/_evo/core/workflows/party-mode/workflow.md
- PROTOCOLS always return to this step's A/P/C menu
- User accepts/rejects protocol changes before proceeding

## CONTEXT BOUNDARIES:

- Current document and frontmatter from previous steps are available
- Project understanding from step 2 informs this step
- No additional data files needed for this step
- Focus on core experience and platform decisions

## YOUR TASK:

Define the core user experience, platform requirements, and what makes the interaction effortless.

## CORE EXPERIENCE DISCOVERY SEQUENCE:

### 1. Define Core User Action

Start by identifying the most important user interaction:
"Now let's dig into the heart of the user experience for {{project_name}}.

**Core Experience Questions:**

- What's the ONE thing users will do most frequently?
- What user action is absolutely critical to get right?
- What should be completely effortless for users?
- If we nail one interaction, everything else follows - what is it?

Think about the core loop or primary action that defines your product's value."

### 2. Explore Platform Requirements

Determine where and how users will interact:
"Let's define the platform context for {{project_name}}:

**Platform Questions:**

- Web, mobile app, desktop, or multiple platforms?
- Will this be primarily touch-based or mouse/keyboard?
- Any specific platform requirements or constraints?
- Do we need to consider offline functionality?
- Any device-specific capabilities we should leverage?"

### 3. Identify Effortless Interactions

Surface what should feel magical or completely seamless:
"**Effortless Experience Design:**

- What user actions should feel completely natural and require zero thought?
- Where do users currently struggle with similar products?
- What interaction, if made effortless, would create delight?
- What should happen automatically without user intervention?
- Where can we eliminate steps that competitors require?"

### 4. Define Critical Success Moments

Identify the moments that determine success or failure:
"**Critical Success Moments:**

- What's the moment where users realize 'this is better'?
- When does the user feel successful or accomplished?
- What interaction, if failed, would ruin the experience?
- What are the make-or-break user flows?
- Where does first-time user success happen?"

### 5. Synthesize Experience Principles

Extract guiding principles from the conversation:
"Based on our discussion, I'm hearing these core experience principles for {{project_name}}:

**Experience Principles:**

- [Principle 1 based on core action focus]
- [Principle 2 based on effortless interactions]
- [Principle 3 based on platform considerations]
- [Principle 4 based on critical success moments]

These principles will guide all our UX decisions. Do these capture what's most important?"

### 6. Generate Core Experience Content

Prepare the content to append to the document:

#### Content Structure:

When saving to document, append these Level 2 and Level 3 sections:

```markdown
## Core User Experience

### Defining Experience

[Core experience definition based on conversation]

### Platform Strategy

[Platform requirements and decisions based on conversation]

### Effortless Interactions

[Effortless interaction areas identified based on conversation]

### Critical Success Moments

[Critical success moments defined based on conversation]

### Experience Principles

[Guiding principles for UX decisions based on conversation]
```

### 7. Write to File and Present Menu

After generating the core experience content:

1. Append the content to `{planning_artifacts}/{active_feature}/ux-design-specification.md` using the structure from step 6
2. Update frontmatter: append step to end of stepsCompleted array

Then display menu:

Display: "**Select:** [A] Advanced Elicitation [P] Party Mode [C] Continue to Emotional Response (Step 4) [R] Rewrite this section"

### 8. Handle Menu Selection

- IF A: Read fully and follow: {project-root}/_evo/core/workflows/advanced-elicitation/workflow.md, ask user "Accept improvements? (y/n)", if yes overwrite section in file, then redisplay menu
- IF P: Read fully and follow: {project-root}/_evo/core/workflows/party-mode/workflow.md, ask user "Accept changes? (y/n)", if yes overwrite section in file, then redisplay menu
- IF C: Load `{project-root}/_evo/bmm/workflows/2-plan-workflows/create-ux-design/steps/step-04-emotional-response.md`
- IF R: Rewrite the section from scratch based on user feedback, overwrite in file, then redisplay menu
- IF Any other: help user respond, then redisplay menu

## APPEND TO DOCUMENT:

After generation, immediately append the content directly to the document using the structure from step 6 (before presenting the menu).

## SUCCESS METRICS:

✅ Core user action clearly identified and defined
✅ Platform requirements thoroughly explored
✅ Effortless interaction areas identified
✅ Critical success moments mapped out
✅ Experience principles established as guiding framework
✅ A/P/C menu presented and handled correctly
✅ Content properly appended to document when C selected

## FAILURE MODES:

❌ Missing the core user action that defines the product
❌ Not properly considering platform requirements
❌ Overlooking what should be effortless for users
❌ Not identifying critical make-or-break interactions
❌ Experience principles too generic or not actionable
❌ Not presenting A/P/C/R menu after writing content to file

❌ **CRITICAL**: Reading only partial step file - leads to incomplete understanding and poor decisions
❌ **CRITICAL**: Proceeding with 'C' without fully reading and understanding the next step file
❌ **CRITICAL**: Making decisions without complete understanding of step requirements and protocols

## NEXT STEP:

After user selects 'C' and content is saved to document, load `{project-root}/_evo/bmm/workflows/2-plan-workflows/create-ux-design/steps/step-04-emotional-response.md` to define desired emotional responses.

Remember: Write content to file immediately after generation. Do NOT proceed to step-04 until user explicitly selects 'C' from the menu.
