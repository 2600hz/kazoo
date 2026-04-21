# Step 4: Desired Emotional Response

## MANDATORY EXECUTION RULES (READ FIRST):

- 🛑 NEVER generate content without user input

- 📖 CRITICAL: ALWAYS read the complete step file before taking any action - partial understanding leads to incomplete decisions
- 🔄 CRITICAL: When loading next step with 'C', ensure the entire file is read and understood before proceeding
- ✅ ALWAYS treat this as collaborative discovery between UX facilitator and stakeholder
- 📋 YOU ARE A UX FACILITATOR, not a content generator
- 💬 FOCUS on defining desired emotional responses and user feelings
- 🎯 COLLABORATIVE discovery, not assumption-based design
- ✅ YOU MUST ALWAYS SPEAK OUTPUT In your Agent communication style with the config `{communication_language}`

## EXECUTION PROTOCOLS:

- 🎯 Show your analysis before taking any action
- 💾 Write content directly to output file after generation
- 📖 Update output file frontmatter, adding this step to the end of the list of stepsCompleted
- ⚠️ Present A/P/C/R menu after writing to file

## COLLABORATION MENUS (A/P/C):

This step will generate content and present choices:

- **A (Advanced Elicitation)**: Use discovery protocols to develop deeper emotional insights
- **P (Party Mode)**: Bring multiple perspectives to define optimal emotional responses
- **C (Continue)**: Save the content to the document and proceed to next step

## PROTOCOL INTEGRATION:

- When 'A' selected: Read fully and follow: {project-root}/_evo/core/workflows/advanced-elicitation/workflow.md
- When 'P' selected: Read fully and follow: {project-root}/_evo/core/workflows/party-mode/workflow.md
- PROTOCOLS always return to this step's A/P/C menu
- User accepts/rejects protocol changes before proceeding

## CONTEXT BOUNDARIES:

- Current document and frontmatter from previous steps are available
- Core experience definition from step 3 informs emotional response
- No additional data files needed for this step
- Focus on user feelings and emotional design goals

## YOUR TASK:

Define the desired emotional responses users should feel when using the product.

## EMOTIONAL RESPONSE DISCOVERY SEQUENCE:

### 1. Explore Core Emotional Goals

Start by understanding the emotional objectives:
"Now let's think about how {{project_name}} should make users feel.

**Emotional Response Questions:**

- What should users FEEL when using this product?
- What emotion would make them tell a friend about this?
- How should users feel after accomplishing their primary goal?
- What feeling differentiates this from competitors?

Common emotional goals: Empowered and in control? Delighted and surprised? Efficient and productive? Creative and inspired? Calm and focused? Connected and engaged?"

### 2. Identify Emotional Journey Mapping

Explore feelings at different stages:
"**Emotional Journey Considerations:**

- How should users feel when they first discover the product?
- What emotion during the core experience/action?
- How should they feel after completing their task?
- What if something goes wrong - what emotional response do we want?
- How should they feel when returning to use it again?"

### 3. Define Micro-Emotions

Surface subtle but important emotional states:
"**Micro-Emotions to Consider:**

- Confidence vs. Confusion
- Trust vs. Skepticism
- Excitement vs. Anxiety
- Accomplishment vs. Frustration
- Delight vs. Satisfaction
- Belonging vs. Isolation

Which of these emotional states are most critical for your product's success?"

### 4. Connect Emotions to UX Decisions

Link feelings to design implications:
"**Design Implications:**

- If we want users to feel [emotional state], what UX choices support this?
- What interactions might create negative emotions we want to avoid?
- Where can we add moments of delight or surprise?
- How do we build trust and confidence through design?

**Emotion-Design Connections:**

- [Emotion 1] → [UX design approach]
- [Emotion 2] → [UX design approach]
- [Emotion 3] → [UX design approach]"

### 5. Validate Emotional Goals

Check if emotional goals align with product vision:
"Let me make sure I understand the emotional vision for {{project_name}}:

**Primary Emotional Goal:** [Summarize main emotional response]
**Secondary Feelings:** [List supporting emotional states]
**Emotions to Avoid:** [List negative emotions to prevent]

Does this capture the emotional experience you want to create? Any adjustments needed?"

### 6. Generate Emotional Response Content

Prepare the content to append to the document:

#### Content Structure:

When saving to document, append these Level 2 and Level 3 sections:

```markdown
## Desired Emotional Response

### Primary Emotional Goals

[Primary emotional goals based on conversation]

### Emotional Journey Mapping

[Emotional journey mapping based on conversation]

### Micro-Emotions

[Micro-emotions identified based on conversation]

### Design Implications

[UX design implications for emotional responses based on conversation]

### Emotional Design Principles

[Guiding principles for emotional design based on conversation]
```

### 7. Write to File and Present Menu

After generating the emotional response content:

1. Append the content to `{planning_artifacts}/{active_feature}/ux-design-specification.md` using the structure from step 6
2. Update frontmatter: append step to end of stepsCompleted array

Then display menu:

Display: "**Select:** [A] Advanced Elicitation [P] Party Mode [C] Continue to Inspiration Analysis (Step 5) [R] Rewrite this section"

### 8. Handle Menu Selection

- IF A: Read fully and follow: {project-root}/_evo/core/workflows/advanced-elicitation/workflow.md, ask user "Accept improvements? (y/n)", if yes overwrite section in file, then redisplay menu
- IF P: Read fully and follow: {project-root}/_evo/core/workflows/party-mode/workflow.md, ask user "Accept changes? (y/n)", if yes overwrite section in file, then redisplay menu
- IF C: Load `{project-root}/_evo/bmm/workflows/2-plan-workflows/create-ux-design/steps/step-05-inspiration.md`
- IF R: Rewrite the section from scratch based on user feedback, overwrite in file, then redisplay menu
- IF Any other: help user respond, then redisplay menu

## APPEND TO DOCUMENT:

After generation, immediately append the content directly to the document using the structure from step 6 (before presenting the menu).

## SUCCESS METRICS:

✅ Primary emotional goals clearly defined
✅ Emotional journey mapped across user experience
✅ Micro-emotions identified and addressed
✅ Design implications connected to emotional responses
✅ Emotional design principles established
✅ A/P/C menu presented and handled correctly
✅ Content properly appended to document when C selected

## FAILURE MODES:

❌ Missing core emotional goals or being too generic
❌ Not considering emotional journey across different stages
❌ Overlooking micro-emotions that impact user satisfaction
❌ Not connecting emotional goals to specific UX design choices
❌ Emotional principles too vague or not actionable
❌ Not presenting A/P/C/R menu after writing content to file

❌ **CRITICAL**: Reading only partial step file - leads to incomplete understanding and poor decisions
❌ **CRITICAL**: Proceeding with 'C' without fully reading and understanding the next step file
❌ **CRITICAL**: Making decisions without complete understanding of step requirements and protocols

## NEXT STEP:

After user selects 'C' and content is saved to document, load `{project-root}/_evo/bmm/workflows/2-plan-workflows/create-ux-design/steps/step-05-inspiration.md` to analyze UX patterns from inspiring products.

Remember: Write content to file immediately after generation. Do NOT proceed to step-05 until user explicitly selects 'C' from the menu.
