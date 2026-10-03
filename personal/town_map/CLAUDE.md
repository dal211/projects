# Project notes for Claude

## GitHub pull request descriptions

When asked for a PR / GitHub description, give both a **title** and a **description**, written so each can be pasted straight into GitHub:

- **Title:** one short line (under about 70 characters) in its own code block, with no markdown inside it, so it pastes into the title field as is.
- **Description:** markdown in a single fenced block (use a four-backtick fence if the description itself contains code fences), so it pastes into the description field as is.
- Keep the title and description in separate blocks, title first, with nothing else inside either block.
- Base both on what the branch actually changes (`git diff main...HEAD` and `git log main..HEAD`), not on the conversation alone.
- Don't open the PR unless asked.
