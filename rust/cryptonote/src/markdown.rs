pub use functora_core::markdown::render_markdown;

/// Rewrites every markdown table into a bullet list so the text wraps on
/// narrow viewports instead of scrolling sideways.
///
/// `egui_commonmark` lays table cells out in a horizontal grid, so a cell is
/// as wide as its content and the table overflows the screen on a phone. The
/// first column becomes the bullet title, the remaining columns become
/// labelled sub-bullets, and a two-column table becomes a flat list of
/// `**Key:** value` entries.
#[must_use]
pub fn tables_to_lists(markdown: &str) -> String {
    let lines: Vec<&str> = markdown.lines().collect();
    let code = code_mask(&lines);
    let tables = table_regions(&lines, &code);
    let body = transform_lines(&lines, &tables).join("\n");
    if markdown.ends_with('\n') {
        format!("{body}\n")
    } else {
        body
    }
}

/// Marks the lines that belong to a fenced code block, so a table-looking
/// line inside a code sample is left alone.
fn code_mask(lines: &[&str]) -> Vec<bool> {
    let mut in_code = false;
    lines
        .iter()
        .map(|line| {
            if is_fence(line) {
                in_code = !in_code;
                true
            } else {
                in_code
            }
        })
        .collect()
}

/// The `(header, end)` index pairs of every markdown table in the document.
fn table_regions(lines: &[&str], code: &[bool]) -> Vec<(usize, usize)> {
    lines
        .iter()
        .enumerate()
        .filter_map(|(index, line)| {
            if code[index] {
                return None;
            }
            let header = index.checked_sub(1)?;
            if code[header] {
                return None;
            }
            let separator = split_cells(line);
            if separator.len() < 2 || !separator.iter().all(|cell| is_separator_cell(cell)) {
                return None;
            }
            (split_cells(lines[header]).len() == separator.len()).then_some((header, index))
        })
        .map(|(header, separator)| (header, table_end(lines, code, separator)))
        .collect()
}

/// The index after the last row of the table whose separator is at `separator`.
fn table_end(lines: &[&str], code: &[bool], separator: usize) -> usize {
    lines
        .iter()
        .enumerate()
        .skip(separator + 1)
        .take_while(|(index, line)| !code[*index] && is_table_row(line))
        .last()
        .map_or(separator + 1, |(index, _)| index + 1)
}

/// Replaces every table region with its bullet list and keeps the rest of the
/// document untouched.
fn transform_lines(lines: &[&str], tables: &[(usize, usize)]) -> Vec<String> {
    let (mut output, cursor) = tables
        .iter()
        .fold((Vec::new(), 0), |(mut output, cursor), (start, end)| {
            output.extend(lines[cursor..*start].iter().map(|line| (*line).to_string()));
            output.push(table_list(lines, *start, *end));
            (output, *end)
        });
    output.extend(lines[cursor..].iter().map(|line| (*line).to_string()));
    output
}

/// The bullet list that replaces the table between `start` and `end`.
fn table_list(lines: &[&str], start: usize, end: usize) -> String {
    let header = split_cells(lines[start]);
    lines[start + 2..end]
        .iter()
        .flat_map(|row| row_bullets(&header, &split_cells(row)))
        .collect::<Vec<_>>()
        .join("\n")
}

/// The bullets that replace one table row.
fn row_bullets(header: &[String], cells: &[String]) -> Vec<String> {
    let title = cells.first().cloned().unwrap_or_default();
    match header.len() {
        0 | 1 => vec![format!("- {title}")],
        2 => {
            let value = cells.get(1).cloned().unwrap_or_default();
            vec![format!("- **{title}:** {value}").trim_end().to_string()]
        }
        _ => std::iter::once(format!("- **{title}**"))
            .chain(
                header
                    .iter()
                    .skip(1)
                    .zip(cells.iter().skip(1))
                    .map(|(name, value)| format!("  - **{name}:** {value}").trim_end().to_string()),
            )
            .collect(),
    }
}

/// Splits a table row into its cells, honouring the escaped `\|` separator.
fn split_cells(line: &str) -> Vec<String> {
    let trimmed = line.trim();
    let inner = trimmed.strip_prefix('|').unwrap_or(trimmed);
    let body = inner.strip_suffix('|').unwrap_or(inner);
    let mut cells = Vec::new();
    let mut cell = String::new();
    let mut chars = body.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '\\' if chars.peek() == Some(&'|') => {
                cell.push('|');
                let _ = chars.next();
            }
            '\\' => cell.push('\\'),
            '|' => {
                cells.push(cell.trim().to_string());
                cell = String::new();
            }
            _ => cell.push(ch),
        }
    }
    cells.push(cell.trim().to_string());
    cells
}

/// True when the cell is a table separator such as `---` or `:---:`.
fn is_separator_cell(cell: &str) -> bool {
    let trimmed = cell.trim();
    let core = trimmed.strip_prefix(':').unwrap_or(trimmed);
    let dashes = core.strip_suffix(':').unwrap_or(core);
    !dashes.is_empty() && dashes.chars().all(|ch| ch == '-')
}

/// True when the line opens or closes a fenced code block.
fn is_fence(line: &str) -> bool {
    let trimmed = line.trim_start();
    trimmed.starts_with("```") || trimmed.starts_with("~~~")
}

/// True when the line looks like a row of a markdown table.
fn is_table_row(line: &str) -> bool {
    line.trim().starts_with('|')
}
