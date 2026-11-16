//! The markdown table to list transform keeps the security policy readable on
//! a narrow screen, where a rendered table would overflow sideways.

use cryptonote::markdown::tables_to_lists;

const POLICY_EN: &str = include_str!("../SECURITY.md");
const POLICY_ES: &str = include_str!("../SECURITY.es.md");
const POLICY_RU: &str = include_str!("../SECURITY.ru.md");

fn table_lines(document: &str) -> Vec<&str> {
    document
        .lines()
        .filter(|line| line.trim_start().starts_with('|'))
        .collect()
}

fn cell_count(line: &str) -> usize {
    line.trim().trim_matches('|').split('|').count()
}

#[test]
fn a_two_column_table_becomes_a_flat_list() {
    let document = "| Parameter | Value |\n| --- | --- |\n| Ciphers | AES-256-GCM |\n";
    assert_eq!(tables_to_lists(document), "- **Ciphers:** AES-256-GCM\n");
}

#[test]
fn a_wide_table_becomes_a_nested_list() {
    let document = "| Finding | Severity | Status |\n| --- | --- | --- |\n| A bug | High | Fixed |\n";
    assert_eq!(
        tables_to_lists(document),
        "- **A bug**\n  - **Severity:** High\n  - **Status:** Fixed\n"
    );
}

#[test]
fn an_escaped_pipe_stays_inside_its_cell() {
    let document = "| Asset | Exposure |\n| --- | --- |\n| Note | `a \\| b` |\n";
    assert_eq!(tables_to_lists(document), "- **Note:** `a | b`\n");
}

#[test]
fn a_table_inside_a_code_block_is_left_alone() {
    let document = "```\n| a | b |\n| --- | --- |\n```\n";
    assert_eq!(tables_to_lists(document), document);
}

#[test]
fn the_text_around_a_table_is_kept() {
    let document = "Intro\n\n| a | b |\n| --- | --- |\n| 1 | 2 |\n\nOutro\n";
    assert_eq!(tables_to_lists(document), "Intro\n\n- **1:** 2\n\nOutro\n");
}

#[test]
fn the_shipped_policies_have_no_table_left() {
    for policy in [POLICY_EN, POLICY_ES, POLICY_RU] {
        let transformed = tables_to_lists(policy);
        assert!(
            table_lines(&transformed).is_empty(),
            "the policy must not keep a table row"
        );
        assert!(transformed.contains("- **"), "the policy must keep its tables as lists");
    }
}

#[test]
fn the_shipped_policies_keep_their_sections() {
    let expectations = [
        (POLICY_EN, ["## Assets", "## Trust boundaries", "## Threat model"]),
        (
            POLICY_ES,
            ["## Activos", "## Límites de confianza", "## Modelo de amenazas"],
        ),
        (POLICY_RU, ["## Активы", "## Границы доверия", "## Модель угроз"]),
    ];

    for (policy, headings) in expectations {
        let transformed = tables_to_lists(policy);
        for heading in headings {
            assert!(
                transformed.contains(heading),
                "the policy must keep the {heading:?} section"
            );
        }
    }
}

#[test]
fn every_table_row_of_the_policies_has_the_same_number_of_cells() {
    for policy in [POLICY_EN, POLICY_ES, POLICY_RU] {
        let mut expected: Option<usize> = None;
        for line in policy.lines() {
            if !line.trim_start().starts_with('|') {
                expected = None;
                continue;
            }
            let cells = cell_count(line);
            match expected {
                None => expected = Some(cells),
                Some(width) => assert_eq!(cells, width, "the table row {line:?} must have {width} cells"),
            }
        }
    }
}

fn heading_levels(policy: &str) -> Vec<usize> {
    policy
        .lines()
        .filter_map(|line| {
            let level = line.chars().take_while(|character| *character == '#').count();
            (level > 0 && line[level..].starts_with(' ')).then_some(level)
        })
        .collect()
}

#[test]
fn the_shipped_policies_keep_the_same_structure() {
    let expected = heading_levels(POLICY_EN);

    for policy in [POLICY_ES, POLICY_RU] {
        assert_eq!(
            heading_levels(policy),
            expected,
            "the policy must keep the same headings and levels as the English one"
        );
    }
}

#[test]
fn the_policies_keep_no_closed_findings_table() {
    let forbidden = ["severity", "status", "severidad", "estado", "серьёзност", "статус"];

    for policy in [POLICY_EN, POLICY_ES, POLICY_RU] {
        for line in table_lines(policy) {
            let lowered = line.to_lowercase();
            assert!(
                !forbidden.iter().any(|column| lowered.contains(column)),
                "the policy must not keep a findings table column: {line:?}"
            );
        }
    }
}
