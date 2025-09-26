use trpl::{Either, Html};
fn main() {
    let args: Vec<String> = std::env::args().collect();
    trpl::run(async {
        let lhs = page_title(&args[1]);
        let rhs = page_title(&args[2]);
        let (url, mtitle) = match trpl::race(lhs, rhs).await {
            Either::Left(x) => x,
            Either::Right(x) => x,
        };
        println!("{url} returned first!");
        match mtitle {
            None => println!("No title for {url}"),
            Some(title) => println!("The title for {url} = {title}"),
        }
    })
}

async fn page_title(url: &str) -> (&str, Option<String>) {
    let response_text = trpl::get(url).await.text().await;
    let title = Html::parse(&response_text)
        .select_first("title")
        .map(|title| title.inner_html());
    (url, title)
}
