use clap::Subcommand;

#[derive(Subcommand, Default)]
pub enum ChangelogCommand {
    #[default]
    Bump,
    Init,
}
