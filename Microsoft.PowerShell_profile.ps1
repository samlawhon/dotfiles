function global:prompt
{
    "`nPS $($executionContext.SessionState.Path.CurrentLocation)$("`n")$('>' * ($nestedPromptLevel + 1)) ";
}