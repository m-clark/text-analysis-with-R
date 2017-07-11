# finds and cuts out everything up to and including the first instance of act, scene, or by

detect_first_act <- function(text_tbl) {
  act_lines = grep(text_tbl$text, pattern='^Act|^ACT|^by|^By|^Sonnet|^W\\. S\\.|^WILLIAM SHAKESPEARE.')
  if(length(act_lines) > 0) {
    idx = 1:act_lines[1]
    text_tbl = text_tbl %>% slice(-idx)
  }
  text_tbl
}
