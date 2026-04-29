use winnow_rule::rule;

fn main() {
    let _ = rule!(
        #A ~ #B ~ #C ~ #D ~ #E ~ #F ~ #G ~ #H ~ #I ~ #J ~ #K ~ #L
    );
}
