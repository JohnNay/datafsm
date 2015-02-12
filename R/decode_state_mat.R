#' Decodes State Matrix
#'
#' \code{decode_state_mat} decodes action vector.
#'
#' add here
#'
#' @param string Numeric vector.
#' @param actions Numeric vector with the number of actions.
#' @param states Numeric vector with the number of states.
#' @param inputs Numeric vector length one.
#'
#' @return Returns numeric matrix.

decode_state_mat <- function(string, states, inputs, actions){
        poss.state.values <- 1:states
        b1 <- GA::decimal2binary(max(poss.state.values))
        l1 <- length(b1) #how many binary elements to represent one element of state matrix

        poss.action.values <- 1:actions
        b2 <- GA::decimal2binary(max(poss.action.values))
        l2 <- length(b2) #how many binary elements to represent one element of action matrix

        stopifnot(length(string) == (states*inputs*l1 + states*l2))
        # this is states*l2 because action.vec has length == nrows(state.matrix)

        string2matrix <- function(string, len){
                string <- GA::gray2binary(string)

                vec <- rep(NA, length(string))

                # TODO: this for loop and na.omit() works but there is probably a cleaner way
                for (i in seq(from = 1, to = length(string), by = len)){
                        vec[i] <- GA::binary2decimal(string[i:(i+len-1)])}
                vec <- na.omit(vec)
                vec <- as.integer(vec)

                matrix(vec,
                       nrow = states, ncol = inputs, byrow = FALSE)
        }

        state.string <- string[(states*l2+1):length(string)]

        state.matrix <- string2matrix(state.string, l1)
        if (states==2){
                state.matrix[state.matrix==3] <- 2
        }
        state.matrix[state.matrix==0] <- 1 # do this whether states is 3 or 2
        state.matrix
}
