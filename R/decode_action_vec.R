#' Decodes Action Vector
#'
#' \code{decode_action_vec} decodes action vector.
#'
#' add here
#'
#' @param string Numeric vector.
#' @param actions Numeric vector with the number of actions.
#' @param states Numeric vector with the number of states.
#' @param inputs Numeric vector length one.
#'
#' @return Returns numeric vector.

decode_action_vec <- function(string, states, inputs, actions){
        # "states" is the number of rows;  "inputs" is the number of columns
        # "actions" (and "states") specifies how many binary elements we need to represent
        # an element of the action (or state) matrix.

        # This function takes a solution string of binary values in Gray representation,
        # transforms it to a decimal representation, then puts it in matrix form
        # with the correct sized matrices, given the specified numbers of states, inputs, and actions.

        poss.state.values <- 1:states
        b1 <- GA::decimal2binary(max(poss.state.values))
        l1 <- length(b1) #how many binary elements to represent one element of state matrix

        poss.action.values <- 1:actions
        b2 <- GA::decimal2binary(max(poss.action.values))
        l2 <- length(b2) #how many binary elements to represent one element of action matrix

        stopifnot(length(string) == (states*inputs*l1 + states*l2))
        # this is states*l2 because action.vec has length == nrows(state.matrix)

        string2vec <- function(string, len){
                string <- GA::gray2binary(string)

                vec <- rep(NA, length(string))

                # TODO: this for loop and na.omit() works but there is probably a cleaner way
                for (i in seq(from = 1, to = length(string), by = len)){
                        vec[i] <- GA::binary2decimal(string[i:(i+len-1)])}
                vec <- na.omit(vec)
                as.vector(vec)
        }

        action.string <- string[1:(states*l2)]
        action.vec <- string2vec(action.string, l2)
        # When you represent decimals in binary form with 2 binaries
        # this can represent 0, 1, 2, or 3, so change all 3s to 2s because here
        # we need only 1 and 2 for the elements of both our action and state matrices.
        # TODO: find a better solution for this issue for both the action and state matrices.
        # Jonathan suggested finding a way to test for 3s and 0s and then if it tests positive,
        # then to kill that string and create a new one. But John thinks this will slow down
        # the GA process too much because many solutions will be thrown out.
        action.vec[action.vec==3] <- 2
        action.vec[action.vec==0] <- 1
        as.integer(action.vec)
}
