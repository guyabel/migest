net_matrix_nn <- function(net_tot, m, zero_mask = NULL,
                          net_weight = 1, prior_weight = 0.1, volume_weight = 0.01,
                          activation = "softplus", clip_threshold = 1e-4,
                          epochs = 500, lr = 0.01, hidden = 64, verbose = TRUE) {
  # Ensure torch is available
  if (!requireNamespace("torch", quietly = TRUE)) stop("Package 'torch' is required.")
  library(torch)

  n <- length(net_tot)
  stopifnot(identical(dim(m), c(n, n)))

  # Tensors
  net_tot_t <- torch_tensor(net_tot, dtype = torch_float())
  prior_t   <- torch_tensor(m, dtype = torch_float())

  # Handle zero_mask
  if (is.null(zero_mask)) zero_mask <- matrix(FALSE, n, n)
  zero_mask_t <- torch_tensor(zero_mask, dtype = torch_bool())

  # Model
  model <- nn_module(
    "FlowNet",
    initialize = function() {
      self$fc1 <- nn_linear(n, hidden)
      self$fc2 <- nn_linear(hidden, n * n)
    },
    forward = function(x) {
      out <- self$fc1(x)
      out <- nnf_relu(out)  # non-inplace
      out <- self$fc2(out)
      out <- switch(activation,
                    softplus = nnf_softplus(out),
                    relu     = nnf_relu(out),
                    stop("Unsupported activation"))
      out <- out$view(c(n, n))
      out <- out$masked_fill(zero_mask_t, 0)  # non-inplace
      out
    }
  )

  net_model <- model()
  optimizer <- torch::optim_adam(net_model$parameters, lr = lr)

  volume_target <- sum(m)
  volume_target_t <- torch_tensor(c(volume_target), dtype = torch_float())  # shape [1]

  # Training loop
  for (epoch in 1:epochs) {
    optimizer$zero_grad()

    flow_pred <- net_model(net_tot_t)

    # Net flow loss: shape [n] vs [n]
    net_pred <- flow_pred$sum(1) - flow_pred$sum(2)
    net_loss <- nnf_mse_loss(net_pred$view(c(n)), net_tot_t$view(c(n)))

    # Prior loss: compare only non-masked entries
    prior_loss <- nnf_mse_loss(
      flow_pred$masked_select(!zero_mask_t),
      prior_t$masked_select(!zero_mask_t)
    )

    # Volume loss: both shape [1]
    volume_pred <- flow_pred$sum()$unsqueeze(1)
    volume_loss <- nnf_mse_loss(volume_pred, volume_target_t)

    # Weighted total loss
    loss <- net_weight * net_loss + prior_weight * prior_loss + volume_weight * volume_loss
    loss$backward()
    optimizer$step()

    if (verbose && epoch %% 50 == 0) {
      cat(sprintf("Epoch %d | Loss: %.6f\n", epoch, loss$item()))
    }
  }

  # Final output
  flow_final <- as.matrix(net_model(net_tot_t)$detach())
  flow_final[flow_final < clip_threshold] <- 0
  dimnames(flow_final) <- dimnames(m)

  list(
    n = flow_final,
    it = epochs,
    value = sum((flow_final[!zero_mask] - m[!zero_mask])^2),
    convergence = TRUE,
    message = "Neural net trained with weighted loss and clipping"
  )
}

m <- matrix(c(0, 100, 30, 70,
              50,   0, 45,  5,
              60,  35,  0, 40,
              20,  25, 20,  0),
            nrow = 4, byrow = TRUE,
            dimnames = list(orig = LETTERS[1:4], dest = LETTERS[1:4]))
addmargins(m)
library(migest)
sum_region(m)

zero_mask <- diag(4) == 1
net <- c(30, 40, -15, -55)

result <- net_matrix_nn(
  net_tot = net,
  m = m,
  zero_mask = zero_mask,
  net_weight = 1,
  prior_weight = 0.1,
  volume_weight = 0.1,
  activation = "softplus",
  clip_threshold = 1e-5, epochs = 1000
)

round(addmargins(result$n), 1)
round(colSums(result$n) - rowSums(result$n), 2)  # check net totals
net


result <- net_matrix_nn(
  net_tot = net,
  m = m,
  zero_mask = zero_mask,
  net_weight = 1,
  prior_weight = 0.01,
  volume_weight = 0.1,
  activation = "softplus",
  clip_threshold = 1e-5, epochs = 1000
)
