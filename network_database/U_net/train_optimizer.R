model$compile(
  optimizer = tf$keras$optimizers$Adam(learning_rate = info_option$learning_rate),
  loss = custom_loss,
  metrics = custom_monitor
)