(defun snord ()
  (interactive)
  (compile "KUBECONFIG=$HOME/.kube/config $HOME/workspace/kubernetes/_output/bin/e2e.test --ginkgo.focus=\"\\[sig-autoscaling\\] \\[Feature:ClusterSizeAutoscalingScaleUp\\] \\[Slow\\] Autoscaling\" --provider=aws --gce-zone=\"us-west-2\" --gce-region=\"us-west-2\" --node-instance-group=default"))
