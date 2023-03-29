import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取车间
const useApprovalCfg = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.approvalCfg)

  // 未加载则拉取
  if (!store.state.config.loaded.approvalCfg) {
    store.dispatch('config/fetchApprovalCfg')
  }

  // 加载成功回调
  if (loadedCallBack) {
    const monitor = watch(
      loaded,
      (flag) => {
        if (flag) {
          setTimeout(() => {
            loadedCallBack()
            monitor()
          }, 0)
        }
      },
      { immediate: true }
    )
  }

  return {
    approvalCfg: computed(() => store.state.config.approvalCfg),
    loaded: computed(() => store.state.config.loaded.approvalCfg)
  }
}

export default useApprovalCfg
