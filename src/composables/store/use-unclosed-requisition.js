import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取未关闭的申购单
const useUnclosedRequisition = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.unclosedRequisitions)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchUnclosedRequisitions')
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
    loaded,
    requisitions: computed(() => store.state.config.unclosedRequisitions)
  }
}

export default useUnclosedRequisition
