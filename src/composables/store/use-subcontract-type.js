import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取分包类别
const useSubcontractType = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.subcontractType)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchSubcontractType')
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
    subcontractType: computed(() => store.state.config.subcontractType),
    loaded
  }
}

export default useSubcontractType
