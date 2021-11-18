import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取部门列表
const useRegional = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.regional)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchRegional')
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
    regional: computed(() => store.state.config.regional),
    loaded
  }
}

export default useRegional
