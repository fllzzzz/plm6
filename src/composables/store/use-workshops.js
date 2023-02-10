import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取车间
const useWorkshop = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.workshops)

  // 未加载则拉取
  if (!store.state.config.loaded.workshops) {
    store.dispatch('config/fetchWorkshops')
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
    workshopKV: computed(() => store.state.config.workshopKV),
    workshops: computed(() => store.state.config.workshops),
    loaded: computed(() => store.state.config.loaded.workshops)
  }
}

export default useWorkshop
