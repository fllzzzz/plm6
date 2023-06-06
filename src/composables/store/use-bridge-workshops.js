import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取车间
const useWorkshop = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.bridgeWorkshops)

  // 未加载则拉取
  if (!store.state.config.loaded.bridgeWorkshops) {
    store.dispatch('config/fetchBridgeWorkshops')
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
    workshops: computed(() => store.state.config.bridgeWorkshops),
    loaded: computed(() => store.state.config.loaded.bridgeWorkshops)
  }
}

export default useWorkshop
