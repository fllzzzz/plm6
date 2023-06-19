import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取仓库所属车间
const useWorkshopName = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.workshopName)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchWorkshopName')
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
    workshopName: computed(() => store.state.config.workshopName),
    loaded
  }
}

export default useWorkshopName
