import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取仓库位置
const useWarehouse = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.warehouse)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchWarehouse')
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
    warehouse: computed(() => store.state.config.warehouse),
    loaded
  }
}

export default useWarehouse
