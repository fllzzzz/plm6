import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取供应商
const useSuppliers = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.suppliers)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchSuppliers')
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
    suppliers: computed(() => store.state.config.suppliers),
    supplierKV: computed(() => store.state.config.supplierKV)
  }
}

export default useSuppliers
