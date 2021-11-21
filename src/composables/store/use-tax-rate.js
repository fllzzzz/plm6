import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取税率列表
const useTaxRate = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.taxRate)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchTaxRate')
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
    taxRateKV: computed(() => store.state.config.taxRateKV)
  }
}

export default useTaxRate
