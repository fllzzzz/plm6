import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取生产线
const useProductLines = () => {
  const store = useStore()
  // 拉取未加载的生产线
  if (!store.state.config.loaded.productLines) {
    store.dispatch('config/fetchProductLines')
  }
  return {
    productLines: computed(() => store.state.config.productLines),
    loaded: computed(() => store.state.config.loaded.productLines)
  }
}

export default useProductLines
