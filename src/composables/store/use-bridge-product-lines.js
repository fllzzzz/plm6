import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取生产线
const useBridgeProductLines = () => {
  const store = useStore()
  // 拉取未加载的生产线
  if (!store.state.config.loaded.productLines) {
    store.dispatch('config/fetchBridgeProductLines')
  }
  return {
    productLines: computed(() => store.state.config.bridgeProductLines),
    loaded: computed(() => store.state.config.loaded.bridgeProductLines)
  }
}

export default useBridgeProductLines
