import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取生产线
const useBridgeOnlyProductLines = () => {
  const store = useStore()
  // 拉取未加载的生产线
  if (!store.state.config.loaded.onlyProductLines) {
    store.dispatch('config/fetchBridgeOnlyProductLines')
  }
  return {
    onlyProductLines: computed(() => store.state.config.bridgeOnlyProductLines),
    loaded: computed(() => store.state.config.loaded.bridgeOnlyProductLines)
  }
}

export default useBridgeOnlyProductLines
