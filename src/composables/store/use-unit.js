import { computed } from 'vue'
import { useStore } from 'vuex'
import { isBlank } from '@data-type'

// 获取字典值
const useUnit = () => {
  const store = useStore()
  // 拉取未加载的字典值
  if (isBlank(store.state.config.unit.all)) {
    store.dispatch('config/fetchUnit')
  }
  return computed(() => store.state.config.unit)
}

export default useUnit
