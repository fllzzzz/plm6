import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取字典值
const dict = (dictNames) => {
  const store = useStore()
  const dict = store.state.config.dict
  const unload = dictNames.filters(name => dict[name] === undefined)
  // 拉取未加载的字典值
  if (unload.length > 0) {
    store.dispatch('config/fetchDict', unload)
  }
  return computed(() => dict)
}

export default dict
