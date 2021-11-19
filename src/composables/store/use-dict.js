import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取字典值
const useDict = (dictNames) => {
  const store = useStore()
  const stateDict = store.state.config.dict
  const unload = dictNames.filter(name => stateDict[name] === undefined)
  // 拉取未加载的字典值
  if (unload.length > 0) {
    store.dispatch('config/fetchDict', unload)
  }
  return computed(() => store.state.config.dict)
}

export default useDict
