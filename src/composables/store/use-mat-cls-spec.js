import { computed, ref } from 'vue'
import { useStore } from 'vuex'

/**
 * 获取分类规格
 * @param {array|number} classifyId 科目id
 */
const useMatClsSpec = (classifyId) => {
  const loaded = ref(true)
  const store = useStore()
  const fetchMatClsSpec = async (classifyId) => {
    const _classifyId = Array.isArray(classifyId) ? classifyId : [classifyId]
    const stateClassifySpec = store.state.config.classifySpec
    const unload = _classifyId.filter(id => stateClassifySpec[id] === undefined)
    // 拉取未加载的当前科目规格
    if (unload.length > 0) {
      loaded.value = false
      unload.forEach(id => {
        store.state.config.classifySpec[id] = {}
      })
      await store.dispatch('config/fetchMarClsSpec', unload)
      loaded.value = true
    }
  }
  if (classifyId) { // 如果传入了id则自动加载
    fetchMatClsSpec(classifyId)
  }
  return {
    loaded,
    fetchMatClsSpec,
    matClsSpecKV: computed(() => store.state.config.classifySpec.specKV),
    matClsSpec: computed(() => store.state.config.classifySpec)
  }
}

export default useMatClsSpec
