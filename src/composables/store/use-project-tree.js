import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取项目树
const useProjectTree = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.project.projectTreeLoaded)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('project/fetchProjectTree')
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
    projectTree: computed(() => store.state.project.projectTree),
    projectMap: computed(() => store.state.project.projectMap),
    monomerMap: computed(() => store.state.project.monomerMap),
    areaMap: computed(() => store.state.project.areaMap)
  }
}

export default useProjectTree
