import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取项目
const useUserProjects = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.project.loaded)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('project/fetchUserProjects')
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
    projects: computed(() => store.state.project.userProjects),
    processProjects: computed(() => store.state.project.userProcessProjects),
    projectsCascade: computed(() => store.state.project.userProjectsCascade),
    businessTypeProjectMap: computed(() => store.state.project.userBusinessTypeProjectMap),
    userBusinessTypeProjectsCascadeMap: computed(() => store.state.project.userBusinessTypeProjectsCascadeMap),
    userProjectsMap: computed(() => store.state.project.userProjectsMap)
  }
}

export default useUserProjects
