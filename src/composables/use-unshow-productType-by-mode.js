import { mapGetters } from '@/store/lib'
import { projectModeEnum } from '@enum-ms/contract'
import { componentTypeEnum } from '@enum-ms/mes'
import { ref, watch, nextTick } from 'vue'

export default function useUnshowProductTypeByMode({ resetQuery }) {
  const { globalProject } = mapGetters('globalProject')

  const unshowVal = ref([])
  const showComponent = ref(false)

  watch(
    () => globalProject.value?.mode,
    () => {
      if (typeof resetQuery === 'function') {
        resetQuery()
      }
      unshowVal.value = getUnshowVal(globalProject.value.mode)
      showComponent.value = true
      nextTick(
        () => {
          showComponent.value = true
        }
      )
    },
    { deep: true, immediate: true }
  )

  function getUnshowVal(mode) {
    switch (mode) {
      case projectModeEnum.STRUCTURE.V:
        return [componentTypeEnum.ASSEMBLE.V, componentTypeEnum.MACHINE_PART.V]
      case projectModeEnum.STRUCTURE_ASSEMBLE.V:
        return [componentTypeEnum.MACHINE_PART.V]
      // case projectModeEnum.STRUCTURE_PART_ASSEMBLE.V:
      //   return []
      default:
        return []
    }
  }

  return { unshowVal, showComponent }
}
