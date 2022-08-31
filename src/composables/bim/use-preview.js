import { getArtifactSearch } from '@/api/bim/model'
import { watch, ref } from 'vue'
import { ElLoading } from 'element-plus'

export default function usePreview({ props, modelStatus, initModelColor, overrideComponentsColorById, isolateComponentsById }) {
  const serialNumberElementIds = ref()

  watch(
    () => props.serialNumber,
    (val) => {
      if (props.isPreview && val && !props.previewShowAll) {
        previewOverride()
      }
    },
    { immediate: true }
  )

  async function previewOverride() {
    const loading = ElLoading.service({
      target: '#modelView',
      lock: true,
      text: '请稍后，正在染色',
      fullscreen: false
    })
    if (!props.serialNumber) return
    try {
      const _elementIds = await getArtifactSearch({
        serialNumber: props.serialNumber,
        fileId: modelStatus.value.fileId
      })
      initModelColor()
      overrideComponentsColorById(_elementIds, { color: '#1682e6', opacity: 1 })
      isolateComponentsById(_elementIds)
      serialNumberElementIds.value = _elementIds
    } catch (error) {
      console.log('构件编号预览染色错误', error)
    } finally {
      loading.close()
    }
  }

  // function serialNumberIsolate() {
  //   isolateComponentsById(serialNumberElementIds.value)
  // }

  return {
    serialNumberElementIds
  }
}
