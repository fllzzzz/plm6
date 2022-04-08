import { getArtifactSearch } from '@/api/bim/model'
import { watch } from 'vue'
import { ElLoading } from 'element-plus'

export default function usePreview({ props, initModelColor, overrideComponentsColorById, isolateComponentsById }) {
  watch(
    () => props.serialNumber,
    (val) => {
      if (props.isPreview && val) {
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
        monomerId: props.monomerId
      })
      initModelColor()
      overrideComponentsColorById(_elementIds, { color: '#1682e6', opacity: 1 })
      isolateComponentsById(_elementIds)
    } catch (error) {
      console.log('构件编号预览染色错误', error)
    } finally {
      loading.close()
    }
  }
}
