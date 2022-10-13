import { ref } from 'vue'
import { artifactTypeEnum } from '@enum-ms/mes'

/**
 *
 * @param {*} queryParams {productionLineTypeEnum,areaIdList}
 */
export default function useGetArtifactTypeList({ getApi, initHook }) {
  const artifactTypeList = ref([])

  async function fetch({ productionLineTypeEnum, areaIdList }) {
    // if (!productionLineTypeEnum || !areaIdList?.length) {
    //   artifactTypeList.value = []
    //   return
    // }
    if (!areaIdList?.length) {
      artifactTypeList.value = []
      return
    }
    try {
      const { content } = await getApi({ productionLineTypeEnum, areaIdList })
      artifactTypeList.value = content.map((v) => {
        v.hasAngle = Boolean(v.artifactType === artifactTypeEnum.SMALL.V)
        return v
      })
      if (typeof initHook === 'function') initHook()
    } catch (error) {
      console.log('获取构件排产类型汇总错误', error)
    }
  }

  return {
    artifactTypeList,
    refreshArtifactType: fetch
  }
}
