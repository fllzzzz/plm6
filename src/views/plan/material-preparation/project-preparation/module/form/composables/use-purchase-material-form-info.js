import { ref, watch } from 'vue'
import { ElMessage } from 'element-plus'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { setSpecInfoForData } from '@/utils/wms/spec'
import { calcSectionSteelWeight, calcSteelPlateWeight } from '@/utils/wms/measurement-calc'
import cloneDeep from 'lodash/cloneDeep'

import useMatClsSpec from '@/composables/store/use-mat-cls-spec'

// 物料采购表单添加封装
export default function usePurchaseMaterialFormAdd({ props, basicClass }) {
  const submittable = ref(false)
  const form = ref({})
  const formRef = ref()
  const { matClsSpec, fetchMatClsSpec } = useMatClsSpec()

  watch(
    () => props.classifyIds,
    async () => {
      submittable.value = false
      await fetchMatClsSpec(props.classifyIds)
      submittable.value = true
    },
    { deep: true, immediate: true }
  )

  // 添加
  const add = async () => {
    if (!submittable.value) {
      ElMessage.info('请稍后提交，正在加载科目信息')
      return false
    }
    const valid = await formRef.value.validate()
    if (valid) {
      const res = cloneDeep(form.value)
      const materialInfo = matClsSpec.value[res.classifyId]
      // 当前规格的物料信息
      let materialSpecInfo
      // 规格
      const specification = getSpecification(props.technologyRow)
      // 获取匹配的材料规格信息
      if (materialInfo && Array.isArray(materialInfo.specList)) {
        for (const specInfo of materialInfo.specList) {
          if (specification === specInfo.spec) {
            materialSpecInfo = specInfo
            break
          }
        }
      }
      if (materialSpecInfo) {
        // 合并对象
        setSpecInfoForData(res, materialSpecInfo)
        res.specification = specification
        setTechnologyRowToResult(res, props.technologyRow)
        // 计算理论重量
        await calcTheoryWeight(res)
        if (res.outboundUnitType === measureTypeEnum.MEASURE.V) {
          res.number = res.quantity
        } else {
          res.number = res.quantity * res.theoryWeight
        }
        return res
      } else {
        ElMessage.error({ message: `该科目下不存在规格为“${specification}”的材料，请联系初鸣售后人员添加`, duration: 5000 })
        return false
      }
    }
    return false
  }

  // 计算理论重量
  const calcTheoryWeight = async (data) => {
    if (basicClass === rawMatClsEnum.STEEL_PLATE.V) {
      data.theoryWeight = await calcSteelPlateWeight({
        name: data.classifyFullName, // 名称，用于判断是否为不锈钢，不锈钢与普通钢板密度不同
        length: data.length,
        width: data.width,
        thickness: data.thickness
      })
      return
    }
    if (basicClass === rawMatClsEnum.SECTION_STEEL.V) {
      data.theoryWeight = await calcSectionSteelWeight({
        length: data.length,
        unitWeight: data.unitWeight
      })
      return
    }
  }

  // 获取规格
  const getSpecification = (technologyRow) => {
    switch (basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V:
        return technologyRow.material // 材质
      case rawMatClsEnum.SECTION_STEEL.V:
        return `${technologyRow.specification} * ${technologyRow.material}` // 规格 * 材质
      default:
        return technologyRow.material
    }
  }

  // 设置信息
  const setTechnologyRowToResult = (res, technologyRow) => {
    if (basicClass === rawMatClsEnum.STEEL_PLATE.V) {
      res.thickness = technologyRow.specification
      return
    }
  }

  // 重置表单
  const resetForm = () => {
    form.value = {}
  }

  return {
    form,
    formRef,
    add,
    resetForm
  }
}
