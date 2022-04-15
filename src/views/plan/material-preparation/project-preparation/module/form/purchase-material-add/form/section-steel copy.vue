<template>
  <el-form ref="formRef" :model="form" :rules="rules" label-position="right" label-width="80px">
    <el-form-item label="科目" prop="classifyId">
      <material-cascader
        check-strictly
        v-model="form.classifyId"
        :classifyId="classifyIds"
        default
        separator=" > "
        show-all-levels
        size="mini"
        class="filter-item"
        style="width: 200px"
        placeholder="可选择/输入科目、编号搜索"
      />
    </el-form-item>
    <el-form-item label="材质" prop="material">
      {{ props.technologyRow.material }}
    </el-form-item>
    <el-form-item label="规格" prop="material">
      {{ props.technologyRow.specification }}
    </el-form-item>
    <el-form-item :label="`长(${baseUnit.length.unit})`" prop="length">
      <common-input-number
        v-model="form.length"
        :min="0"
        :max="999999"
        :precision="baseUnit.length.precision"
        :controls="false"
        text-align="left"
        size="mini"
        placeholder="长"
        style="width: 200px"
      />
    </el-form-item>
    <el-form-item label="品牌" prop="brand">
      <el-input v-model.trim="form.brand" maxlength="60" size="mini" placeholder="品牌" style="width: 200px" />
    </el-form-item>
    <el-form-item label="数量" prop="quantity">
      <common-input-number
        v-model="form.quantity"
        :min="0"
        :max="999999999"
        :step="1"
        :precision="baseUnit.measure.precision"
        :controls="false"
        text-align="left"
        size="mini"
        placeholder="数量"
        style="width: 200px"
      />
    </el-form-item>
    <el-form-item label="备注" prop="remark">
      <el-input v-model.trim="form.remark" maxlength="250" size="mini" placeholder="备注" style="width: 200px" />
    </el-form-item>
  </el-form>
</template>

<script setup>
import { defineExpose, defineProps, ref, watch } from 'vue'
import { setSpecInfoForData } from '@/utils/wms/spec'
import { calcSectionSteelWeight } from '@/utils/wms/measurement-calc'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { positiveNumPattern } from '@/utils/validate/pattern'
import cloneDeep from 'lodash/cloneDeep'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import materialCascader from '@comp-cls/material-cascader/index.vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import useMatClsSpec from '@/composables/store/use-mat-cls-spec'
import { ElMessage } from 'element-plus'

const props = defineProps({
  technologyRow: {
    type: Object,
    default: () => ({})
  },
  classifyIds: {
    type: Array,
    default: () => []
  }
})

const form = ref({})
const formRef = ref()
const { matClsSpec, fetchMatClsSpec } = useMatClsSpec()
const submittable = ref(true)

watch(
  () => props.classifyIds,
  async () => {
    submittable.value = false
    await fetchMatClsSpec(props.classifyIds)
    submittable.value = true
  },
  { deep: true, immediate: true }
)

const rules = ref({
  classifyId: [{ required: true, message: '请选择科目', trigger: 'change' }],
  length: [
    { required: true, message: '请输入钢板长度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '钢板长度必须大于0', trigger: 'blur' }
  ],
  quantity: [{ pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }]
})

// 物料基础分类单位配置
const { baseUnit } = useMatBaseUnit(rawMatClsEnum.SECTION_STEEL.V)

// 添加
async function add() {
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
    const specification = `${props.technologyRow.specification} * ${props.technologyRow.material}`
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
async function calcTheoryWeight(data) {
  data.theoryWeight = await calcSectionSteelWeight({
    length: data.length,
    unitWeight: data.unitWeight
  })
}

// default and number change
function resetForm() {
  form.value = {}
}
defineExpose({
  add,
  resetForm
})
</script>
