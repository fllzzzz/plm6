<template>
  <el-form v-if="unitLoaded" ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="left" label-width="120px">
    <div class="material-info">
      <common-material-info :material="material" :form="form">
        <template #afterSpec>
          <el-form-item label="厚 * 宽 * 长">
            <span>
              {{
                `${material.thickness}${baseUnit.thickness.unit} * ${material.width}${baseUnit.width.unit} * ${material.length}${baseUnit.length.unit}`
              }}
            </span>
          </el-form-item>
        </template>
        <template #afterBrand>
          <el-form-item label="炉批号">
            <span>{{ material.heatNoAndBatchNo }}</span>
          </el-form-item>
        </template>
      </common-material-info>
    </div>
    <div class="form-info">
      <el-form-item label="出库方式" prop="materialOutboundMode">
        <common-radio v-model="form.materialOutboundMode" :options="materialOutboundModeEnum.ENUM" type="enum" size="small" />
      </el-form-item>
      <template v-if="form.materialOutboundMode === materialOutboundModeEnum.HALF.V">
        <el-form-item label="半出方式" prop="halfMode">
          <common-radio v-model="form.halfMode" :options="steelPlateHalfModeEnum.ENUM" type="enum" size="small" />
        </el-form-item>
        <el-form-item :label="`半出尺寸(${halfUnitInfo.unit})`" prop="halfSize">
          <common-input-number
            v-model="form.halfSize"
            :min="0"
            :max="maxHalfSize"
            :precision="halfUnitInfo.precision"
            controls-position="right"
          />
        </el-form-item>
      </template>
      <common-form-item :material="material" :form="form" />
    </div>
  </el-form>
</template>

<script setup>
import { steelPlateOutboundHandling } from '@/api/wms/material-outbound/raw-material/outbound-handling'
import { defineProps, defineExpose, computed, ref, watch, provide } from 'vue'
import { mapGetters } from '@/store/lib'
import { materialOutboundModeEnum, steelPlateHalfModeEnum, outboundDestinationTypeEnum } from '@/utils/enum/modules/wms'
import { deepClone, isBlank } from '@/utils/data-type'
import { numFmtByUnit } from '@/utils/wms/convert-unit'

import useWorkshopName from '@compos/store/use-workshop-name'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useWatchFormValidate from '@/composables/form/use-watch-form-validate'
import commonFormItem from '../components/common-form-item.vue'
import commonMaterialInfo from '../components/common-material-info.vue'

const { mesWorkShopName } = useWorkshopName()
const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  material: {
    // 物料出库信息
    type: Object
  }
})

const validateQuantity = (rule, value, callback) => {
  if (isBlank(value)) {
    return callback(new Error('请填写数量'))
  }
  if (value <= 0) {
    return callback(new Error('数量必须大于0'))
  }
  if (value > maxQuantity.value) {
    return callback(new Error('数量不可超过可操作数量'))
  }
  callback()
}

const validateHalfSize = (rule, value, callback) => {
  if (isBlank(value)) {
    return callback(new Error('请填写半出尺寸'))
  }
  if (value <= 0) {
    return callback(new Error('半出尺寸必须大于0'))
  }
  if (value > maxHalfSize.value) {
    return callback(new Error('半出尺寸不可超过当前物料尺寸'))
  }
  callback()
}

const rules = {
  outboundAddress: [{ required: true, message: '出库目的地', trigger: 'change' }],
  projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }],
  workshopId: [{ required: true, message: '请选择出库车间', trigger: 'change' }],
  materialOutboundMode: [{ required: true, message: '请选择物料出库方式', trigger: 'change' }],
  halfMode: [{ required: true, message: '请选择物料半出方式', trigger: 'change' }],
  halfSize: [{ required: true, validator: validateHalfSize, trigger: 'blur' }],
  quantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

const formRef = ref()
// 表单
const form = ref({
  quantity: undefined
})
// 当前用户
const { user } = mapGetters('user')
// 材料
const material = computed(() => props.material || {})

// 当前分类基础单位
const { loaded: unitLoaded, baseUnit } = useMatBaseUnit(props.basicClass)

// 监听校验
useWatchFormValidate(formRef, form, ['quantity', 'halfSize'])

// 最大半出尺寸
const maxHalfSize = computed(() => {
  if (form.value.halfMode === steelPlateHalfModeEnum.LENGTH.V) {
    return +material.value.length
  }
  if (form.value.halfMode === steelPlateHalfModeEnum.WIDTH.V) {
    return +material.value.width
  }
  return 0
})

// 最大半出尺寸
const halfUnitInfo = computed(() => {
  if (form.value.halfMode === steelPlateHalfModeEnum.LENGTH.V) {
    return baseUnit.value.length
  }
  if (form.value.halfMode === steelPlateHalfModeEnum.WIDTH.V) {
    return baseUnit.value.width
  }
  return {}
})

// 最大数量
const maxQuantity = computed(() => {
  if (!form.value || !form.value.projectId || !material.value.projectFrozenForUnitKV) return material.value.corOperableQuantity
  return material.value.corOperableQuantity + (material.value.projectFrozenForUnitKV[form.value.projectId] || 0)
})

provide('maxQuantity', maxQuantity)

watch(
  material,
  (val) => {
    formInit(val)
  },
  { immediate: true, deep: true }
)

// 表单初始化
function formInit(data) {
  const newForm = {
    materialId: data.id, // 物料id
    monomerId: data?.monomerId, // 单体id
    areaId: data?.areaId, // 区域id
    workshopId: mesWorkShopName.value?.findIndex(v => v.id === data.workshop?.id) > -1 ? data.workshop?.id : undefined, // 车间id
    outboundUnit: data.outboundUnit, // 出库单位
    outboundUnitPrecision: data.outboundUnitPrecision, // 出库单位精度
    outboundAddress: outboundDestinationTypeEnum.FACTORY.V, // 出库目的地
    projectId: data.project ? data.project.id : undefined, // 项目id
    materialOutboundMode: materialOutboundModeEnum.WHOLE.V, // 出库方式 整出/半出
    halfMode: steelPlateHalfModeEnum.LENGTH.V, // 半出方式 取长/取宽
    recipientId: user.value.id, // 领用人id
    quantity: undefined, // 数量
    remark: undefined // 备注
  }
  form.value = newForm
}

// 出库办理，表单提交
async function submit() {
  const valid = await formRef.value.validate()
  if (!valid) return false
  const formData = deepClone(form.value)
  await numFmtByUnit(formData, {
    unit: formData.outboundUnit,
    precision: formData.outboundUnitPrecision,
    fields: ['quantity'],
    toSmallest: true,
    toNum: true
  })
  const res = await steelPlateOutboundHandling(formData)
  return res
}

// 重置表单
function resetForm() {
  formRef.value && formRef.value.resetFields()
}

// 清空校验
function clearValidate() {
  formRef.value && formRef.value.clearValidate()
}

defineExpose({
  submit,
  resetForm,
  clearValidate
})
</script>

<style lang="scss" scoped>
.form {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
}
.material-info {
  flex: auto;
}
.form-info {
  margin-left: 20px;
  width: 380px;
  flex: none;
}
</style>
