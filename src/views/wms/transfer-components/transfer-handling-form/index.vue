<template>
  <common-dialog
    title="调拨办理"
    v-model="dialogVisible"
    width="900px"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-transfer-handling"
    top="10vh"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" @click="submit"> 提 交 </common-button>
    </template>
    <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="left" label-width="120px">
      <div class="material-info">
        <component :is="comp" :material="props.material" />
      </div>
      <div class="form-info">
        <common-form-item :material="material" :form="form" />
      </div>
    </el-form>
  </common-dialog>
</template>

<script setup>
import {
  steelPlateTransferHandling,
  sectionSteelTransferHandling,
  steelCoilTransferHandling,
  auxMatTransferHandling,
  gasTransferHandling
} from '@/api/wms/transfer/transfer-handling'
import { defineEmits, defineProps, watch, computed, ref, nextTick } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { transferNormalTypeEnum } from '@/utils/enum/modules/wms'
import { deepClone, isBlank } from '@/utils/data-type'

import useVisible from '@compos/use-visible'
import useWatchFormValidate from '@/composables/form/use-watch-form-validate'
import commonFormItem from './components/common-form-item.vue'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'
import { numFmtByUnit } from '@/utils/wms/convert-unit'

const emit = defineEmits(['success', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  material: {
    // 物料信息
    type: Object
  }
})

const submitLoading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: clearValidate })

const validateQuantity = (rule, value, callback) => {
  if (isBlank(value)) {
    return callback(new Error('请填写数量'))
  }
  if (value <= 0) {
    return callback(new Error('数量必须大于0'))
  }
  if (value > material.value.corOperableQuantity) {
    return callback(new Error('数量不可超过可操作数量'))
  }
  callback()
}

const rules = {
  transferType: [{ required: true, message: '请选择调拨类型', trigger: 'change' }],
  projectId: [{ required: true, message: '请选择调拨项目', trigger: 'change' }],
  factoryId: [{ required: true, message: '请选择调拨工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择调拨仓库', trigger: 'change' }],
  quantity: [
    { required: true, validator: validateQuantity, trigger: 'blur' }
  ],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

// 表单ref
const formRef = ref()
// 表单
const form = ref({})
// 材料
const material = computed(() => props.material || {})
// 监听校验
useWatchFormValidate(formRef, form, ['quantity'])
// 监听物料变化，在物料发生变化时，初始化form表单
watch(
  material,
  (val) => {
    formInit(val)
  },
  { immediate: true, deep: true }
)

// 监听调拨类型
watch(
  () => form.value.transferType,
  () => {
    if (form.value.transferType !== transferNormalTypeEnum.PROJECT_WARE.V) {
      clearValidate('projectId')
    }
    if (form.value.transferType === transferNormalTypeEnum.RETURN_PARTY_A.V) {
      clearValidate('factoryId')
      clearValidate('warehouseId')
    }
  }
)

// 表单初始化
function formInit(data) {
  const newForm = {
    materialId: data.id, // 物料id
    outboundUnit: data.outboundUnit, // 出库单位
    outboundUnitPrecision: data.outboundUnitPrecision, // 出库单位精度
    transferType: transferNormalTypeEnum.PROJECT_WARE.V, // 默认项目调拨
    factoryId: data.factory ? data.factory.id : undefined, // 工厂
    warehouseId: data.warehouse ? data.warehouse.id : undefined, // 仓库
    quantity: undefined, // 数量
    remark: undefined // 备注
  }
  form.value = newForm
}

// 重置表单
function resetForm() {
  nextTick(() => {
    formRef.value && formRef.value.resetFields()
  })
}

// 清空校验
function clearValidate(field) {
  nextTick(() => {})
  formRef.value && formRef.value.clearValidate(field)
}

// 表单提交
async function submit() {
  try {
    submitLoading.value = true
    const valid = await formRef.value.validate()
    if (!valid) return false
    const submitApi = getApi(props.basicClass)
    // 数据格式转换
    const formData = deepClone(form.value)
    await numFmtByUnit(formData, {
      unit: formData.outboundUnit,
      precision: formData.outboundUnitPrecision,
      fields: ['quantity'],
      toSmallest: true,
      toNum: true
    })
    await submitApi(formData)
    emit('success')
    handleClose()
    resetForm()
  } catch (error) {
    console.log('调拨办理', error)
  } finally {
    submitLoading.value = false
  }
}

// 组件
const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoil
    case rawMatClsEnum.MATERIAL.V:
      return auxMat
    case rawMatClsEnum.GAS.V:
      return gas
    default:
      return auxMat
  }
})

// 批量出库api
function getApi(basicClass) {
  switch (basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlateTransferHandling
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteelTransferHandling
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoilTransferHandling
    case rawMatClsEnum.MATERIAL.V:
      return auxMatTransferHandling
    case rawMatClsEnum.GAS.V:
      return gasTransferHandling
    default:
      return null
  }
}
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
  width: 400px;
  flex: none;
}
</style>
