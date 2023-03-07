<template>
  <common-dialog
    title="解冻办理"
    v-model="dialogVisible"
    width="900px"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-unfreeze-handling"
    top="10vh"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" @click="submit"> 确认解冻 </common-button>
    </template>
    <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="left" label-width="120px">
      <div class="material-info">
        <component :is="comp" :material="material" />
      </div>
      <div class="form-info">
        <common-form-item :record="record" :material="material" :form="form" />
      </div>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { unfreezeHandling } from '@/api/wms/material-freeze/raw-material/record'
import { defineEmits, defineProps, watch, computed, ref, nextTick, provide } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { deepClone, isBlank } from '@/utils/data-type'

import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByUnit } from '@/utils/wms/convert-unit'
import useVisible from '@compos/use-visible'
import useWatchFormValidate from '@/composables/form/use-watch-form-validate'
import CommonFormItem from './components/common-form-item.vue'
import SteelPlate from './module/steel-plate.vue'
import SectionSteel from './module/section-steel.vue'
import SteelCoil from './module/steel-coil.vue'
import AuxMat from './module/aux-mat.vue'
import Gas from './module/gas.vue'

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
  },
  record: {
    // 冻结记录信息
    type: Object
  }
})

const submitLoading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: clearValidate })

// 最大可设置数量
const maxQuantity = computed(() => {
  if (props.material.outboundUnitType === measureTypeEnum.MEASURE.V) {
    return props.record.quantity // 数量
  } else {
    return props.record.mete
  }
})
provide('maxQuantity', maxQuantity)

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

const rules = {
  quantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

// 表单ref
const formRef = ref()
// 表单
const form = ref({})

// 监听校验
useWatchFormValidate(formRef, form, ['quantity'])

// 监听物料变化，在物料发生变化时，初始化form表单
watch(
  () => props.record,
  (val) => {
    formInit(val)
  },
  { immediate: true, deep: true }
)

// 表单初始化
function formInit(data) {
  const newForm = {
    quantity: undefined, // 数量
    remark: undefined // 备注
  }
  if (data) {
    newForm.id = data.id
  }
  form.value = newForm
  clearValidate()
}

// 重置表单
function resetForm() {
  nextTick(() => {
    formRef.value && formRef.value.resetFields()
  })
}

// 清空校验
function clearValidate(field) {
  nextTick(() => {
    formRef.value && formRef.value.clearValidate(field)
  })
}

// 表单提交
async function submit() {
  try {
    submitLoading.value = true
    // 条板默认全部解冻
    if (props.record?.boolBatten) {
      form.value.quantity = maxQuantity.value
    }
    const valid = await formRef.value.validate()
    if (!valid) return false
    const formData = deepClone(form.value)
    await numFmtByUnit(formData, {
      unit: props.material.outboundUnit,
      precision: props.material.outboundUnitPrecision,
      fields: ['quantity'],
      toSmallest: true,
      toNum: true
    })
    await unfreezeHandling(formData)
    emit('success', formData)
    handleClose()
    resetForm()
  } catch (error) {
    console.log('解冻', error)
  } finally {
    submitLoading.value = false
  }
}

// 组件
const comp = computed(() => {
  switch (props.material.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return SteelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return SectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return SteelCoil
    case rawMatClsEnum.MATERIAL.V:
      return AuxMat
    case rawMatClsEnum.GAS.V:
      return Gas
    default:
      return AuxMat
  }
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
  width: 400px;
  flex: none;
}
</style>
