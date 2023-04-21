<template>
  <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="left" label-width="120px">
    <div class="material-info">
      <common-material-info :material="material" :form="form" />
    </div>
    <div class="form-info">
      <common-form-item :material="material" :form="form" />
    </div>
  </el-form>
</template>

<script setup>
import { otherOutboundHandling } from '@/api/wms/material-outbound/raw-material/outbound-handling'
import { defineProps, defineExpose, provide, computed, ref, watch } from 'vue'
import { mapGetters } from '@/store/lib'
import { deepClone, isBlank } from '@/utils/data-type'

import useWatchFormValidate from '@/composables/form/use-watch-form-validate'
import commonFormItem from '../components/common-form-item.vue'
import commonMaterialInfo from '../components/common-material-info.vue'
import { numFmtByUnit } from '@/utils/wms/convert-unit'

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

const rules = ref({
  quantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
})

const formRef = ref()
// 表单
const form = ref({})
// 当前用户
const { user } = mapGetters('user')
// 材料
const material = computed(() => props.material || {})
// 监听校验
useWatchFormValidate(formRef, form, ['quantity'])
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
  { immediate: true }
)

// 表单初始化
function formInit(data) {
  const newForm = {
    materialId: data.id, // 物料id
    monomerId: data?.monomerId, // 单体id
    areaId: data?.areaId, // 区域id
    factoryId: data.factory?.id, // 车间id
    outboundUnit: data.outboundUnit, // 出库单位
    outboundUnitPrecision: data.outboundUnitPrecision, // 出库单位精度
    projectId: data.project ? data.project.id : undefined, // 项目id
    recipientId: user.value.id, // 领用人id
    quantity: undefined, // 数量
    remark: undefined // 备注
  }
  form.value = newForm

  rules.value = {
    quantity: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
    remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
  }
  if (newForm.projectId) {
    rules.value.projectId = { required: true, message: '项目库辅材必须选择项目出库', trigger: 'change' }
  }
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
  const res = await otherOutboundHandling(formData)
  return res
}

// 重置表单
function resetForm() {
  formRef.value.resetFields()
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
