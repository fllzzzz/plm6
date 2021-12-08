<template>
  <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="left" label-width="120px">
    <div class="material-info">
      <common-material-info :material="material" />
    </div>
    <div class="form-info">
      <common-form-item :material="material" :form="form" />
    </div>
  </el-form>
</template>

<script setup>
import { gasOutboundHandling } from '@/api/wms/outbound/outbound-handling'
import { defineProps, defineExpose, computed, ref, watch } from 'vue'
import { mapGetters } from '@/store/lib'

import commonFormItem from '../components/common-form-item.vue'
import commonMaterialInfo from '../components/common-material-info.vue'

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
  if (value <= 0) {
    callback(new Error('数量必须大于0'))
  }
  if (value > material.value.corOperableQuantity) {
    callback(new Error('数量不可超过可操作数量'))
    return
  }
  callback()
}

const rules = {
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { validator: validateQuantity, trigger: 'change' }
  ],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

const formRef = ref()
// 表单
const form = ref({})
// 当前用户
const { user } = mapGetters('user')
// 材料
const material = computed(() => props.material || {})

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
    outboundUnit: data.outboundUnit, // 出库单位
    outboundUnitPrecision: data.outboundUnitPrecision, // 出库单位精度
    projectId: data.project ? data.project.id : undefined, // 项目id
    recipientId: user.value.id, // 领用人id
    quantity: undefined, // 长度
    remark: undefined // 备注
  }
  form.value = newForm
}

// 出库办理，表单提交
async function submit() {
  const valid = await formRef.value.validate()
  if (!valid) return false
  const res = await gasOutboundHandling(form.value)
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
