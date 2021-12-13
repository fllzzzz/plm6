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
        <common-form-item :record="record" :form="form" />
      </div>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { unfreezeHandling } from '@/api/wms/freeze/raw-mat'
import { defineEmits, defineProps, watch, computed, ref, nextTick } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isBlank } from '@/utils/data-type'

import useVisible from '@compos/use-visible'
import commonFormItem from './components/common-form-item.vue'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'

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

const validateQuantity = (rule, value, callback) => {
  if (isBlank(value)) {
    return callback(new Error('请填写数量'))
  }
  if (value <= 0) {
    return callback(new Error('数量必须大于0'))
  }
  if (value > props.material.corOperableQuantity) {
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
    // TODO: 点击加减按钮为change,因此将两种trigger方式都包含
    { required: true, validator: validateQuantity, trigger: 'blur' },
    { validator: validateQuantity, trigger: 'change' }
  ],
  remark: [{ max: 200, message: '不能超过200个字符', trigger: 'blur' }]
}

// 表单ref
const formRef = ref()
// 表单
const form = ref({})
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
    const valid = await formRef.value.validate()
    if (!valid) return false
    await unfreezeHandling(form.value)
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
  switch (props.material.basicClass) {
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
