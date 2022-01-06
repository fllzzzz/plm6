<template>
  <common-dialog
    append-to-body
    title="任务删除"
    :close-on-click-modal="false"
    :before-close="handleClose"
    :visible="dialogVisible"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" :disabled="!form.quantity" @click="submitIt">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-form-item label="编号">
        <span v-empty-text>{{ details.serialNumber }}</span>
      </el-form-item>
      <el-form-item label="所属项目">
        <span v-empty-text>{{ details.project?.name }}</span>
      </el-form-item>
      <el-form-item label="任务数">
        <span>{{ details.sourceSchedulingQuantity }}</span>
      </el-form-item>
      <el-form-item label="未生产数">
        <span>{{ unInProductionQuantity }}</span>
      </el-form-item>
      <el-form-item label="输入数量" prop="quantity">
        <el-input-number
          v-model="form.quantity"
          :step="1"
          :min="0"
          :max="unInProductionQuantity"
          size="small"
          controls-position="right"
          style="width: 250px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { delTask } from '@/api/mes/scheduling-manage/task/common'
import { defineProps, ref, defineEmits, computed, reactive, watch } from 'vue'
import { ElNotification } from 'element-plus'

import useVisible from '@compos/use-visible'

const rules = {
  quantity: [{ required: true, message: '请填写数量', trigger: 'blur' }]
}
const formRef = ref()
const emit = defineEmits(['delSuccess', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  details: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const submitLoading = ref(false)
const form = reactive({ quantity: null })

const unInProductionQuantity = computed(() => {
  return (props.details?.sourceSchedulingQuantity || 0) - (props.details?.inProductionQuantity || 0)
})

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      form.quantity = null
    }
  },
  { immediate: true }
)

function submitIt() {
  formRef.value.validate(async (valid) => {
    if (valid) {
      const data = {
        id: props.details.id,
        quantity: form?.quantity
      }
      await delTask(data)
      ElNotification({ title: '任务删除成功', type: 'success', duration: 2500 })
      emit('delSuccess')
      handleClose()
    } else {
      return false
    }
  })
}
</script>
